package caches.hardware.reppol

import caches.hardware.util._
import chisel3._
import chisel3.util._

class BitPlruReadStage(nWays: Int, nSets: Int, repSetFormat: BaseReplacementSetFormat) extends BasePolicyReadStageType(nWays, nSets, nWays, repSetFormat) {
  val readMruBits = mruBitsMem(rIdx = io.rIdx, wrEn = io.wrEn, wIdx = io.wIdx, wData = io.wData, stall = io.stall)

  val inState = WireDefault(0.U(nWays.W))
  when(io.fwd) {
    inState := io.wData
  }.otherwise {
    inState := readMruBits
  }

  val repSet = repSetFormat match {
    case NumericalFormat() => getNumericalReplacementSet(inState)
    case MruFormat() => getMruReplacementSet(inState)
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }

  def mruBitsMem(rIdx: UInt, wrEn: Bool, wIdx: UInt, wData: UInt, stall: Bool): UInt = {
    val mruBits = Module(new MemBlock(nSets, nWays))

    mruBits.io.readAddr := rIdx
    mruBits.io.writeAddr := wIdx
    mruBits.io.writeData := wData
    mruBits.io.wrEn := wrEn
    mruBits.io.stall := stall

    mruBits.io.readData
  }

  override def getNumericalReplacementSet(state: UInt): Vec[UInt] = {
    val mruBits = VecInit(state.asBools)
    var wayIdxBits = log2Up(nWays)

    if (wayIdxBits % 2 != 0) {
      wayIdxBits += 1
    }

    val lruOrderedSet = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
    val zeros = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
    val ones = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))

    val onesCount = VecInit(Seq.fill(mruBits.length + 1)(0.U(log2Up(nWays).W)))
    val zerosCount = VecInit(Seq.fill(mruBits.length + 1)(0.U(log2Up(nWays).W)))

    for (i <- 0 until mruBits.length) {
      val updateOnesCount = WireDefault(false.B)
      val updateZerosCount = WireDefault(false.B)

      when(mruBits(i) === false.B) {
        zeros(zerosCount(i)) := i.U
        updateZerosCount := true.B
      }.otherwise {
        ones(onesCount(i)) := i.U
        updateOnesCount := true.B
      }

      zerosCount(i + 1) := Mux(updateZerosCount, zerosCount(i) + 1.U, zerosCount(i))
      onesCount(i + 1) := Mux(updateOnesCount, onesCount(i) + 1.U, onesCount(i))
    }

    val shiftedOnes = ones.asUInt << (zerosCount(mruBits.length) << (log2Up(nWays) - 1).asUInt).asUInt
    val orderedIdxs = zeros.asUInt + shiftedOnes.asUInt
    for (i <- 0 until mruBits.length) {
      val temp = orderedIdxs((i * wayIdxBits) + (wayIdxBits - 1), i * wayIdxBits)
      lruOrderedSet(i) := temp
    }
    lruOrderedSet
  }

  override def getMruReplacementSet(state: UInt): Vec[UInt] = {
    val mruRepSet = VecInit(Seq.fill(nWays)(0.U(1.W)))

    for (i <- 0 until mruRepSet.length) {
      mruRepSet(i) := state(i).asUInt
    }

    mruRepSet
  }

  override def getRepWay(state: UInt): UInt = {
    // Find the first 0 bit by inverting the bit registers and passing them to the priority encoder
    PriorityEncoder((~state).asUInt)
  }

  io.readState := inState
  io.replaceWay := getRepWay(inState)
  io.replacementSet := repSet
}
