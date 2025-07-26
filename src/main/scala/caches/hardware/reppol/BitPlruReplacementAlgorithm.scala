package caches.hardware.reppol

import chisel3._
import chisel3.util._
import chisel3.util.PriorityEncoder

/**
 * Bit based implementation of Pseudo LRU
 *
 * @param nWays number of ways in a set-associate cache
 */
class BitPlruReplacementAlgorithm(nWays: Int) extends Module() {
  val io = IO(new Bundle {
    val hitWay = Input(UInt(log2Up(nWays).W))
    val computeMruBits = Input(Vec(nWays, Bool()))
    val updateMruBits = Input(Vec(nWays, Bool()))
    val updatedMru = Output(Vec(nWays, Bool()))
    val replaceWay = Output(UInt(log2Up(nWays).W))
    val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W)))
  })

  /**
   * Updates the selected MRU register and resets at capacity
   *
   * @param way the way that has been accessed
   */
  def updateMruBits(way: UInt, currentMruBits: Vec[Bool]): Vec[Bool] = {
    val newMruBits = VecInit(Seq.fill(nWays)(false.B))

    // Check for capacity
    val capacity = ((~currentMruBits.asUInt).asUInt & ((~currentMruBits.asUInt).asUInt - 1.U)) === 0.U

    for (bitIdx <- 0 until nWays) {
      when(capacity && (bitIdx.U =/= way).asBool) { // When at capacity, reset all bits except the accessed way bit
        newMruBits(bitIdx) := false.B
      } .otherwise{
        newMruBits(bitIdx) := currentMruBits(bitIdx)
      }
    }

    newMruBits(way) := true.B // Update the accessed way

    newMruBits
  }

  /**
   * Generates logic for computing an ordered LRU set from a given MRU bits array
   *
   * @param mruBits set of mru bits
   * @return
   */
  def getLruOrderedSet(mruBits: Vec[Bool]): Vec[UInt] = {
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
      } .otherwise {
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

  /**
   * Finds the index of the LRU way in the set
   *
   * @return the index of the LRU way in the set
   */
  def getLru(mruBits: Vec[Bool]): UInt = {
    // Find the first 0 bit by inverting the bit registers and passing them to the priority encoder
    PriorityEncoder((~mruBits.asUInt).asUInt)
  }

  val updatedMruBits = updateMruBits(io.hitWay, io.updateMruBits)

  // Replacement selection logic
  val replaceWay = getLru(io.computeMruBits)
  val replaceSet = getLruOrderedSet(io.computeMruBits)

  io.updatedMru := updatedMruBits
  io.replaceWay := replaceWay
  io.replacementSet := replaceSet
}
