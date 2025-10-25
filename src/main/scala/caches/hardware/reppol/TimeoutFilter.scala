package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import chisel3._
import chisel3.util._

class TimeoutFilterIO(nWays: Int, nSets: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat) extends Bundle {
  val setIdx = Input(UInt(log2Up(nSets).W))
  val update = Input(Valid(UInt(log2Up(nWays).W)))
  val updateCoreId = Input(UInt(log2Up(nCores).W))
  val baseCandidates = repSetFormat match {
    case NumericalFormat() => Input(Vec(nWays, UInt(log2Up(nWays).W)))
    case MruFormat() => Input(Vec(nWays, UInt(1.W)))
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }
  val coreTimeouts = Input(Vec(nCores, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val decIdxTimers = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val updateIdxTimers = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val isRepValid = Output(Bool())
  val replaceWay = Output(UInt(log2Up(nWays).W))
  val wTimers = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  val wIdx = Output(UInt(log2Up(nSets).W))
  val decIdx = Output(UInt(log2Up(nSets).W))
}

class TimeoutFilter(nWays: Int, nSets: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat) extends Module {
  val io = IO(new TimeoutFilterIO(nWays, nSets, nCores, repSetFormat))

  def mruFilter(baseCandidates: Vec[UInt], timeoutMask: Vec[Bool]): UInt = {
    val anyTimeoutInFirstSet = (~baseCandidates.asUInt).asUInt & timeoutMask.asUInt
    val anyTimeoutInSecondSet = baseCandidates.asUInt & timeoutMask.asUInt
    val baseCandMask = WireDefault(0.U(nWays.W))

    when(anyTimeoutInFirstSet.asUInt.orR) {
      baseCandMask := anyTimeoutInFirstSet
    } .otherwise {
      baseCandMask := anyTimeoutInSecondSet
    }

    PriorityEncoder(baseCandMask)
  }

  def numericalFilter(baseCandidates: Vec[UInt], timeoutMask: Vec[Bool]): UInt = {
    val baseCandMask = VecInit(Seq.fill(nWays)(true.B))
    for (i <- 0 until nWays) {
      val wayIdx = baseCandidates(i)
      val timedOut = timeoutMask(wayIdx)

      baseCandMask(i) := timedOut
    }

    baseCandidates(PriorityEncoder(baseCandMask))
  }

  def isCritical(coreIdx: UInt, coreTmts: Vec[UInt]): Bool = coreTmts(coreIdx) =/= 0.U

  val isRepValid = WireDefault(false.B)
  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val freeRejQueue = WireDefault(false.B)

  val decIdx = RegInit(0.U(log2Up(nSets).W))  // A counter to manage which next set needs its timers decremented

  val timedOutWays = VecInit(Seq.fill(nWays)(true.B))
  for (wayIdx <- 0 until nWays) {
    timedOutWays(wayIdx) := io.updateIdxTimers(wayIdx) === 0.U
  }

  val firstTimedOutWay = repSetFormat match {
    case NumericalFormat() => numericalFilter(io.baseCandidates, timedOutWays)
    case MruFormat() => mruFilter(io.baseCandidates, timedOutWays)
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }

  val anyTimedOutWays = timedOutWays.reduce((x, y) => x || y)
  val refreshWayEnable = io.update.valid

  // We decrement the current set's timers
  val wayTimesDecremented = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  for (i <- 0 until nWays) {
    when(io.decIdxTimers(i) =/= 0.U) {
      wayTimesDecremented(i) := io.decIdxTimers(i) - 1.U
    }
  }

  isRepValid := anyTimedOutWays || isCritical(io.updateCoreId, io.coreTimeouts)
  when(!anyTimedOutWays && isCritical(io.updateCoreId, io.coreTimeouts)) {
    replaceWay := io.baseCandidates(0)
  }.otherwise {
    replaceWay := firstTimedOutWay
  }

  //------------- Timers update -----------------
  val refreshWayIdx = io.update.bits
  val refreshCoreId = io.updateCoreId
  val refreshTime = io.coreTimeouts(refreshCoreId)
  val wTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val wIdx = WireDefault(0.U(log2Up(nSets).W))
  val rDecIdx = WireDefault(0.U(log2Up(nSets).W))

  when(!refreshWayEnable) {
    wTimers := wayTimesDecremented
    decIdx := decIdx + 1.U
    wIdx := decIdx
    rDecIdx := decIdx + 1.U
  }.elsewhen(refreshWayEnable && (decIdx === io.setIdx)) { // refresh a set while its being decremented
    val wayTimesFinal = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for (i <- 0 until nWays) {
      when(i.U === refreshWayIdx && (refreshTime > wayTimesDecremented(i))) {
        wayTimesFinal(i) := refreshTime
      }.otherwise {
        wayTimesFinal(i) := wayTimesDecremented(i)
      }
    }

    wTimers := wayTimesFinal
    decIdx := decIdx + 1.U
    wIdx := decIdx
    rDecIdx := decIdx + 1.U
  }.otherwise { // When a set is being refreshed, and It's different from decrement index, we simply do not decrement the index
    val updatedWays = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for (i <- 0 until nWays) {
      when(i.U === refreshWayIdx && (refreshTime > io.updateIdxTimers(i))) {
        updatedWays(i) := refreshTime
      }.otherwise {
        updatedWays(i) := io.updateIdxTimers(i)
      }
    }

    wTimers := updatedWays
    wIdx := io.setIdx
    rDecIdx := decIdx
  }

  io.isRepValid := isRepValid
  io.replaceWay := replaceWay
  io.wTimers := wTimers
  io.wIdx := wIdx
  io.decIdx := rDecIdx
}
