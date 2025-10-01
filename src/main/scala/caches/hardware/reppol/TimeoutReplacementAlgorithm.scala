package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import chisel3._
import chisel3.util._

class TimeoutReplacementAlgorithm(nWays: Int, nSets: Int, nCores: Int) extends Module {
  val io = IO(new Bundle {
//    val timers = Input(Vec(nSets, UInt((nWays * TIMEOUT_LIMIT_WIDTH).W)))
//    val coreTimeouts = Input(Vec(nSets, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val setIdx = Input(UInt(log2Up(nSets).W))
    val evict = Input(Bool())
    val update = Input(Valid(UInt(log2Up(nWays).W)))
    val updateCoreId = Input(UInt(log2Up(nCores).W))
    val baseCandidates = Input(Vec(nWays, UInt(log2Up(nWays).W)))
    val scheduler = new SchedulerControlIO(nCores, TIMEOUT_LIMIT_WIDTH)
    val isRepValid = Output(Bool())
    val replaceWay = Output(UInt(log2Up(nWays).W))
    val freeRejQueue = Output(Bool())
  })

  val isRepValid = WireDefault(false.B)
  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val freeRejQueue = WireDefault(false.B)

  val timers = RegInit(VecInit(Seq.fill(nSets)(0.U((nWays * TIMEOUT_LIMIT_WIDTH).W))))
  val coreTimeouts = RegInit(VecInit(Seq.fill(nCores)(0.U(TIMEOUT_LIMIT_WIDTH.W))))

  def isCritical(coreIdx: UInt, coreTmts: Vec[UInt]): Bool = coreTmts(coreIdx) =/= 0.U

  // A counter manages which next set needs its timers decremented
  val decIdx = RegInit(0.U(log2Up(nSets).W))
  decIdx := decIdx + 1.U // TODO: Do not increment if we are updating

  // Connect scheduler
  when(io.scheduler.cmd === SchedulerCmd.WR) {
    coreTimeouts(io.scheduler.addr) := io.scheduler.wData
  }.elsewhen(io.scheduler.cmd === SchedulerCmd.RD) {
    coreTimeouts(io.scheduler.addr) := 0.U
    freeRejQueue := true.B
  }

  // We decrement the current set's timers
  val wayTimes = VecInit(Seq.tabulate(nWays) { i =>
    timers(decIdx)((TIMEOUT_LIMIT_WIDTH * (i + 1)) - 1, TIMEOUT_LIMIT_WIDTH * i)
  })

  // Calculate replacement order
  val wayTimesDecremented = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  for (i <- 0 until nWays) {
    when(wayTimes(i) =/= 0.U) {
      wayTimesDecremented(i) := wayTimes(i) - 1.U
    }
  }

  // TODO: Add a multiplexer before a memory read address, that selects if we are reading current set timers or decrement timers
  val currentSetWayTimes = VecInit(Seq.tabulate(nWays) { i =>
    timers(io.setIdx)((TIMEOUT_LIMIT_WIDTH * (i + 1)) - 1, TIMEOUT_LIMIT_WIDTH * i)
  })

  val timedOutWays = VecInit(Seq.fill(nWays)(true.B))
  for (i <- 0 until nWays) {
    val wayIdx = io.baseCandidates(i)
    val timedOut = currentSetWayTimes(wayIdx) === 0.U

    timedOutWays(i) := timedOut
  }

  val firstTimedOutBaseCandIdx = PriorityEncoder(timedOutWays)

  val firstTimedOutWay = io.baseCandidates(firstTimedOutBaseCandIdx)
  val anyTimedOutWays = timedOutWays.reduce((x, y) => x || y)
  val refreshWayEnable = io.update.valid

  isRepValid := anyTimedOutWays || isCritical(io.updateCoreId, coreTimeouts) // Can be switched out with update core id once more pipeline registers are added
  when(!anyTimedOutWays && isCritical(io.updateCoreId, coreTimeouts)) {
    replaceWay := io.baseCandidates(0)
  }.otherwise {
    replaceWay := firstTimedOutWay
  }

  //------------- Timers update -----------------
  val refreshWayIdx = io.update.bits
  val refreshSetIdx = io.setIdx
  val refreshCoreId = io.updateCoreId
  val refreshEvict = io.evict
  val refreshTime = coreTimeouts(refreshCoreId)

  when(!refreshWayEnable) {
    timers(decIdx) := Cat(wayTimesDecremented.reverse)
  }.elsewhen(refreshWayEnable && (decIdx === refreshSetIdx)) { // refresh a set while its being decremented
    val wayTimesFinal = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for (i <- 0 until nWays) {
      when(i.U === refreshWayIdx && (refreshTime > wayTimesDecremented(i))) {
        wayTimesFinal(i) := refreshTime
      }.otherwise {
        wayTimesFinal(i) := wayTimesDecremented(i)
      }
    }
    timers(decIdx) := Cat(wayTimesFinal.reverse)
  }.otherwise { // Accessing a set that is not being refreshed
    timers(decIdx) := Cat(wayTimesDecremented.reverse)

    val updatedWays = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for (i <- 0 until nWays) {
      when(i.U === refreshWayIdx && (refreshTime > currentSetWayTimes(i))) {
        updatedWays(i) := refreshTime
      }.otherwise {
        updatedWays(i) := currentSetWayTimes(i)
      }
    }
    timers(refreshSetIdx) := Cat(updatedWays.reverse)
  }

  io.isRepValid := isRepValid
  io.replaceWay := replaceWay
  io.freeRejQueue := freeRejQueue

  io.scheduler.rData := DontCare
}
