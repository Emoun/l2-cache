package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

class TimeoutReplacementPolicy (ways: Int, sets: Int, nCores: Int, basePolicy: () => SharedCacheReplacementPolicyType) extends SharedCacheReplacementPolicyType(ways, sets, nCores, TIMEOUT_LIMIT_WIDTH) {

  // Base policy instantiation
  val basePolicyInst = Module(basePolicy())

  // Update base policy
  basePolicyInst.io.control.isHit := io.control.isHit
  basePolicyInst.io.control.setIdx := io.control.setIdx
  basePolicyInst.io.control.coreId := io.control.coreId
  basePolicyInst.io.control.evict := io.control.evict
  basePolicyInst.io.control.update.valid := io.control.update.valid
  basePolicyInst.io.control.update.bits := io.control.update.bits
  basePolicyInst.io.control.updateCoreId := io.control.update.bits
  basePolicyInst.io.control.stall := io.control.stall
  basePolicyInst.io.control.missActive := io.control.missActive
  basePolicyInst.io.scheduler <> io.scheduler

  // Need to delay this signal by two CCs because PLRU has 2 stages
  val setIdxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
  val setIdxPipeReg = PipelineReg(setIdxDelayReg, 0.U, !io.control.stall)

  val timers = RegInit(VecInit(Seq.fill(sets)(0.U((ways * TIMEOUT_LIMIT_WIDTH).W))))
  val coreTimeouts = RegInit(VecInit(Seq.fill(nCores)(0.U(TIMEOUT_LIMIT_WIDTH.W))))

  def isCritical(coreIdx: UInt) = coreTimeouts(coreIdx) =/= 0.U

  // Connect scheduler
  when(io.scheduler.cmd === SchedulerCmd.WR) {
    coreTimeouts(io.scheduler.addr) := io.scheduler.wData
  }.elsewhen(io.scheduler.cmd === SchedulerCmd.RD) {
    coreTimeouts(io.scheduler.addr) := 0.U
  }

  // A counter manages which next set needs its timers decremented
  val decIdx = RegInit(0.U(log2Up(sets).W))
  decIdx := decIdx + 1.U

  // We decrement the current set's timers
  val wayTimes = VecInit(Seq.tabulate(ways) { i =>
    timers(decIdx)((TIMEOUT_LIMIT_WIDTH * (i + 1)) - 1, TIMEOUT_LIMIT_WIDTH * i)
  })
  val wayTimesDecremented = VecInit(Seq.fill(ways)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  for(i <- 0 until ways) {
    when(wayTimes(i) =/= 0.U) {
      wayTimesDecremented(i) := wayTimes(i) - 1.U
    }
  }

  // Calculate replacement order
  val baseCandidates = basePolicyInst.io.control.replacementSet
  val currentSetWayTimes = VecInit(Seq.tabulate(ways) { i =>
    timers(setIdxPipeReg)((TIMEOUT_LIMIT_WIDTH * (i + 1)) - 1, TIMEOUT_LIMIT_WIDTH * i)
  })

  val timedoutWays = VecInit(Seq.fill(ways)(true.B))
  for (i <- 0 until ways) {
    val wayIdx = baseCandidates(i)
    val timedOut = currentSetWayTimes(wayIdx) === 0.U

    timedoutWays(i) := timedOut
  }
  val firstTimedoutBaseCandIdx = PriorityEncoder(timedoutWays)
  val firstTimedoutWay = baseCandidates(firstTimedoutBaseCandIdx)

  val anyTimedOutWays = timedoutWays.reduce((x, y) => x || y)

  io.control.isValid := anyTimedOutWays || isCritical(io.control.updateCoreId) // Can be switched out with update core id once more pipeline registers are added
  when(!anyTimedOutWays && isCritical(io.control.updateCoreId)) {
    io.control.replaceWay := baseCandidates(0)
  }.otherwise {
    io.control.replaceWay := firstTimedoutWay
  }

  io.control.replacementSet := VecInit(Seq.fill(ways)(0.U(log2Up(ways).W)))

  //-------------------------------------

  val refreshWayEnable = io.control.update.valid
  val refreshWayIdx = io.control.update.bits
  val refreshSetIdx = setIdxPipeReg
  val refreshCoreId = io.control.updateCoreId
  val refreshEvict = io.control.evict
  val refreshTime = coreTimeouts(refreshCoreId)

  when(!refreshWayEnable) {
    timers(decIdx) := Cat(wayTimesDecremented.reverse)
  }.elsewhen(refreshWayEnable && (decIdx === refreshSetIdx)) { // refresh a set while its being decremented
    val wayTimesFinal = VecInit(Seq.fill(ways)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for(i <- 0 until ways) {
      when(i.U === refreshWayIdx && (refreshTime > wayTimesDecremented(i))) {
        wayTimesFinal(i) := refreshTime
      }.otherwise{
        wayTimesFinal(i) := wayTimesDecremented(i)
      }
    }
    timers(decIdx) := Cat(wayTimesFinal.reverse)
  }.otherwise {  // Accessing a set that is not being refreshed
    timers(decIdx) := Cat(wayTimesDecremented.reverse)

    val updatedWays = VecInit(Seq.fill(ways)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
    for(i <- 0 until ways) {
      when(i.U === refreshWayIdx && (refreshTime > currentSetWayTimes(i))) {
        updatedWays(i) := refreshTime
      }.otherwise{
        updatedWays(i) := currentSetWayTimes(i)
      }
    }
    timers(refreshSetIdx) := Cat(updatedWays.reverse)
  }

  io.control.popRejQueue.valid := false.B
  io.control.popRejQueue.bits := 0.U
}
