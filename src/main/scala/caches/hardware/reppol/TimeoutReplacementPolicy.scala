package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import caches.hardware.util.{MemBlock, PipelineReg}
import chisel3._
import chisel3.util._

class TimerMemory(nWays: Int, nSets: Int) extends Module {
  val io = IO(new Bundle{
    val stall = Input(Bool())
    val rIdx = Input(UInt(log2Up(nSets).W))
    val wrEn = Input(Bool())
    val wIdx = Input(UInt(log2Up(nSets).W))
    val wData = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val wMask = Input(Vec(nWays, Bool()))
    val rTimers = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  })

  val timers = Array.fill(nWays)(Module(new MemBlock(nSets, TIMEOUT_LIMIT_WIDTH)))

  val rTimers = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  for (wayIdx <- 0 until nWays) {
    val wrWayEn = io.wMask(wayIdx)

    timers(wayIdx).io.wrEn := io.wrEn && wrWayEn
    timers(wayIdx).io.readAddr := io.rIdx
    timers(wayIdx).io.writeAddr := io.wIdx
    timers(wayIdx).io.writeData := io.wData(wayIdx)
    timers(wayIdx).io.stall := io.stall

    rTimers(wayIdx) := timers(wayIdx).io.readData
  }

  io.rTimers := rTimers
}

/**
 * Timeout replacement policy. When critical cores access a line, a counter is initiated for that line.
 * Any non-critical core can only evict lines whose timer has reached zero, i.e. lines that are owned by non-critical
 * cores or critical core lines that have timed out.
 *
 * @param nWays      number of ways in a cache set
 * @param nSets      number of sets in a cache
 * @param nCores     number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class TimeoutReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, basePolicy: () => SharedCacheReplacementPolicyType) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, TIMEOUT_LIMIT_WIDTH) {
  //--------------- Base Policy ---------------------
  val basePolicyInst = Module(basePolicy())

  override def printConfig(): Unit = println(s"Timeout replacement policy configuration: Base policy: ${basePolicyInst.getClass.getSimpleName}, ways: $nWays, sets: $nSets, cores: $nCores" + "\n")

  // Default assignments to base policy
  basePolicyInst.io.control <> 0.U.asTypeOf(basePolicyInst.io.control)
  basePolicyInst.io.scheduler <> 0.U.asTypeOf(basePolicyInst.io.scheduler)

  // Update base policy
  basePolicyInst.io.control.setIdx := io.control.setIdx
  basePolicyInst.io.control.update.valid := io.control.update.valid
  basePolicyInst.io.control.update.bits := io.control.update.bits
  basePolicyInst.io.control.stall := io.control.stall

  // Need to delay this signal by two CCs because PLRU has 2 stages
  val setIdxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall) // Delay once for accessing base policy mem
  val setIdxPipeReg = PipelineReg(setIdxDelayReg, 0.U, !io.control.stall) // Delay twice for computing replacement set

  //-------------------------------------
  val timeoutAlgo = Module(new TimeoutReplacementAlgorithm(nWays = nWays, nSets = nSets, nCores = nCores))
  timeoutAlgo.io.setIdx := setIdxPipeReg
  timeoutAlgo.io.evict := io.control.evict
  timeoutAlgo.io.update := io.control.update
  timeoutAlgo.io.updateCoreId := io.control.updateCoreId
  timeoutAlgo.io.baseCandidates := basePolicyInst.io.control.replacementSet
  timeoutAlgo.io.scheduler <> io.scheduler

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)

  io.control.replaceWay := timeoutAlgo.io.replaceWay
  io.control.isValid := timeoutAlgo.io.isRepValid
  io.control.isReplacementWayCrit := false.B // TODO: Not sure if relevant
  io.control.isReplacementWayAtLimit := false.B // TODO: Not sure if relevant
  io.control.updateCoreReachedLimit := false.B // TODO: Not sure if relevant
}
