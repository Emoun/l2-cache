package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import caches.hardware.util.{PipelineReg, TwoReadMemBlock}
import chisel3._
import chisel3.util._

class CoreTimeoutTable(nCores: Int) extends Module {
  val io = IO(new Bundle {
    val scheduler = new SchedulerControlIO(nCores, TIMEOUT_LIMIT_WIDTH)
    val rCoreTimeouts = Output(Vec(nCores, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  })

  val coreTimeouts = RegInit(VecInit(Seq.fill(nCores)(0.U(TIMEOUT_LIMIT_WIDTH.W))))
  val schedulerRData = WireDefault(0.U(TIMEOUT_LIMIT_WIDTH.W))

  // Connection to scheduler
  when(io.scheduler.cmd === SchedulerCmd.WR) {
    coreTimeouts(io.scheduler.addr) := io.scheduler.wData
  }.elsewhen(io.scheduler.cmd === SchedulerCmd.RD) {
    schedulerRData := coreTimeouts(io.scheduler.addr)
    coreTimeouts(io.scheduler.addr) := 0.U
  }

  io.scheduler.rData := schedulerRData
  io.rCoreTimeouts := coreTimeouts
}

class TimerMemory(nWays: Int, nSets: Int) extends Module {
  val io = IO(new Bundle{
    val stall = Input(Bool())
    val rIdx1 = Input(UInt(log2Up(nSets).W))
    val rIdx2 = Input(UInt(log2Up(nSets).W))
    val wrEn = Input(Bool())
    val wIdx = Input(UInt(log2Up(nSets).W))
    val wData = Input(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
//    val wMask = Input(Vec(nWays, Bool()))
    val rTimers1 = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
    val rTimers2 = Output(Vec(nWays, UInt(TIMEOUT_LIMIT_WIDTH.W)))
  })

  val timers = Array.fill(nWays)(Module(new TwoReadMemBlock(nSets, TIMEOUT_LIMIT_WIDTH, stallReg1 = false)))

  val rTimers1 = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val rTimers2 = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  for (wayIdx <- 0 until nWays) {
//    val wrWayEn = io.wMask(wayIdx)

    timers(wayIdx).io.wrEn := io.wrEn //&& wrWayEn
    timers(wayIdx).io.readAddr1 := io.rIdx1
    timers(wayIdx).io.readAddr2 := io.rIdx2
    timers(wayIdx).io.writeAddr := io.wIdx
    timers(wayIdx).io.writeData := io.wData(wayIdx)
    timers(wayIdx).io.stall := io.stall

    rTimers1(wayIdx) := timers(wayIdx).io.readData1
    rTimers2(wayIdx) := timers(wayIdx).io.readData2
  }

  io.rTimers1 := rTimers1
  io.rTimers2 := rTimers2
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
  val decIdx = WireDefault(0.U(log2Up(nSets).W))
  val timerMemWrEn = WireDefault(false.B)
  val timerMemWrData = VecInit(Seq.fill(nWays)(0.U(TIMEOUT_LIMIT_WIDTH.W)))
  val timerMemWrIdx = WireDefault(0.U(log2Up(nSets).W))

  val timerMemory = Module(new TimerMemory(nWays, nSets))
  timerMemory.io.stall := io.control.stall
  timerMemory.io.rIdx1 := decIdx
  timerMemory.io.rIdx2 := setIdxDelayReg
  timerMemory.io.wrEn := timerMemWrEn
  timerMemory.io.wIdx := timerMemWrIdx
  timerMemory.io.wData := timerMemWrData

  val coreTimeoutTable = Module(new CoreTimeoutTable(nCores))
  coreTimeoutTable.io.scheduler <> io.scheduler

  val timeoutAlgo = Module(new TimeoutReplacementAlgorithm(nWays = nWays, nSets = nSets, nCores = nCores))
  timeoutAlgo.io.setIdx := setIdxPipeReg
  timeoutAlgo.io.update := io.control.update
  timeoutAlgo.io.updateCoreId := io.control.updateCoreId
  timeoutAlgo.io.baseCandidates := basePolicyInst.io.control.replacementSet
  timeoutAlgo.io.coreTimeouts := coreTimeoutTable.io.rCoreTimeouts
  timeoutAlgo.io.decIdxTimers := timerMemory.io.rTimers1
  timeoutAlgo.io.updateIdxTimers := timerMemory.io.rTimers2
  decIdx := timeoutAlgo.io.decIdx
  timerMemWrEn := true.B // TODO: ???
  timerMemWrData := timeoutAlgo.io.wTimers
  timerMemWrIdx := timeoutAlgo.io.wIdx

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)

  io.control.replaceWay := timeoutAlgo.io.replaceWay
  io.control.isValid := timeoutAlgo.io.isRepValid
}
