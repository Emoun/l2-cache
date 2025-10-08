package caches.hardware.reppol

import chisel3._
import chisel3.util._
import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import caches.hardware.util.{MemBlock, PipelineReg}

class LineAssignmentsArray(nWays: Int, nSets: Int, nCores: Int) extends Module() {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val rSet = Input(UInt(log2Up(nSets).W))
    val wrEn = Input(Bool())
    val wrSet = Input(UInt(log2Up(nSets).W))
    val wrWay = Input(UInt(log2Up(nWays).W))
    val wrLineAssign = Input(UInt(log2Up(nCores).W))
    val rLineAssign = Output(Vec(nWays, UInt(log2Up(nCores).W)))
    val rValidAssign = Output(Vec(nWays, Bool()))
  })

  // Memory array for keeping track of line assignments
  val lineAssignments = Array.fill(nWays)(Module(new MemBlock(nSets, log2Up(nCores))))
  val validLineAssignments = Array.fill(nWays)(Module(new MemBlock(nSets, 1)))

  val rLineAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))
  val rValidLineAssignments = VecInit(Seq.fill(nWays)(false.B))

  for (wayIdx <- 0 until nWays) {
    val wrWayEn = io.wrWay === wayIdx.U

    lineAssignments(wayIdx).io.wrEn := io.wrEn && wrWayEn
    lineAssignments(wayIdx).io.readAddr := io.rSet
    lineAssignments(wayIdx).io.writeAddr := io.wrSet
    lineAssignments(wayIdx).io.writeData := io.wrLineAssign
    lineAssignments(wayIdx).io.stall := io.stall

    validLineAssignments(wayIdx).io.wrEn := io.wrEn && wrWayEn
    validLineAssignments(wayIdx).io.readAddr := io.rSet
    validLineAssignments(wayIdx).io.writeAddr := io.wrSet
    validLineAssignments(wayIdx).io.writeData := true.B
    validLineAssignments(wayIdx).io.stall := io.stall

    rLineAssignments(wayIdx) := lineAssignments(wayIdx).io.readData
    rValidLineAssignments(wayIdx) := validLineAssignments(wayIdx).io.readData
  }

  io.rLineAssign := rLineAssignments
  io.rValidAssign := rValidLineAssignments
}

class CoreContentionTable(nCores: Int) extends Module() {
  val io = IO(new Bundle {
    val schedCoreId = Input(UInt(log2Up(nCores).W))
    val setCritical = Input(Bool())
    val unsetCritical = Input(Bool())
    val setContLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
    val wrCoreLimits = Input(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val wrEn = Input(Bool())
    val readData = Output(UInt(CONTENTION_LIMIT_WIDTH.W)) // Returns the current core limit if it is unset as critical to the scheduler
    val rLimits = Output(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val rCritCores = Output(Vec(nCores, Bool()))
    val freeRejectionQueue = Output(Bool())
  })

  // NOTE: There is no forwarding here, if a core is set or unset as critical,
  // the current cache request will not see it until the next cycle.

  // Registers for keeping the state of each active core
  val contentionLimits = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))))
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

  // Set and unset cores as critical
  for (coreTableIdx <- 0 until nCores) {
    // Store updated contention limits
    when(io.wrEn) {
      contentionLimits(coreTableIdx) := io.wrCoreLimits(coreTableIdx)
    }

    // Scheduler assignments
    when(io.setCritical && io.schedCoreId === coreTableIdx.U) {
      criticalCores(coreTableIdx) := true.B
      contentionLimits(coreTableIdx) := io.setContLimit
    }.elsewhen(io.unsetCritical && io.schedCoreId === coreTableIdx.U) {
      criticalCores(coreTableIdx) := false.B
      contentionLimits(coreTableIdx) := 0.U
    }
  }

  // When a core is unset we empty the rejection queue
  val freeRejQueue = WireDefault(false.B)
  when(io.unsetCritical) {
    freeRejQueue := criticalCores(io.schedCoreId) // Free the rejection queue if the request was critical
  }

  val readData = WireDefault(0.U(CONTENTION_LIMIT_WIDTH.W))
  when(io.unsetCritical) {
    readData := contentionLimits(io.schedCoreId)
  }

  io.rLimits := contentionLimits
  io.rCritCores := criticalCores
  io.freeRejectionQueue := freeRejQueue
  io.readData := readData
}

/**
 * On a hit we update only the base policy, on an eviction we update the contention policy.
 *
 * @param nWays      number of ways in a cache set
 * @param nSets      number of sets in a cache
 * @param nCores     number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class ContentionReplacementPolicy(
                                   nWays: Int,
                                   nSets: Int,
                                   nCores: Int,
                                   basePolicy: () => SharedCacheReplacementPolicyType,
                                   missQueueDepth: Int = 4,
                                   enableMissInMiss: Boolean = false,
                                   enablePrecedentEvents: Boolean = false,
                                   enableWbEvents: Boolean = false
                                 ) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, CONTENTION_LIMIT_WIDTH, missQueueDepth) {
  // ---------------- Base policy stage ----------------

  // Base policy instantiation
  val basePolicyInst = Module(basePolicy())

  override def printConfig(): Unit = println(s"Contention replacement policy configuration: Base policy: ${basePolicyInst.getClass.getSimpleName}, ways: $nWays, sets: $nSets, cores: $nCores, mim events: $enableMissInMiss, precedent events: $enablePrecedentEvents, wb events: $enableWbEvents" + "\n")

  // Default assignments to base policy
  basePolicyInst.io.control <> 0.U.asTypeOf(basePolicyInst.io.control)
  basePolicyInst.io.scheduler <> 0.U.asTypeOf(basePolicyInst.io.scheduler)

  // Update base policy
  basePolicyInst.io.control.setIdx := io.control.setIdx
  basePolicyInst.io.control.update.valid := io.control.update.valid
  basePolicyInst.io.control.update.bits := io.control.update.bits
  basePolicyInst.io.control.stall := io.control.stall

  // Need to delay this signal by two CCs since the bit plru uses memory to store the MRU bits
  val setIdxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
  val setIdxPipeReg = PipelineReg(setIdxDelayReg, 0.U, !io.control.stall)

  // ---------------- Eviction stage ----------------
  val contAlgorithm = Module(new ContentionReplacementAlgorithm(nWays, nCores, nMshrs = missQueueDepth, enableMissInMiss, enablePrecedentEvents, enableWbEvents))

  val assignArr = Module(new LineAssignmentsArray(nWays, nSets, nCores))
  assignArr.io.stall := io.control.stall
  assignArr.io.rSet := setIdxDelayReg
  assignArr.io.wrEn := io.control.evict
  assignArr.io.wrSet := setIdxPipeReg
  assignArr.io.wrWay := contAlgorithm.io.replacementWay.bits
  assignArr.io.wrLineAssign := io.control.updateCoreId

  // ---------------------------------------------------

  val coreTable = Module(new CoreContentionTable(nCores))
  coreTable.io.schedCoreId := io.scheduler.addr
  coreTable.io.setCritical := io.scheduler.cmd === SchedulerCmd.WR
  coreTable.io.unsetCritical := io.scheduler.cmd === SchedulerCmd.RD
  coreTable.io.wrEn := contAlgorithm.io.updateCoreLimits
  coreTable.io.wrCoreLimits := contAlgorithm.io.newCoreLimits
  coreTable.io.setContLimit := io.scheduler.wData
  io.scheduler.rData := coreTable.io.readData

  // Compute the eviction for each set
  contAlgorithm.io.evict := io.control.evict
  contAlgorithm.io.update := io.control.update.valid
  contAlgorithm.io.hit := io.control.isHit
  contAlgorithm.io.hitWayIdx := io.control.update.bits
  contAlgorithm.io.reqCore := io.control.updateCoreId
  contAlgorithm.io.baseCandidates := basePolicyInst.io.control.replacementSet
  contAlgorithm.io.lineAssignments := assignArr.io.rLineAssign
  contAlgorithm.io.validLineAssignments := assignArr.io.rValidAssign
  contAlgorithm.io.coreLimits := coreTable.io.rLimits
  contAlgorithm.io.criticalCores := coreTable.io.rCritCores
  contAlgorithm.io.missQueueEmpty := io.control.missQueueEmpty
  contAlgorithm.io.missQueueCores := io.control.missQueueCores
  contAlgorithm.io.missQueueValidCores := io.control.missQueueValidCores
  contAlgorithm.io.missQueueCritCores := io.control.missQueueValidCores
  contAlgorithm.io.wbNonCritPop := io.control.nonCritWbPop
  contAlgorithm.io.wbPopEntryCrit := io.control.nonCritWbEntryIsCrit

  io.control.replaceWay := contAlgorithm.io.replacementWay.bits
  io.control.isValid := contAlgorithm.io.replacementWay.valid
  io.control.isReplacementWayCrit := contAlgorithm.io.isReplacementWayCrit
  io.control.isReplacementWayAtLimit := contAlgorithm.io.isReplacementWayAtLimit
  io.control.replacementSet := VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W)))
  io.control.updateCoreReachedLimit := coreTable.io.rCritCores(io.control.updateCoreId) && (coreTable.io.rLimits(io.control.updateCoreId) === 0.U)
  io.control.updateCoreIsCrit := coreTable.io.rCritCores(io.control.updateCoreId)
}