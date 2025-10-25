package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import caches.hardware.util.{MemBlock, PipelineReg}
import chisel3._
import chisel3.util._

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
 */
class ContentionReplacementPolicy(
                                   nWays: Int,
                                   nSets: Int,
                                   nCores: Int,
                                   basePolicyType: BasePolicyType,
                                   enableMissInMiss: Boolean = false,
                                   enablePrecedentEvents: Boolean = false,
                                   enableWbEvents: Boolean = false,
                                   missQueueDepth: Int = 4,
                                   repSetFormat: BaseReplacementSetFormat = new NumericalFormat,
                                 ) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, CONTENTION_LIMIT_WIDTH, missQueueDepth, repSetFormat) {
  // ---------------- Read Stage ----------------
  override def printConfig(): Unit = println(s"Contention replacement policy configuration: " +
    s"base policy type: ${basePolicyType.getName}, " +
    s"replacement set format: ${repSetFormat.getName}, " +
    s"ways: $nWays, " +
    s"sets: $nSets, " +
    s"cores: $nCores, " +
    s"mim events: $enableMissInMiss, " +
    s"precedent events: $enablePrecedentEvents, " +
    s"wb events: $enableWbEvents" + "\n")

  // TODO: Need to provide core ID at the input to the replacement policy
  val basePolRead = Module(basePolicyType.buildBasePolicyRead(nWays, nSets, repSetFormat))
  val assignArr = Module(new LineAssignmentsArray(nWays, nSets, nCores))

  // Need to delay this signal by two CCs since the bit plru uses memory to store the MRU bits
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
  val wbStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val wbStageMruBits = WireDefault(0.U(basePolRead.stateWidth.W))
  val wbStageRepWay = WireDefault(0.U(log2Up(nWays).W))
  val wbStageUpdateCore = WireDefault(0.U(log2Up(nCores).W))
  val repWayValid = WireDefault(false.B)

  basePolRead.io.stall := io.control.stall
  basePolRead.io.rIdx := io.control.setIdx
  basePolRead.io.wrEn := io.control.update.valid
  basePolRead.io.wIdx := wbStageSetIdx
  basePolRead.io.wData := wbStageMruBits
  basePolRead.io.fwd := wbStageSetIdx === idxDelayReg && io.control.update.valid

  assignArr.io.stall := io.control.stall
  assignArr.io.rSet := idxDelayReg
  assignArr.io.wrEn := io.control.evict
  assignArr.io.wrSet := wbStageSetIdx
  assignArr.io.wrWay := wbStageRepWay
  assignArr.io.wrLineAssign := wbStageUpdateCore

  val replaceSetPipeReg = PipelineReg(basePolRead.io.replacementSet, getDefaultRepSet, !io.control.stall)
  val mruBitsPipeReg = PipelineReg(basePolRead.io.readState, 0.U, !io.control.stall)
  val setIdxPipeReg = PipelineReg(idxDelayReg, 0.U, !io.control.stall)

  // ---------------- Update stage ----------------
  val coreTable = Module(new CoreContentionTable(nCores))
  val critFilter = Module(new CriticalityFilter(nWays, nCores, nMshrs = missQueueDepth, repSetFormat, enableMissInMiss, enablePrecedentEvents, enableWbEvents))
  val basePolUpdate = Module(basePolicyType.buildBasePolicyUpdate(nWays))

  critFilter.io.evict := io.control.evict
  critFilter.io.update := io.control.update.valid
  critFilter.io.hit := io.info.isHit
  critFilter.io.hitWayIdx := io.control.update.bits
  critFilter.io.reqCore := io.info.updateCoreId
  critFilter.io.baseCandidates := replaceSetPipeReg
  critFilter.io.lineAssignments := assignArr.io.rLineAssign
  critFilter.io.validLineAssignments := assignArr.io.rValidAssign
  critFilter.io.coreLimits := coreTable.io.rLimits
  critFilter.io.criticalCores := coreTable.io.rCritCores
  critFilter.io.missQueueCores := io.info.missQueueCores
  critFilter.io.missQueueValidCores := io.info.missQueueValidCores
  critFilter.io.wbNonCritPop := io.info.nonCritWbPop
  critFilter.io.wbPopEntryCrit := io.info.nonCritWbEntryIsCrit

  coreTable.io.schedCoreId := io.scheduler.addr
  coreTable.io.setCritical := io.scheduler.cmd === SchedulerCmd.WR
  coreTable.io.unsetCritical := io.scheduler.cmd === SchedulerCmd.RD
  coreTable.io.wrEn := critFilter.io.updateCoreLimits && critFilter.io.replacementWay.valid
  coreTable.io.wrCoreLimits := critFilter.io.newCoreLimits
  coreTable.io.setContLimit := io.scheduler.wData
  io.scheduler.rData := coreTable.io.readData

  // Update base policy
  basePolUpdate.io.hitWay := io.control.update.bits
  basePolUpdate.io.stateIn := mruBitsPipeReg

  // Base policy forwarding signals
  wbStageSetIdx := setIdxPipeReg
  wbStageRepWay := critFilter.io.replacementWay.bits
  wbStageMruBits := basePolUpdate.io.stateOut
  wbStageUpdateCore := io.info.updateCoreId

  io.control.replaceWay := critFilter.io.replacementWay.bits
  io.control.isValid := critFilter.io.replacementWay.valid

  io.info.isReplacementWayCrit := critFilter.io.isReplacementWayCrit
  io.info.isReplacementWayAtLimit := critFilter.io.isReplacementWayAtLimit
  io.info.updateCoreReachedLimit := coreTable.io.rCritCores(io.info.updateCoreId) && (coreTable.io.rLimits(io.info.updateCoreId) === 0.U)
  io.info.updateCoreIsCrit := coreTable.io.rCritCores(io.info.updateCoreId)
}