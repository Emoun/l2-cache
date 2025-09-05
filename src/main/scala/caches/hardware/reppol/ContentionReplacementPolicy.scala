package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import caches.hardware.util.{MemBlock, PipelineReg, UpdateSingleVecElem}
import chisel3._
import chisel3.util._

class LineAssignmentsArray(nWays: Int, nSets: Int, nCores: Int) extends Module() {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val wrEn = Input(Bool())
    val rSet = Input(UInt(log2Up(nSets).W))
    val wrSet = Input(UInt(log2Up(nSets).W))
    val wrLineAssign = Input(Vec(nWays, UInt(log2Up(nCores).W)))
    val wrValiAssign = Input(Vec(nWays, Bool()))
    val rLineAssign = Output(Vec(nWays, UInt(log2Up(nCores).W)))
    val rValidAssign = Output(Vec(nWays, Bool()))
  })

  // Memory array for keeping track of line assignments
  val lineAssignments = Module(new MemBlock(nSets, nWays * log2Up(nCores)))
  val validLineAssignments = Module(new MemBlock(nSets, nWays))

  lineAssignments.io.wrEn := io.wrEn
  lineAssignments.io.readAddr := io.rSet
  lineAssignments.io.writeAddr := io.wrSet
  lineAssignments.io.writeData := io.wrLineAssign.asUInt
  lineAssignments.io.stall := io.stall

  validLineAssignments.io.wrEn := io.wrEn
  validLineAssignments.io.readAddr := io.rSet
  validLineAssignments.io.writeAddr := io.wrSet
  validLineAssignments.io.writeData := io.wrValiAssign.asUInt
  validLineAssignments.io.stall := io.stall

  // Break apart the read data into a VECs
  val rLineAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))
  val rValidLineAssignments = VecInit(Seq.fill(nWays)(false.B))

  for (idx <- 0 until nWays) {
    rLineAssignments(idx) := lineAssignments.io.readData((log2Up(nCores) - 1) + (idx * log2Up(nCores)), idx * log2Up(nCores))
    rValidLineAssignments(idx) := validLineAssignments.io.readData(idx)
  }

  io.rLineAssign := rLineAssignments
  io.rValidAssign := rValidLineAssignments
}

class CoreContentionTable(nCores: Int) extends Module() {
  val io = IO(new Bundle{
    val schedCoreId = Input(UInt(log2Up(nCores).W))
    val setCritical = Input(Bool())
    val unsetCritical = Input(Bool())
    val contentionLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
    val incrContention1 = Input(Valid(UInt(log2Up(nCores).W)))
    val incrContention2 = Input(Valid(UInt(log2Up(nCores).W)))
    val rLimits = Output(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val rCritCores = Output(Vec(nCores, Bool()))
    val freeRejectionQueue = Output(Bool())
  })

  // NOTE: There is no forwarding, if a core is set or unset as critical,
  // the current cache request will not see it until the next cycle.

  // Registers for keeping the state of each active core
  val contentionLimits = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))))
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

  // Set and unset cores as critical
  for (coreTableIdx <- 0 until nCores) {
    when (io.setCritical && io.schedCoreId === coreTableIdx.U) {
      criticalCores(coreTableIdx) := true.B
      contentionLimits(coreTableIdx) := io.contentionLimit
    } .elsewhen (io.unsetCritical && io.schedCoreId === coreTableIdx.U) {
      criticalCores(coreTableIdx) := false.B
      contentionLimits(coreTableIdx) := 0.U
    }

    when(io.incrContention1.valid) {
      when(io.incrContention1.bits === coreTableIdx.U){
        when(io.incrContention2.valid && io.incrContention1.bits === io.incrContention2.bits) {
          contentionLimits(coreTableIdx) := contentionLimits(coreTableIdx) - 2.U
        }.otherwise{
          contentionLimits(coreTableIdx) := contentionLimits(coreTableIdx) - 1.U
        }
      }
    }.elsewhen(io.incrContention2.valid && io.incrContention2.bits === coreTableIdx.U) {
      contentionLimits(coreTableIdx) := contentionLimits(coreTableIdx) - 1.U
    }
  }

  // When a core is unset we empty the rejection queue
  val freeRejQueue = WireDefault(false.B)
  when (io.unsetCritical) {
    freeRejQueue := criticalCores(io.schedCoreId) // Free the rejection queue if the request was critical
  }

  io.rLimits := contentionLimits
  io.rCritCores := criticalCores
  io.freeRejectionQueue := freeRejQueue
}

/**
 * On a hit we update only the base policy, on an eviction we update the contention policy.
 *
 * @param nWays number of ways in a cache set
 * @param nSets number of sets in a cache
 * @param nCores number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class ContentionReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, basePolicy: () => SharedCacheReplacementPolicyType, enableMissInMiss: Boolean = false) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, CONTENTION_LIMIT_WIDTH) {
  // ---------------- Base policy stage ----------------

  // Base policy instantiation
  val basePolicyInst = Module(basePolicy())

  // Update base policy
  basePolicyInst.io.control.setIdx := io.control.setIdx
  basePolicyInst.io.control.coreId := io.control.coreId
  basePolicyInst.io.control.evict := io.control.evict
  basePolicyInst.io.control.update.valid := io.control.update.valid
  basePolicyInst.io.control.update.bits := io.control.update.bits
  basePolicyInst.io.control.updateCoreId := io.control.update.bits
  basePolicyInst.io.control.stall := io.control.stall
  basePolicyInst.io.control.missActive := io.control.missActive
  basePolicyInst.io.scheduler <> io.scheduler

  // Need to delay this signal by two CCs since the bit plru uses memory to store the MRU bits
  val setIdxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
  val setIdxPipeReg = PipelineReg(setIdxDelayReg, 0.U, !io.control.stall)

  // ---------------- Eviction stage ----------------
  val contAlgorithm = Module(new ContentionReplacementAlgorithm(nWays, nCores, enableMissInMiss))

  val assignArr = Module(new LineAssignmentsArray(nWays, nSets, nCores))
  assignArr.io.stall := io.control.stall
  assignArr.io.wrEn := io.control.evict
  assignArr.io.rSet := setIdxDelayReg
  assignArr.io.wrSet := setIdxPipeReg
  assignArr.io.wrLineAssign := UpdateSingleVecElem(assignArr.io.rLineAssign, io.control.coreId, contAlgorithm.io.replacementWay.bits)
  assignArr.io.wrValiAssign := UpdateSingleVecElem(assignArr.io.rValidAssign, true.B, contAlgorithm.io.replacementWay.bits)

  val coreTable = Module(new CoreContentionTable(nCores))
  coreTable.io.schedCoreId := io.scheduler.addr
  coreTable.io.setCritical := io.scheduler.cmd === SchedulerCmd.WR
  coreTable.io.unsetCritical := io.scheduler.cmd === SchedulerCmd.RD
  coreTable.io.contentionLimit := io.scheduler.wData
  io.scheduler.rData := 0.U
  coreTable.io.incrContention1 := contAlgorithm.io.updateCore
  coreTable.io.incrContention2 := contAlgorithm.io.updateCoreMim

  // Compute the eviction for each set
  contAlgorithm.io.evict := io.control.evict
  contAlgorithm.io.reqCore := io.control.coreId
  contAlgorithm.io.baseCandidates := basePolicyInst.io.control.replacementSet
  contAlgorithm.io.lineAssignments := assignArr.io.rLineAssign
  contAlgorithm.io.validLineAssignments := assignArr.io.rValidAssign
  contAlgorithm.io.coreLimits := coreTable.io.rLimits
  contAlgorithm.io.criticalCores := coreTable.io.rCritCores
  contAlgorithm.io.missActive := io.control.missActive

  io.control.replaceWay := contAlgorithm.io.replacementWay.bits
  io.control.isValid := contAlgorithm.io.replacementWay.valid
  io.control.replacementSet := VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W)))

  io.control.popRejQueue.valid := coreTable.io.freeRejectionQueue
  io.control.popRejQueue.bits := nCores.U // Free the entire rejection queue
}

object ContentionReplacementPolicy extends App {
//  val l2Size = 262144 // 256 KiB
//  val l2Size = 16384 // 16 KiB
  val l2Size = 131072 // 128 KiB
  val l2Ways = 8
  val nCores = 4
  val l2BytesPerBlock = 64
  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)

  val plruL2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)

  (new chisel3.stage.ChiselStage).emitVerilog(
    new ContentionReplacementPolicy(
      l2Ways,
      l2nSets,
      nCores,
      plruL2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}