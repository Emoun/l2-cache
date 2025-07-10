package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import chisel3._
import chisel3.util._

/**
 * On a hit we update only the base policy, on an eviction we update the contention policy.
 *
 * @param nWays number of ways in a cache set
 * @param nSets number of sets in a cache
 * @param nCores number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class ContentionReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, basePolicy: () => ReplacementAlgorithmType) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores) {
  // TODO: Think if this should be pipelined too
  def lineAssignmentsArray(set: UInt, update: Bool, way: UInt): (Vec[UInt], Vec[Bool]) = {
    // Registers for keeping track of which cache line is assigned to which core and whether the assignment is valid
    val lineAssignments = Array.fill(nSets)(RegInit(VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))))
    val validLineAssignments = Array.fill(nSets)(RegInit(VecInit(Seq.fill(nWays)(false.B))))

    val selLineAssign = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))
    val selineValidAssign = VecInit(Seq.fill(nWays)(false.B))

    for (setIdx <- 0 until nSets) {
      when(setIdx.U === set) {
        selLineAssign := lineAssignments(setIdx)
        selineValidAssign := validLineAssignments(setIdx)

        when(update) {
          lineAssignments(setIdx)(way) := io.control.reqId
          validLineAssignments(setIdx)(way) := true.B
        }
      }
    }

    (selLineAssign, selineValidAssign)
  }

  def coreContentionTable(updateContention: Bool, updateCoreIdx: UInt): (Vec[UInt], Vec[Bool]) = {
    // Registers for keeping the state of each active core
    val contentionLimits = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))))
    val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

    // Set and unset cores as critical
    for (coreTableIdx <- 0 until nCores) {
      when (io.scheduler.setCritical && (io.scheduler.coreId.valid && io.scheduler.coreId.bits === coreTableIdx.U)) {
        criticalCores(coreTableIdx) := true.B
        contentionLimits(coreTableIdx) := io.scheduler.contentionLimit
      } .elsewhen (io.scheduler.unsetCritical && (io.scheduler.coreId.valid && io.scheduler.coreId.bits === coreTableIdx.U)) {
        criticalCores(coreTableIdx) := false.B
        contentionLimits(coreTableIdx) := 0.U
      }

      when(updateContention && updateCoreIdx === coreTableIdx.U) {
        contentionLimits(coreTableIdx) := contentionLimits(coreTableIdx) - 1.U
      }
    }

    (contentionLimits, criticalCores)
  }

  // Base policies for each set and a single contention policy
  val basePolicies = Array.fill(nSets)(Module(basePolicy())) // TODO: Add pipeline register here
  val contentionPolicy = Module(new ContentionReplacementAlgorithm(nWays, nCores))

  val assignmentsArray = lineAssignmentsArray(io.control.setIdx, io.control.evict, contentionPolicy.io.replacementWay.bits)
  val coreTable = coreContentionTable(contentionPolicy.io.updateCore.valid, contentionPolicy.io.updateCore.bits)

  val lineAssignments = assignmentsArray._1
  val validAssignments = assignmentsArray._2
  val coreLimits = coreTable._1
  val criticalCores = coreTable._2

  val isReqCoreCritical = criticalCores(io.control.reqId)
  val setBaseReplaceWays = VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W)))))

  for (set <- 0 until nSets) {
    // Update base policy
    basePolicies(set).io.update.valid := io.control.update.valid && (io.control.setIdx === set.U)
    basePolicies(set).io.update.bits := io.control.update.bits

    setBaseReplaceWays(set) := basePolicies(set).io.replacementSet
  }

  // Compute the eviction for each set
  contentionPolicy.io.evict := io.control.evict
  contentionPolicy.io.isReqCoreCritical := isReqCoreCritical
  contentionPolicy.io.baseCandidates := setBaseReplaceWays(io.control.setIdx)
  contentionPolicy.io.lineAssignments := lineAssignments
  contentionPolicy.io.validLineAssignments := validAssignments
  contentionPolicy.io.coreLimits := coreLimits
  contentionPolicy.io.criticalCores := criticalCores

  io.control.replaceWay := contentionPolicy.io.replacementWay.bits
  io.control.isValid := contentionPolicy.io.replacementWay.valid
}