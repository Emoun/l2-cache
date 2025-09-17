package caches.hardware.reppol

import caches.hardware.util.Constants._
import chisel3._
import chisel3.util._

class ContentionReplacementAlgorithm(nWays: Int, nCores: Int, nMshrs: Int = 4, enableMissInMiss: Boolean = false, enablePrecedentEvents: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val evict = Input(Bool())
    val update = Input(Bool())
    val hit = Input(Bool())
    val hitWayIdx = Input(UInt(log2Up(nWays).W))
    val reqCore = Input(UInt(log2Up(nCores).W))
    val missQueueEmpty = Input(Bool())
    val missQueueCores = Input(Vec(nMshrs, UInt(log2Up(nCores).W)))
    val missQueueValidCores = Input(Vec(nMshrs, Bool()))
    val baseCandidates = Input(Vec(nWays, UInt(log2Up(nWays).W)))
    val validLineAssignments = Input(Vec(nWays, Bool()))
    val lineAssignments = Input(Vec(nWays, UInt(log2Up(nCores).W)))
    val coreLimits = Input(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val criticalCores = Input(Vec(nCores, Bool()))
    val updateCore = Output(Valid(UInt(log2Up(nCores).W)))
    val updateCoreMim = Output(Valid(UInt(log2Up(nCores).W)))
    val updateCoreMimEventCnt = Output(UInt(CONTENTION_LIMIT_WIDTH.W))
    val updateCorePrecedent = Output(Valid(UInt(log2Up(nCores).W)))
    val replacementWay = Output(Valid(UInt(log2Up(nWays).W)))
  })

  val isReqCoreCritical = io.criticalCores(io.reqCore)

  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val isValidReplaceWay = WireDefault(false.B)

  val criticalWays = VecInit(Seq.fill(nWays)(false.B))
  val unlimitedWays = VecInit(Seq.fill(nWays)(false.B))
  val coreAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))

  val updateCore = WireDefault(false.B)
  val updateCoreId = WireDefault(0.U(log2Up(nCores).W))
  val updateCoreMim = WireDefault(false.B)
  val updateCoreIdMim = WireDefault(0.U(log2Up(nCores).W))
  val updateCoreMimEventCnt = WireDefault(0.U(log2Up(nMshrs).W))
  val updateCorePrecedent = WireDefault(false.B)
  val updateCoreIdPrecedent = WireDefault(0.U(log2Up(nCores).W))

  // Determine critical and unlimited ways
  for (i <- 0 until nWays) {
    val wayIdx = io.baseCandidates(i)
    val assignedCoreIdx = io.lineAssignments(wayIdx)
    val hasValidAssignment = io.validLineAssignments(wayIdx)

    val limit = Mux(hasValidAssignment, io.coreLimits(assignedCoreIdx), 0.U(CONTENTION_LIMIT_WIDTH.W))
    val isCriticalWay = Mux(hasValidAssignment, io.criticalCores(assignedCoreIdx), false.B)

    val isUnlimited = !isCriticalWay || (isCriticalWay && limit > 0.U)

    unlimitedWays(i) := isUnlimited
    criticalWays(i) := isCriticalWay
    coreAssignments(i) := assignedCoreIdx
  }

  val firstUCSetIdx = PriorityEncoder(unlimitedWays)
  val firstUCSetWayCoreCritical = criticalWays(firstUCSetIdx)
  val firstUCSetWayCore = coreAssignments(firstUCSetIdx)
  val anyNonCriticalWays = criticalWays.map(x => !x).reduce((x, y) => x || y)

  // This will address both miss in miss and miss-q events
  val missInMissEvent = isReqCoreCritical && io.evict && !io.missQueueEmpty // TODO: Can replace io.evict with !io.hit instead

  // Trigger Miss-In-Miss event
  if (enableMissInMiss) {
    val nonCritMissesInQueue = VecInit(Seq.fill(nMshrs)(false.B))
    for (i <- 0 until nMshrs) {
      nonCritMissesInQueue(i) := io.missQueueValidCores(i) && !io.criticalCores(io.missQueueCores(i))
    }

    when(missInMissEvent) {
      updateCoreMim := true.B
      updateCoreIdMim := io.reqCore
      updateCoreMimEventCnt := PopCount(nonCritMissesInQueue)
    }
  }

  // Check if it is a precedent event
  if (enablePrecedentEvents) {
    // If the core whose way we hit is not owned by the requesting core and the owner is not a critical core, then we increment the contention limit.
    // Additionally, we only increment it if the requesting core is critical, since it would make no sense to do that for a non-critical core.
    val ownerCore = io.lineAssignments(io.hitWayIdx)
    val precedentEvent = isReqCoreCritical && (ownerCore =/= io.reqCore) && !io.criticalCores(ownerCore) && io.update && io.hit
    when(precedentEvent) {
      updateCorePrecedent := true.B
      updateCoreIdPrecedent := io.reqCore
    }
  }

  when(unlimitedWays.reduce((x, y) => x || y)) {
    // If we encounter a contention event we need to update the contention count
    val evictionEvent = firstUCSetWayCoreCritical && anyNonCriticalWays
    val replacementEvent = firstUCSetWayCoreCritical && !isReqCoreCritical
    when(io.evict && (evictionEvent || replacementEvent)) {
      updateCore := true.B
      updateCoreId := firstUCSetWayCore
    }
    replaceWay := io.baseCandidates(firstUCSetIdx)
    isValidReplaceWay := true.B
  }.elsewhen(isReqCoreCritical) {
    replaceWay := io.baseCandidates(0)
    isValidReplaceWay := true.B
  }

  io.updateCore.valid := updateCore
  io.updateCore.bits := updateCoreId
  io.updateCoreMim.valid := updateCoreMim
  io.updateCoreMim.bits := updateCoreIdMim
  io.updateCoreMimEventCnt := updateCoreMimEventCnt
  io.updateCorePrecedent.valid := updateCorePrecedent
  io.updateCorePrecedent.bits := updateCoreIdPrecedent
  io.replacementWay.valid := isValidReplaceWay
  io.replacementWay.bits := replaceWay
}
