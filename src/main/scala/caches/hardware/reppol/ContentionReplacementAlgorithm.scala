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
    val baseCandidates = Input(Vec(nWays, UInt(log2Up(nWays).W))) // TODO: Instead of giving order list of way indexes, give a list where the first element refers to the index of the first way in the list
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

  val updateCore = WireDefault(false.B)
  val updateCoreId = WireDefault(0.U(log2Up(nCores).W))
  val updateCoreMim = WireDefault(false.B)
  val updateCoreIdMim = WireDefault(0.U(log2Up(nCores).W))
  val updateCoreMimEventCnt = WireDefault(0.U(log2Up(nMshrs).W))
  val updateCorePrecedent = WireDefault(false.B)
  val updateCoreIdPrecedent = WireDefault(0.U(log2Up(nCores).W))

  // Compute UC mask
  val criticalWays = VecInit(Seq.fill(nWays)(false.B))
  val unlimitedWays = VecInit(Seq.fill(nWays)(false.B))
  val coreAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))

  for (wayIdx <- 0 until nWays) {
    val assignedCoreIdx = io.lineAssignments(wayIdx)
    val hasValidAssignment = io.validLineAssignments(wayIdx)

    val limit = io.coreLimits(assignedCoreIdx)
    val isCriticalWay = hasValidAssignment && io.criticalCores(assignedCoreIdx)

    val isUnlimited = !isCriticalWay || (isCriticalWay && limit > 0.U)

    unlimitedWays(wayIdx) := isUnlimited
    criticalWays(wayIdx) := isCriticalWay
    coreAssignments(wayIdx) := assignedCoreIdx
  }

  // Order the UC mask
  val orderedUcMask = VecInit(Seq.fill(nWays)(false.B))
  for (i <- 0 until nWays) {
    orderedUcMask(i) := unlimitedWays(io.baseCandidates(i))
  }

//  val firstUCSetIdx = PriorityEncoder(orderedUcMask)
  val firstUCWay = io.baseCandidates(PriorityEncoder(orderedUcMask))
  val firstUCSetWayCoreCritical = criticalWays(firstUCWay)
  val firstUCSetWayCore = coreAssignments(firstUCWay)
  val anyNonCriticalWays = criticalWays.map(x => !x).reduce((x, y) => x || y)

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

  val evictionEvent = WireDefault(false.B)
  val replacementEvent = WireDefault(false.B)

  when(unlimitedWays.reduce((x, y) => x || y)) {
    // If we encounter a contention event we need to update the contention count
    evictionEvent := firstUCSetWayCoreCritical && anyNonCriticalWays
    replacementEvent := firstUCSetWayCoreCritical && !isReqCoreCritical
    replaceWay := firstUCWay
    isValidReplaceWay := true.B
  }.elsewhen(isReqCoreCritical) {
    replaceWay := io.baseCandidates(0)
    isValidReplaceWay := true.B
  }

  // Decrement the contention limit when we encounter an eviction or replacement event
  when(io.evict && (evictionEvent || replacementEvent)) {
    updateCore := true.B
    updateCoreId := firstUCSetWayCore
  }

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
