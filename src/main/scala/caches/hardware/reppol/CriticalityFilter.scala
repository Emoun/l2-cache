package caches.hardware.reppol

import caches.hardware.util.Constants._
import chisel3._
import chisel3.util._

class CriticalityFilterIO(nWays: Int, nCores: Int, nMshrs: Int, repSetFormat: BaseReplacementSetFormat) extends Bundle {
    val evict = Input(Bool())
    val update = Input(Bool())
    val hit = Input(Bool())
    val hitWayIdx = Input(UInt(log2Up(nWays).W))
    val reqCore = Input(UInt(log2Up(nCores).W))
    val missQueueCores = Input(Vec(nMshrs, UInt(log2Up(nCores).W)))
    val wbNonCritPop = Input(Bool())
    val wbPopEntryCrit = Input(Bool())
    val missQueueValidCores = Input(Vec(nMshrs, Bool()))
    val baseCandidates = repSetFormat match {
      case NumericalFormat() => Input(Vec(nWays, UInt(log2Up(nWays).W)))
      case MruFormat() => Input(Vec(nWays, UInt(1.W)))
      case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
    }
    val validLineAssignments = Input(Vec(nWays, Bool()))
    val lineAssignments = Input(Vec(nWays, UInt(log2Up(nCores).W)))
    val coreLimits = Input(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val criticalCores = Input(Vec(nCores, Bool()))
    val updateCoreLimits = Output(Bool())
    val newCoreLimits = Output(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val replacementWay = Output(Valid(UInt(log2Up(nWays).W)))
    val isReplacementWayCrit = Output(Bool())
    val isReplacementWayAtLimit = Output(Bool())
}

class CriticalityFilter(
                                      nWays: Int,
                                      nCores: Int,
                                      nMshrs: Int = 4,
                                      repSetFormat: BaseReplacementSetFormat,
                                      enableMissInMiss: Boolean = false,
                                      enablePrecedentEvents: Boolean = false,
                                      enableWbEvents: Boolean = false
                                    ) extends Module {
  val io = IO(new CriticalityFilterIO(nWays, nCores, nMshrs, repSetFormat))

  def mruFilter(baseCandidates: Vec[UInt], ucMask: Vec[Bool]): UInt = {
    val anyUcInFirstSet = (~baseCandidates.asUInt).asUInt & ucMask.asUInt
    val anyUcInSecondSet = baseCandidates.asUInt & ucMask.asUInt
    val baseCandMask = WireDefault(0.U(nWays.W))

    when(anyUcInFirstSet.orR) {
      baseCandMask := anyUcInFirstSet
    }.otherwise {
      baseCandMask := anyUcInSecondSet
    }

    PriorityEncoder(baseCandMask)
  }

  def numericalFilter(baseCandidates: Vec[UInt], ucMask: Vec[Bool]): UInt = {
    // Order the UC mask
    val orderedUcMask = VecInit(Seq.fill(nWays)(false.B))
    for (i <- 0 until nWays) {
      orderedUcMask(i) := ucMask(baseCandidates(i))
    }

    baseCandidates(PriorityEncoder(orderedUcMask))
  }

  // Default signals
  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val isValidReplaceWay = WireDefault(false.B)
  val isReplaceWayCrit = WireDefault(false.B)
  val isRepWayAtLimit = WireDefault(false.B)
  val updateCoreLimits = WireDefault(false.B)

  val criticalWays = VecInit(Seq.fill(nWays)(false.B))
  val unlimitedWays = VecInit(Seq.fill(nWays)(false.B))
  val coreAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))

  val isReqCoreCritical = io.criticalCores(io.reqCore)

  // Compute UC mask
  for (wayIdx <- 0 until nWays) {
    val assignedCoreIdx = io.lineAssignments(wayIdx)
    val hasValidAssignment = io.validLineAssignments(wayIdx)

    val limit = io.coreLimits(assignedCoreIdx)
    val isCriticalWay = hasValidAssignment && io.criticalCores(assignedCoreIdx)
    val criticalAtLimit = isCriticalWay && limit > 0.U

    val isUnlimited = !isCriticalWay || criticalAtLimit

    unlimitedWays(wayIdx) := isUnlimited
    criticalWays(wayIdx) := isCriticalWay
    coreAssignments(wayIdx) := assignedCoreIdx
  }

  val firstUCWay = repSetFormat match {
    case NumericalFormat() => numericalFilter(io.baseCandidates, unlimitedWays)
    case MruFormat() => mruFilter(io.baseCandidates, unlimitedWays)
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }

  val firstUCSetWayCoreCritical = criticalWays(firstUCWay)
  val firstUCSetWayCore = coreAssignments(firstUCWay)
  val anyNonCriticalWays = criticalWays.map(x => !x).reduce((x, y) => x || y)

  val evictionEvent = WireDefault(false.B)
  val replacementEvent = WireDefault(false.B)

  when(unlimitedWays.reduce((x, y) => x || y)) {
    // If we encounter a contention event we need to update the contention count
    evictionEvent := firstUCSetWayCoreCritical && anyNonCriticalWays
    replacementEvent := firstUCSetWayCoreCritical && !isReqCoreCritical
    replaceWay := firstUCWay
    isReplaceWayCrit := firstUCSetWayCoreCritical
    isValidReplaceWay := true.B
    isRepWayAtLimit := false.B
  }.elsewhen(isReqCoreCritical) {
    replaceWay := io.baseCandidates(0) // Critical core, can evict any line
    isReplaceWayCrit := criticalWays(io.baseCandidates(0))
    isValidReplaceWay := true.B
    isRepWayAtLimit := true.B
  }

  // Decrement the contention limit when we encounter an eviction or replacement event
  val coreContIncrRep = VecInit(Seq.fill(nCores)(0.U(1.W)))
  when(io.evict && (evictionEvent || replacementEvent)) {
    updateCoreLimits := true.B
    coreContIncrRep(firstUCSetWayCore) := 1.U
  }

  // Precedent Event
  val coreContIncrPrecedent = VecInit(Seq.fill(nCores)(0.U(1.W)))
  if (enablePrecedentEvents) {
    // If the core whose way we hit is not owned by the requesting core and the owner is not a critical core, then we increment the contention limit.
    // Additionally, we only increment it if the requesting core is critical, since it would make no sense to do that for a non-critical core.
    val ownerCore = io.lineAssignments(io.hitWayIdx)
    val precedentEvent = isReqCoreCritical && (ownerCore =/= io.reqCore) && !io.criticalCores(ownerCore) && io.update && io.hit

    when(precedentEvent) {
      updateCoreLimits := true.B
      coreContIncrPrecedent(io.reqCore) := 1.U
    }
  }

  // Trigger Miss-In-Miss and Miss-Q events
  val coreContIncrMiM = VecInit(Seq.fill(nCores)(0.U(nMshrs.W)))
  if (enableMissInMiss) {
    // This will address both miss in miss and miss-q events
    val nonCritMissCnt = VecInit(Seq.fill(nMshrs)(false.B))
    for (i <- 0 until nMshrs) {
      nonCritMissCnt(i) := io.missQueueValidCores(i) && !io.criticalCores(io.missQueueCores(i))
    }

    val missInMissEvent = isReqCoreCritical && io.evict && nonCritMissCnt.reduce((x, y) => x || y) // TODO: Can replace io.evict with !io.hit and isReqValid instead

    when(missInMissEvent) {
      updateCoreLimits := true.B
      coreContIncrMiM(io.reqCore) := PopCount(nonCritMissCnt)
    }
  }

  // Writeback event
  val coreContIncrWb = VecInit(Seq.fill(nCores)(0.U(1.W)))
  if (enableWbEvents) {
    // If mem interface pops from non-critical wb-q and the popped entry is not critical,
    // we check if there are any critical lines in the miss-q and if so, it is a wb event for each unique core
    val wbPopNonCrit = io.wbNonCritPop && !io.wbPopEntryCrit

    // Check if the valid missQueueLines have critical cores, and if so that would be an event per each unique core
    for (i <- 0 until nMshrs) {
      val wbEvent = io.missQueueValidCores(i) && io.criticalCores(io.missQueueCores(i))

      when(wbEvent && wbPopNonCrit) {
        updateCoreLimits := true.B
        coreContIncrWb(io.missQueueCores(i)) := 1.U
      }
    }

  }

  // TODO: This creates a long combinational path
  val newCoreLimits = VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W)))
  for (i <- 0 until nCores) {
    val decrAmount = WireDefault(0.U(log2Up(nMshrs + 3).W))
    val incrAmount = WireDefault(0.U(1.W))

    decrAmount := coreContIncrRep(i) + coreContIncrMiM(i) + coreContIncrWb(i)
    incrAmount := coreContIncrPrecedent(i)

    val coreLimit = io.coreLimits(i)
    val maxLimit = ((1 << CONTENTION_LIMIT_WIDTH) - 1).U

    val decrCoreLimit = Mux(coreLimit < decrAmount, 0.U, coreLimit - decrAmount)
    val incrCoreLimit = Mux(decrCoreLimit === maxLimit, maxLimit, decrCoreLimit + incrAmount)

    newCoreLimits(i) := incrCoreLimit
  }

  io.updateCoreLimits := updateCoreLimits
  io.newCoreLimits := newCoreLimits
  io.replacementWay.valid := isValidReplaceWay
  io.replacementWay.bits := replaceWay
  io.isReplacementWayCrit := isReplaceWayCrit // Need to know if the replacement way belongs to a critical core
  io.isReplacementWayAtLimit := isRepWayAtLimit // Need to know if the core to which the line belongs to reached limit
}
