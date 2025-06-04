package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import chisel3._
import chisel3.util._

/**
 * On a hit we update only the base policy, on an eviction we update the contention policy.
 *
 * @param ways number of ways in a cache set
 * @param sets number of sets in a cache
 * @param nCores number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class ContentionReplacementPolicy(ways: Int, sets: Int, nCores: Int, basePolicy: () => ReplacementAlgorithm) extends SharedCacheReplacementPolicyType(ways, sets, nCores) {
  // Base policy for each set
  val basePolicies = Array.fill(sets)(Module(basePolicy()))

  // Registers for keeping the state of each active core
  val contentionLimits = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W)))) // TODO: If all cores have the same contention limit, we need not have this
  val contentionCounts = RegInit(VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))))
  val criticalCores = RegInit(VecInit(Seq.fill(nCores)(false.B)))

  // Registers for keeping track of which cache line is assigned to which core and whether the assignment is valid
  val lineAssignments = Array.fill(sets)(RegInit(VecInit(Seq.fill(ways)(0.U(log2Up(nCores).W)))))
  val validAssignment = Array.fill(sets)(RegInit(VecInit(Seq.fill(ways)(false.B))))

  val contentionOverflow = VecInit(Seq.fill(nCores)(0.U(CONTENTION_LIMIT_WIDTH.W))) // TODO: Turn this into registers and compute it when a core is set or unset

  private def getAssignedCoreIdx(set: Int, way: UInt): (UInt, Bool) = {
    (lineAssignments(set)(way), validAssignment(set)(way))
  }

  private def getContention(coreIdx: (UInt, Bool)): UInt = {
    Mux(coreIdx._2, contentionCounts(coreIdx._1), 0.U(CONTENTION_LIMIT_WIDTH.W))
  }

  private def getLimit(coreIdx: (UInt, Bool)): UInt = {
    Mux(coreIdx._2, contentionLimits(coreIdx._1), 0.U(CONTENTION_LIMIT_WIDTH.W))
  }

  private def isCritical(coreIdx: (UInt, Bool)): Bool = {
    Mux(coreIdx._2, criticalCores(coreIdx._1), false.B)
  }

  private def getOverflow(coreIdx: (UInt, Bool)): UInt = {
    Mux(coreIdx._2, contentionOverflow(coreIdx._1), 0.U(CONTENTION_LIMIT_WIDTH.W))
  }

  def isUnlimitedWay(set: Int, way: UInt): Bool = {
    val coreIdx = getAssignedCoreIdx(set, way)
    val critical = isCritical(coreIdx)

    !critical || (critical && getLimit(coreIdx) >= getOverflow(coreIdx))
  }

  // TODO: Add registers to hold the replacement set, as updating the base policy and updating contention
  // counters never happens in the same cycle

  def filterVec(set: Int, filterFunc: (Int, UInt) => Bool, array: Vec[UInt], previousOut: Option[Vec[Bool]]): Vec[Bool] = {
    val filterVec = VecInit(Seq.fill(ways)(false.B))
    val previousFilterVec = VecInit(Seq.fill(ways)(true.B))

    // If we want filter a Vec based on the output of the previous filter
    if (previousOut.isDefined) {
      previousFilterVec := previousOut.get
    }

    for (i <- 0 until ways) {
      filterVec(i) := filterFunc(set, array(i)) && previousFilterVec(i)
    }

    filterVec
  }

  def getEvictCand(set: Int): (UInt, Bool) = {
    val evictWay = WireDefault(0.U(log2Up(ways).W))
    val isValidReplaceWay = WireDefault(false.B)

    val baseEvictionCands = basePolicies(set).io.replacementSet

    // Filter out base candidates based on which way is critical and which way is not
    val unlimitedWays = filterVec(set, isUnlimitedWay, baseEvictionCands, None)

    val reqCoreIdx = io.control.reqId
    val reqCoreCritical = isCritical((reqCoreIdx, true.B))

    when(unlimitedWays.reduce((x, y) => x || y)) {
      val nonCriticalWays = filterVec(set, (set, way: UInt) => { !isCritical(getAssignedCoreIdx(set, way)) }, baseEvictionCands, Some(unlimitedWays))

      // Use the priority encoder to get the index of the first way that is unlimited
      evictWay := baseEvictionCands(PriorityEncoder(unlimitedWays))
      isValidReplaceWay := true.B

      val evictWayCoreIdx = getAssignedCoreIdx(set, evictWay)

      when(io.control.evict && (io.control.setIdx === set.U)) {
        // If we encounter a contention event we need to update the contention count
        when(isCritical(evictWayCoreIdx) && (!reqCoreCritical || nonCriticalWays.reduce((x, y) => x || y))) { // !io.unsetCritical.valid
          when(evictWayCoreIdx._2) { // Only update contention count if the way we are evicting is assigned to a core
            contentionCounts(evictWayCoreIdx._1) := contentionCounts(evictWayCoreIdx._1) + 1.U
          }
        }

        lineAssignments(set)(evictWay) := reqCoreIdx
        validAssignment(set)(evictWay) := true.B
      }
    } .elsewhen(reqCoreCritical) {
      evictWay := baseEvictionCands(0)
      isValidReplaceWay := true.B

      when(io.control.evict && (io.control.setIdx === set.U)) {
        lineAssignments(set)(evictWay) := reqCoreIdx
        validAssignment(set)(evictWay) := true.B
      }
    } .otherwise {
      isValidReplaceWay := false.B
    }

    (evictWay, isValidReplaceWay)
  }

  def updateBase(set: Int): Unit = {
    basePolicies(set).io.update.valid := io.control.update.valid && (io.control.setIdx === set.U)
    basePolicies(set).io.update.bits := io.control.update.bits
  }

  // Set and unset cores as critical
  for (core <- 0 until nCores) {
    when (io.scheduler.setCritical.valid && (io.scheduler.setCritical.bits === core.U)) {
      criticalCores(core) := true.B
      contentionLimits(core) := io.scheduler.contentionLimit
    } .elsewhen (io.scheduler.unsetCritical.valid && (io.scheduler.unsetCritical.bits === core.U)) {
      criticalCores(core) := false.B
      contentionCounts(core) := 0.U
    }

    contentionOverflow(core) := getContention((core.asUInt, true.B)) + 1.U
  }

  // For each set, get the candidate and update the policy (in this case we only update base policy)
  for (set <- 0 until sets) {
    val (repWay, isValid) = getEvictCand(set)
    setValidWays(set) := isValid
    setReplaceWays(set) := repWay

    updateBase(set)
  }

  io.control.replaceWay := setReplaceWays(io.control.setIdx)
  io.control.isValid := setValidWays(io.control.setIdx)
}