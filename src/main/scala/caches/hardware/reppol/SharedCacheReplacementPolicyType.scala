package caches.hardware.reppol

import caches.hardware.util.Constants.{CONTENTION_LIMIT_WIDTH, CORE_REQUEST_ID_WIDTH}
import chisel3._
import chisel3.util._

class ReplacementPolicyIO(ways: Int, sets: Int) extends Bundle {
  val update = Input(Valid(UInt(log2Up(ways).W)))
  val evict = Input(Bool()) // Some policies may need to know if when the line is being evicted
  val setIdx = Input(UInt(log2Up(sets).W))
  val reqId = Input(UInt(CORE_REQUEST_ID_WIDTH.W)) // ID of the requesting core
  val replaceWay = Output(UInt(log2Up(ways).W))
  val isValid = Output(Bool()) // To signal if there are no valid ways to replace
}

class SchedulerIO(nCores: Int) extends Bundle {
  val setCritical = Input(Valid(UInt(log2Up(nCores).W)))
  val unsetCritical = Input(Valid(UInt(log2Up(nCores).W)))
  val contentionLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
}

/**
 * A replacement policy for a shared set associate cache.
 *
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class SharedCacheReplacementPolicyType(ways: Int, sets: Int, nCores: Int) extends Module {
  val io = IO(new Bundle {
    val control = new ReplacementPolicyIO(ways, sets)
    val scheduler = new SchedulerIO(nCores)
  })

  /**
   * Store the way to replace for each set
   */
  val setReplaceWays = VecInit(Seq.fill(sets)(0.U(log2Up(ways).W)))

  /**
   * Indicates if there are any valid ways to evict, i.e. empty set or not
   */
  val setValidWays = VecInit(Seq.fill(sets)(false.B))
}
