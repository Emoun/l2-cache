package caches.hardware.reppol

import caches.hardware.util.Constants.CORE_REQUEST_ID_WIDTH
import chisel3._
import chisel3.util._

class ReplacementPolicyIO(ways: Int, sets: Int) extends Bundle {
  val update = Input(Valid(UInt(log2Up(ways).W)))
  val evict = Input(Bool()) // Some policies may need to know if when the line is being evicted
  val setIdx = Input(UInt(log2Up(sets).W))
  val reqID = Input(UInt(CORE_REQUEST_ID_WIDTH.W)) // ID of the requesting core
  val replaceWay = Output(UInt(log2Up(ways).W))
  val isValid = Output(Bool()) // To signal if there are no valid ways to replace
}

/**
 * A replacement policy for a set associate cache.
 *
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class ReplacementPolicyType(ways: Int, sets: Int) extends Module {
  val io = IO(new ReplacementPolicyIO(ways, sets))

  /**
   * Store the way to replace for each set
   */
  val setReplaceWays = VecInit(Seq.fill(sets)(0.U(log2Up(ways).W)))

  /**
   * Indicates if there are any valid ways to evict, i.e. empty set or not
   */
  val setValidWays = VecInit(Seq.fill(ways)(false.B))
}