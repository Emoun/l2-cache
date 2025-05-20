package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * A replacement policy for a set associate cache.
 *
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class ReplacementPolicyType(ways: Int, sets: Int) extends Module {
  val io = IO(new Bundle {
    val index = Input(UInt(log2Up(sets).W))
    val update = Input(Valid(UInt(log2Up(ways).W)))
    val replaceWay = Output(UInt(log2Up(ways).W))
  })
}
