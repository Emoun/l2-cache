package caches.hardware.reppol

import caches.hardware.util.Constants.{CONTENTION_LIMIT_WIDTH, CORE_REQUEST_ID_WIDTH}
import chisel3._
import chisel3.util._

class SharedReplacementPolicyIO(ways: Int, sets: Int) extends ReplacementPolicyIO(ways, sets) {
  val setCritical = Input(Valid(UInt(CORE_REQUEST_ID_WIDTH.W)))
  val unsetCritical = Input(Valid(UInt(CORE_REQUEST_ID_WIDTH.W)))
  val contentionLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
}

class SharedCacheReplacementPolicyType(ways: Int, sets: Int) extends Module {
  val io = IO(new SharedReplacementPolicyIO(ways, sets))

  val setReplaceWays = VecInit(Seq.fill(sets)(0.U(log2Up(ways).W)))
  val setValidWays = VecInit(Seq.fill(ways)(false.B))
}
