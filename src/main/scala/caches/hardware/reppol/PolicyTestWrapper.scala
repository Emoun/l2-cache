package caches.hardware.reppol

import chisel3._
import chisel3.util.experimental.BoringUtils
import chisel3.util.log2Up

class PolicyDebugIO(nWays: Int, repSetFormat: BaseReplacementSetFormat) extends Bundle {
  val repSet = repSetFormat match {
    case NumericalFormat() => Output(Vec(nWays, UInt(log2Up(nWays).W)))
    case MruFormat() => Output(Vec(nWays, Bool()))
    case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
  }
}

/**
 * Chisel module that wraps any replacement policy that extends SharedCacheReplacementPolicyType in an interface
 * for testing.
 */
class PolicyTestWrapper(policyGenerator: () => SharedCacheReplacementPolicyType) extends Module {
  val pol = Module(policyGenerator())

  val io = IO(new Bundle {
    val dbg = new PolicyDebugIO(pol.getWays, pol.getReplacementSetFormat)
    val policy = new SharedCacheReplacementIO(pol.getWays, pol.getSets, pol.getCores, pol.getSchedulerDataWidth)
  })

  pol.io <> io.policy

  io.dbg.repSet := DontCare
  BoringUtils.bore(pol.repSet, Seq(io.dbg.repSet))
}
