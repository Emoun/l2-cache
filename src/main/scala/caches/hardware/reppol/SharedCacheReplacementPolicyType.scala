package caches.hardware.reppol

import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH
import chisel3._
import chisel3.util._

class ReplacementPolicyIO(nWays: Int, nSets: Int, nCores: Int) extends Bundle {
  val update = Input(Valid(UInt(log2Up(nWays).W)))
  val stall = Input(Bool())
  val evict = Input(Bool()) // Some policies may need to know if when the line is being evicted
  val setIdx = Input(UInt(log2Up(nSets).W))
  val coreId = Input(UInt(log2Up(nCores).W)) // ID of the requesting core
  val replaceWay = Output(UInt(log2Up(nWays).W))
  val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W))) // If a replacement policy needs an ordered set of ways, otherwise can be ignored
  val isValid = Output(Bool()) // To signal if there are no valid ways to replace
}

class SchedulerIO(nCores: Int) extends Bundle {
  val coreId = Input(Valid(UInt(log2Up(nCores).W)))
  val setCritical = Input(Bool())
  val unsetCritical = Input(Bool())
  val contentionLimit = Input(UInt(CONTENTION_LIMIT_WIDTH.W))
}

class SharedCacheReplacementIO(nWays: Int, nSets: Int, nCores: Int) extends Bundle {
  val control = new ReplacementPolicyIO(nWays, nSets, nCores)
  val scheduler = new SchedulerIO(nCores)
}

/**
 * A replacement policy for a shared set associate cache.
 *
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class SharedCacheReplacementPolicyType(nWays: Int, nSets: Int, nCores: Int) extends Module {
  val io = IO(new SharedCacheReplacementIO(nWays, nSets, nCores))

  def pipelineReg[T <: Data](next: T, init: T, en: Bool): T = {
    val pipelineReg = RegInit(init)
    when(en) {
      pipelineReg := next
    }
    pipelineReg
  }

  /**
   * Store the way to replace for each set
   */
  val setReplaceWays = VecInit(Seq.fill(nSets)(0.U(log2Up(nWays).W)))

  /**
   * Indicates if there are any valid ways to evict, i.e. empty set or not
   */
  val setValidWays = VecInit(Seq.fill(nSets)(false.B))
}
