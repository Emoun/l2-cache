package caches.hardware.reppol

import chisel3._
import chisel3.util._

object SchedulerCmd {
  val schedulerCmdWidth = 2

  val NULL = "b00".U(schedulerCmdWidth.W)
  val RD = "b01".U(schedulerCmdWidth.W)
  val WR  = "b10".U(schedulerCmdWidth.W)
}

class SchedulerControlIO(nCores: Int, dataWidth: Int) extends Bundle {
  val cmd = Input(UInt(2.W))
  val addr = Input(UInt(log2Up(nCores).W))
  val wData = Input(UInt(dataWidth.W))
  val rData = Output(UInt(dataWidth.W))
}

class ReplacementPolicyIO(nWays: Int, nSets: Int, nCores: Int) extends Bundle {
  val update = Input(Valid(UInt(log2Up(nWays).W)))
  val updateCoreId = Input(UInt(log2Up(nCores).W))
  val stall = Input(Bool())
  val evict = Input(Bool()) // Some policies may need to know if when the line is being evicted
  val setIdx = Input(UInt(log2Up(nSets).W))
  val coreId = Input(UInt(log2Up(nCores).W)) // ID of the requesting core
  val popRejQueue = Valid(UInt((log2Up(nCores) + 1).W)) // For specifying how many entries should be popped from the rejection queue
  val isValid = Output(Bool()) // To signal if there are no valid ways to replace
  val replaceWay = Output(UInt(log2Up(nWays).W))
  val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W))) // If a replacement policy needs an ordered set of ways, otherwise can be ignored
  val missActive = Input(Bool()) // Whether misses are currently being serviced by the miss queue
}

class SharedCacheReplacementIO(nWays: Int, nSets: Int, nCores: Int, dataWidth: Int) extends Bundle {
  val control = new ReplacementPolicyIO(nWays, nSets, nCores)
  val scheduler = new SchedulerControlIO(nCores, dataWidth)
}

/**
 * A replacement policy for a shared set associate cache.
 *
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class SharedCacheReplacementPolicyType(nWays: Int, nSets: Int, nCores: Int, dataWidth: Int = 1) extends Module {
  val io = IO(new SharedCacheReplacementIO(nWays, nSets, nCores, dataWidth))

  val schedulerDataWidth = dataWidth
}
