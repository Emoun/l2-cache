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

class ReplacementPolicyIO(nWays: Int, nSets: Int, nCores: Int, missQueueDepth: Int = 4) extends Bundle {
  val stall = Input(Bool())
  val evict = Input(Bool()) // Some policies may need to know if when the line is being evicted
  val isHit = Input(Bool())
  val missQueueEmpty = Input(Bool())
  val missQueueCores = Input(Vec(missQueueDepth, UInt(log2Up(nCores).W)))
  val missQueueValidCores = Input(Vec(missQueueDepth, Bool()))
  val update = Input(Valid(UInt(log2Up(nWays).W)))
  val setIdx = Input(UInt(log2Up(nSets).W))
  val updateCoreId = Input(UInt(log2Up(nCores).W))
  val isValid = Output(Bool()) // To signal if there are no valid ways to replace
  val replaceWay = Output(UInt(log2Up(nWays).W))
  val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W))) // If a replacement policy needs an ordered set of ways, otherwise can be ignored
  val popRejQueue = Valid(UInt((log2Up(nCores) + 1).W)) // For specifying how many entries should be popped from the rejection queue
  val pushReqToCritQueue = Output(Bool())
}

class SharedCacheReplacementIO(nWays: Int, nSets: Int, nCores: Int, dataWidth: Int, missQueueDepth: Int) extends Bundle {
  val control = new ReplacementPolicyIO(nWays, nSets, nCores, missQueueDepth)
  val scheduler = new SchedulerControlIO(nCores, dataWidth)
}

/**
 * A replacement policy for a shared set associate cache.
 *
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class SharedCacheReplacementPolicyType(nWays: Int, nSets: Int, nCores: Int, dataWidth: Int = 1, missQueueDepth: Int = 4) extends Module {
  val io = IO(new SharedCacheReplacementIO(nWays, nSets, nCores, dataWidth, missQueueDepth))

  val schedulerDataWidth = dataWidth
}
