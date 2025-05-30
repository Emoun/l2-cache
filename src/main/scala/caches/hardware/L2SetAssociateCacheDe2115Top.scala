package caches.hardware

import caches.hardware.reppol._
import caches.hardware.util.Constants._
import chisel3._


class dummyCoreIO(addrWidth: Int, dataWidth: Int, nCores: Int) extends SharedCacheIO(addrWidth, dataWidth, nCores) {
  override val rData = Output(UInt(1.W))
  override val wMask = Input(UInt(1.W))
}

class L2CacheDe2115IO(addressWidth: Int, nCores: Int, bytesPerWord: Int, bytesPerBlock: Int) extends Bundle {
  val scheduler = new SchedulerIO(nCores)
  val higher = new dummyCoreIO(addressWidth, bytesPerWord * 8, nCores)
  val lower = new MemoryControllerIO(addressWidth, 16)
}

class L2SetAssociateCacheDe2115Top(
                                    size: Int,
                                    ways: Int,
                                    bytesPerBlock: Int,
                                    bytesPerWord: Int,
                                    nCores: Int,
                                    addressWidth: Int,
                                    repPolicy: () => SharedCacheReplacementPolicyType
                                  ) extends Module {

  val io = IO(new L2CacheDe2115IO(addressWidth, nCores, bytesPerWord, bytesPerBlock))

  // TODO: Add a smaller cache to act as core

  val mem = Module(new DummyMemoryController(
    addressWidth = addressWidth,
    blockSize = bytesPerBlock,
    burstSize = bytesPerWord
  ))

  val l2Cache = Module(new L2SetAssociateCache(
    size = size,
    ways = ways,
    bytesPerBlock = bytesPerBlock,
    bytesPerWord = bytesPerWord,
    nCores = nCores,
    addressWidth = addressWidth,
    repPolicy = repPolicy
  ))

  io.scheduler <> l2Cache.io.scheduler

  // Connection to a core/arbiter
  l2Cache.io.higher.req := io.higher.req
  l2Cache.io.higher.reqId := io.higher.reqId
  l2Cache.io.higher.addr := io.higher.addr
  l2Cache.io.higher.rw := io.higher.rw
  l2Cache.io.higher.wData := io.higher.wData
  l2Cache.io.higher.wMask := io.higher.wMask.orR

  io.higher.rData := l2Cache.io.higher.rData.orR
  io.higher.ack := l2Cache.io.higher.ack
  io.higher.responseStatus := l2Cache.io.higher.responseStatus

  // Connection between the memory and the cache
  l2Cache.io.lower.ack := mem.io.cache.ack
  l2Cache.io.lower.rData := mem.io.cache.rData
  l2Cache.io.lower.responseStatus := mem.io.cache.responseStatus

  mem.io.cache.req := l2Cache.io.lower.req
  mem.io.cache.reqId := l2Cache.io.lower.reqId
  mem.io.cache.addr := l2Cache.io.lower.addr
  mem.io.cache.rw := l2Cache.io.lower.rw
  mem.io.cache.wData := l2Cache.io.lower.wData
  mem.io.cache.wMask := l2Cache.io.lower.wMask

  // Connection between mem controller and memory
  io.lower <> mem.io.mem
}


object L2SetAssociateCacheDe2115Top extends App {
  val size = 8192 // in bytes
  val ways = 8
  val bytesPerBlock = 64
  val bytesPerWord = 16
  val nCores = 8
  val nSets = (size / bytesPerBlock) / ways
  val basePolicy = () => new BitPlruReplacementAlgorithm(ways)
  //  val repPolicy = () => new ContentionReplacementPolicy(ways, nSets, nCores, L2_MISS_LATENCY, basePolicy)
  val repPolicy = () => new BitPlruReplacementPolicy(ways, nSets, nCores)

  println(s"Generating hardware for L2 Cache and its surrounding environment (DE2-115 board)...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new L2SetAssociateCacheDe2115Top(
      size = size,
      ways = ways,
      bytesPerBlock = bytesPerBlock,
      bytesPerWord = bytesPerWord,
      nCores = nCores,
      addressWidth = ADDRESS_WIDTH,
      repPolicy = repPolicy
    ), Array("--target-dir", "generated", "--no-dedup"))
}