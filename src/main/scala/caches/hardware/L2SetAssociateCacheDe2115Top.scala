package caches.hardware

import caches.hardware.reppol._
import caches.hardware.util.Constants._
import chisel3._


class L2CacheDe2115IO(addressWidth: Int, coreBytesPerWord: Int, nCores: Int) extends Bundle {
  val scheduler = new SchedulerIO(nCores)
  val higher = new SharedCacheIO(addressWidth, coreBytesPerWord * 8, nCores)
  val lower = new MemoryControllerIO(addressWidth, 16)
}

class L2SetAssociateCacheDe2115Top(
                                    l2Size: Int,
                                    l2Ways: Int,
                                    l2BytesPerBlock: Int,
                                    l2BytesPerWord: Int,
                                    coreBytesPerWord: Int,
                                    nCores: Int,
                                    addressWidth: Int,
                                    l2RepPolicy: () => SharedCacheReplacementPolicyType,
                                    l1Cache: () => L2SetAssociateCache
                                  ) extends Module {

  val io = IO(new L2CacheDe2115IO(addressWidth, coreBytesPerWord, 1))

  val l1 = Module(l1Cache())

  val mem = Module(new DummyMemoryController(
    addressWidth = addressWidth,
    blockSize = l2BytesPerBlock,
    burstSize = l2BytesPerWord
  ))

  val l2Cache = Module(new L2SetAssociateCache(
    size = l2Size,
    ways = l2Ways,
    bytesPerBlock = l2BytesPerBlock,
    bytesPerWord = l2BytesPerWord,
    nCores = nCores,
    addressWidth = addressWidth,
    repPolicy = l2RepPolicy
  ))

  l1.io.higher <> io.higher

  // Connection between scheduler and the L2
  io.scheduler <> l2Cache.io.scheduler

  // Empty assignments to l1 scheduler
  l1.io.scheduler.setCritical.valid := false.B
  l1.io.scheduler.setCritical.bits := 0.U
  l1.io.scheduler.unsetCritical.valid := false.B
  l1.io.scheduler.unsetCritical.bits := 0.U
  l1.io.scheduler.contentionLimit := 0.U

  // Connection between the L1 cache and the L2 cache
  // TODO: Registers here to isolate the critical path of the l2 cache
  l2Cache.io.higher.req := RegNext(l1.io.lower.req)
  l2Cache.io.higher.reqId := RegNext(l1.io.lower.reqId)
  l2Cache.io.higher.addr := RegNext(l1.io.lower.addr)
  l2Cache.io.higher.rw := RegNext(l1.io.lower.rw)
  l2Cache.io.higher.wData := RegNext(l1.io.lower.wData)
  l2Cache.io.higher.wMask := RegNext(l1.io.lower.wMask)
  l1.io.lower.ack := RegNext(l2Cache.io.higher.ack)
  l1.io.lower.responseStatus := RegNext(l2Cache.io.higher.responseStatus)
  l1.io.lower.rData := RegNext(l2Cache.io.higher.rData)

  // Connection between the memory and the cache
  l2Cache.io.lower.ack := RegNext(mem.io.cache.ack)
  l2Cache.io.lower.rData := RegNext(mem.io.cache.rData)
  l2Cache.io.lower.responseStatus := RegNext(mem.io.cache.responseStatus)

  mem.io.cache.req := RegNext(l2Cache.io.lower.req)
  mem.io.cache.reqId := RegNext(l2Cache.io.lower.reqId)
  mem.io.cache.addr := RegNext(l2Cache.io.lower.addr)
  mem.io.cache.rw := RegNext(l2Cache.io.lower.rw)
  mem.io.cache.wData := RegNext(l2Cache.io.lower.wData)
  mem.io.cache.wMask := RegNext(l2Cache.io.lower.wMask)

  // Connection between mem controller and memory
  io.lower <> mem.io.mem
}


object L2SetAssociateCacheDe2115Top extends App {
  val l2Size = 8192 // in bytes
  val l2Ways = 8
  val l2BytesPerBlock = 64
  val l2BytesPerWord = 16
  val l2nCores = 8
  val l2nSets = (l2Size / l2BytesPerBlock) / l2Ways
  val l2BasePolicy = () => new BitPlruReplacementAlgorithm(l2Ways)
  val l2RepPolicy = () => new ContentionReplacementPolicy(l2Ways, l2nSets, l2nCores, l2BasePolicy)
//  val l2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, l2nCores)

  val l1BytesPerWord = 4
  val l1Cache = () => new L2SetAssociateCache(
    size = 1024,
    ways = 4,
    bytesPerBlock = 16,
    bytesPerWord = l1BytesPerWord,
    nCores = 1,
    addressWidth = ADDRESS_WIDTH,
    repPolicy = () => new BitPlruReplacementPolicy(4, 16, l2nCores)
  )

  println(s"Generating hardware for L2 Cache and its surrounding environment (DE2-115 board)...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new L2SetAssociateCacheDe2115Top(
      l2Size = l2Size,
      l2Ways = l2Ways,
      l2BytesPerBlock = l2BytesPerBlock,
      l2BytesPerWord = l2BytesPerWord,
      coreBytesPerWord = l1BytesPerWord,
      nCores = l2nCores,
      addressWidth = ADDRESS_WIDTH,
      l2RepPolicy = l2RepPolicy,
      l1Cache = l1Cache
    ), Array("--target-dir", "generated", "--no-dedup"))
}