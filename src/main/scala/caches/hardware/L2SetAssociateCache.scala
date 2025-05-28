package caches.hardware

import chisel3._
import chisel3.util._
import caches.hardware.reppol.{BitPlruReplacementAlgorithm, BitPlruReplacementPolicy, ContentionReplacementPolicy, SchedulerIO, SharedCacheReplacementPolicyType, TreePlruReplacementPolicy}
import caches.hardware.util.Constants.{ADDRESS_WIDTH, L2_MISS_LATENCY}

class SharedCacheIO(addrWidth: Int, dataWidth: Int, nCores: Int) extends Bundle {
  val req = Input(Bool())
  val reqId = Input(UInt(log2Up(nCores).W))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool())
  val wData = Input(UInt(dataWidth.W))
  val wMask = Input(UInt((dataWidth / 8).W))
  val rData = Output(UInt(dataWidth.W))
  val ack = Output(Bool())
  val responseStatus = Output(UInt(1.W)) // 0: OK, 1: REJECT
}

class L2SetAssociateCache(size: Int, ways: Int, bytesPerBlock: Int, bytesPerWord: Int, nCores: Int, repPolicy: () => SharedCacheReplacementPolicyType) extends Module {
  private val nSets = (size / bytesPerBlock) / ways

  val io = IO(new Bundle {
    val scheduler = new SchedulerIO(nCores)
    val higher = new SharedCacheIO(ADDRESS_WIDTH, bytesPerWord * 8, nCores)
    val lower = Flipped(new SharedCacheIO(ADDRESS_WIDTH, bytesPerBlock * 8, nCores))
  })

  val repPol = Module(repPolicy())
  val cacheController = Module(new CacheController(ways, nSets, nCores))
  val cacheMem = Module(new SetAssociateCacheMemory(size, ways, nSets, bytesPerBlock, bytesPerWord))

  // Connection between the controller and the cache memory
  cacheController.io.mem <> cacheMem.io.controller

  // Connection between the controller and the replacement policy
  repPol.io.control <> cacheController.io.repPol

  // Connection between the scheduler and the replacement policy
  repPol.io.scheduler <> io.scheduler

  // Connections between higher level and the cache
  cacheController.io.higher.req := io.higher.req
  cacheController.io.higher.rw := io.higher.rw
  cacheController.io.higher.reqId := io.higher.reqId
  io.higher.ack := cacheController.io.higher.ack
  io.higher.responseStatus := cacheController.io.higher.status

  cacheMem.io.higher.addr := io.higher.addr
  cacheMem.io.higher.wData := io.higher.wData
  cacheMem.io.higher.wMask := io.higher.wMask
  io.higher.rData := cacheMem.io.higher.rData

  // Connections between lower level and the cache
  io.lower.req := cacheController.io.lower.req
  io.lower.reqId := cacheController.io.lower.reqId
  io.lower.rw := cacheController.io.lower.rw
  cacheController.io.lower.ack := io.lower.ack
  cacheController.io.lower.status := io.lower.responseStatus

  io.lower.addr := cacheMem.io.lower.addr
  io.lower.wData := cacheMem.io.lower.wData
  io.lower.wMask := cacheMem.io.lower.wMask
  cacheMem.io.lower.rData := io.lower.rData
}

object L2SetAssociateCache extends App {
  val size = 8192 // in bytes
  val ways = 8
  val bytesPerBlock = 64
  val bytesPerWord = 16
  val nCores = 8
  val nSets = (size / bytesPerBlock) / ways
  val basePolicy = () => new BitPlruReplacementAlgorithm(ways)

  println(s"Generating hardware for LRU Cache...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new L2SetAssociateCache(
      size = size,
      ways = ways,
      bytesPerBlock = bytesPerBlock,
      bytesPerWord = bytesPerWord,
      nCores = nCores,
      repPolicy = () => new ContentionReplacementPolicy(ways, nSets, nCores, L2_MISS_LATENCY, basePolicy)
    ), Array("--target-dir", "generated", "--no-dedup"))
}
