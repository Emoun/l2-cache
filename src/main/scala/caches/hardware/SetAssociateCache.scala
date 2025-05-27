package caches.hardware

import caches.hardware.reppol._
import caches.hardware.util.Constants.ADDRESS_WIDTH
import chisel3._
import chisel3.util._

class CacheIO(addrWidth: Int, dataWidth: Int, tokenSize: Int) extends Bundle {
  val req = Input(Bool())
  val token = Input(UInt(log2Up(tokenSize).W))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool())
  val wData = Input(UInt(dataWidth.W))
  val wMask = Input(UInt((dataWidth / 8).W))
  val rData = Output(UInt(dataWidth.W))
  val ack = Output(Bool())
  val responseStatus = Output(UInt(2.W)) // 0: Hit, 1: Miss, 2: Reject, 3: Error
}

// TODO: Rename this to be L1 cache
// TODO: Extend the interface to include access IDs and response status
class SetAssociateCache(size: Int, ways: Int, bytesPerBlock: Int, bytesPerWord: Int, repPolicy: () => ReplacementPolicyType) extends Module {
  private val nSets = (size / bytesPerBlock) / ways

  val io = IO(new Bundle {
    val higher = new CacheIO(ADDRESS_WIDTH, bytesPerWord * 8, 0)
    val lower = Flipped(new CacheIO(ADDRESS_WIDTH, bytesPerBlock * 8, 0))
  })

  val cacheControl = Module(new CacheController(ways, nSets, repPolicy))
  val cacheMem = Module(new SetAssociateCacheMemory(size, ways, nSets, bytesPerBlock, bytesPerWord))

  // Connection between the controller and the cache memory
  cacheControl.io.mem <> cacheMem.io.controller

  // Connections between higher level and the cache
  cacheControl.io.higher.req := io.higher.req
  cacheControl.io.higher.rw := io.higher.rw
  io.higher.ack := cacheControl.io.higher.ack
  io.higher.responseStatus := 0.U // TODO: Implement response status

  cacheMem.io.higher.addr := io.higher.addr
  cacheMem.io.higher.wData := io.higher.wData
  cacheMem.io.higher.wMask := io.higher.wMask
  io.higher.rData := cacheMem.io.higher.rData

  // Connections between lower level and the cache
  io.lower.req := cacheControl.io.lower.req
  io.lower.rw := cacheControl.io.lower.rw
  io.lower.token := 0.U // TODO: Implement passing of the token
  cacheControl.io.lower.ack := io.lower.ack

  io.lower.addr := cacheMem.io.lower.addr
  io.lower.wData := cacheMem.io.lower.wData
  io.lower.wMask := cacheMem.io.lower.wMask
  cacheMem.io.lower.rData := io.lower.rData
}

object SetAssociateCache extends App {
  val size = 256
  val ways = 4
  val bytesPerBlock = 8
  val bytesPerWord = 4
  val nSets = (size / bytesPerBlock) / ways

  println(s"Generating hardware for LRU Cache...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new SetAssociateCache(
      size,
      ways,
      bytesPerBlock,
      bytesPerWord,
      () => new TreePlruReplacementPolicy(ways, nSets)
    ), Array("--target-dir", "generated", "--no-dedup"))
}

