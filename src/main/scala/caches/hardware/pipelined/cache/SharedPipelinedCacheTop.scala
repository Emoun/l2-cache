package caches.hardware.pipelined.cache

import chisel3._
import caches.hardware.reppol._
import chisel3.util._

class SharedPipelinedCacheTop(
                         sizeInBytes: Int,
                         nWays: Int,
                         nCores: Int,
                         addressWidth: Int,
                         bytesPerBlock: Int,
                         bytesPerSubBlock: Int,
                         bytesPerBurst: Int,
                         l2RepPolicy: () => SharedCacheReplacementPolicyType,
                         dataFile: Option[String] = None
                       ) extends Module {
  require(isPow2(bytesPerBurst), "Bytes per burst need to be a power of 2.")

  val io = IO(new Bundle {
    val scheduler = new SchedulerIO(nCores)
    val cache = new CacheIO(nCores, addressWidth, bytesPerSubBlock * 8)
  })

  // TODO: Add some sort of queue for rejected responses, to retry them at a later time
  //  or expect the core re-attempt at a later time
  val l2Cache = Module(new SharedPipelinedCache(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    bytesPerBurst = bytesPerBurst
  ))

  val replacementPolicy = Module(l2RepPolicy())

  // The dummy memory is sub-block addressable
  val memory = Module(new DummyMemory(addressWidth - log2Ceil(bytesPerBurst), bytesPerBlock * 8, bytesPerBurst * 8, dataFile))

  l2Cache.io.repPol <> replacementPolicy.io.control
  l2Cache.io.cache <> io.cache
  replacementPolicy.io.scheduler := io.scheduler

  memory.io.rChannel.rAddr.valid := l2Cache.io.mem.rChannel.rAddr.valid
  memory.io.rChannel.rAddr.bits := l2Cache.io.mem.rChannel.rAddr.bits(addressWidth - 1, log2Ceil(bytesPerBurst)) // Dummy memory accepts the address bits for bursts addresses only
  l2Cache.io.mem.rChannel.rAddr.ready := memory.io.rChannel.rAddr.ready

  memory.io.wChannel.wAddr.valid := l2Cache.io.mem.wChannel.wAddr.valid
  memory.io.wChannel.wAddr.bits := l2Cache.io.mem.wChannel.wAddr.bits(addressWidth - 1, log2Ceil(bytesPerBurst)) // Dummy memory accepts the address bits for bursts addresses only
  l2Cache.io.mem.wChannel.wAddr.ready := memory.io.wChannel.wAddr.ready

  memory.io.rChannel.rData <> l2Cache.io.mem.rChannel.rData
  memory.io.rChannel.rLast <> l2Cache.io.mem.rChannel.rLast
  memory.io.wChannel.wData <> l2Cache.io.mem.wChannel.wData
  memory.io.wChannel.wLast <> l2Cache.io.mem.wChannel.wLast
}

object SharedPipelinedCacheTop extends App {
  val l2Size = 262144
  val l2Ways = 8
  val nCores = 4
  val addressWidth = 32
  val l2BytesPerBlock = 64
  val l2BytesPerSubBlock = 16
  val l2BytesPerMemBurst = 4

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)
  val l2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)

  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheTop(
      sizeInBytes = l2Size,
      nWays = l2Ways,
      nCores = nCores,
      addressWidth = addressWidth,
      bytesPerBlock = l2BytesPerBlock,
      bytesPerSubBlock = l2BytesPerSubBlock,
      bytesPerBurst = l2BytesPerMemBurst,
      l2RepPolicy = l2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}
