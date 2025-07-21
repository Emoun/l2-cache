package caches.hardware.pipelined.cache


import chisel3._
import chisel3.util._
import caches.hardware.reppol._

class SharedPipelinedCacheTestTop(
                                    sizeInBytes: Int,
                                    nWays: Int,
                                    nCores: Int,
                                    reqIdWidth: Int,
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
    val cache = new CacheIO(nCores, reqIdWidth: Int, addressWidth, bytesPerSubBlock * 8)
  })

  val l2Cache = Module(new SharedPipelinedCacheTop(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    reqIdWidth = reqIdWidth,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    bytesPerBurst = bytesPerBurst,
    l2RepPolicy = l2RepPolicy
  ))

  // The dummy memory is sub-block addressable
  val memory = Module(new DummyMemory(addressWidth - log2Ceil(bytesPerBurst), bytesPerBlock * 8, bytesPerBurst * 8, dataFile))

  l2Cache.io.scheduler <> io.scheduler
  l2Cache.io.cache <> io.cache

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