package caches.hardware.pipelined.cache

import ocp._
import chisel3._
import caches.hardware.reppol._

class OcpCacheWrapper(
                       nCores: Int,
                       addrWidth: Int,
                       coreDataWidth: Int,
                       coreBurstLen: Int,
                       memDataWidth: Int,
                       memBurstLen: Int,
                       l2Cache: () => SharedPipelinedCacheTop
                     ) extends Module {
  val io = IO(new Bundle {
    val cores = Vec(nCores, new OcpBurstSlavePort(addrWidth, coreDataWidth, coreBurstLen))
    val scheduler = new SchedulerIO(nCores)
    val mem = new OcpBurstMasterPort(addrWidth, memDataWidth, memBurstLen)
  })

  val ocpSlaveAdapters = Array.fill(nCores) {
    Module(new OcpBurstSlaveToCacheAdapter(addrWidth, coreDataWidth, coreBurstLen))
  }
  val ocpMasterAdapter = Module(new CacheToOcpBurstMasterAdapter(addrWidth, memDataWidth, memBurstLen))

  val cache = Module(l2Cache())
  cache.io.mem <> ocpMasterAdapter.io.cache
  cache.io.scheduler <> io.scheduler

  for (i <- ocpSlaveAdapters.indices) {
    ocpSlaveAdapters(i).io.ocpBurst <> io.cores(i)
    ocpSlaveAdapters(i).io.corePort <> cache.io.cache.cores(i)
  }

  io.mem <> ocpMasterAdapter.io.ocpBurst
}

object OcpCacheWrapper extends App {
  //  val l2Size = 524288 // 256 KiB
  val l2Size = 131072 // 128 KiB
  //  val l2Size = 16384 // 16 KiB
  val l2Ways = 8
  val nCores = 4
  val reqIdWidth = 6
  val addressWidth = 32
  val coreCacheAccessBurstLen = 4
  val l2BytesPerSubBlock = 16
  val l2BytesPerBlock = 64
  val cacheMemAccessBurstLen = 8

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)

  val plruL2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)
  val contL2RepPolicy = () => new ContentionReplacementPolicy(l2Ways, l2nSets, nCores, plruL2RepPolicy)

  val l2Cache = () => new SharedPipelinedCacheTop(
    sizeInBytes = l2Size,
    nWays = l2Ways,
    nCores = nCores,
    addressWidth = addressWidth,
    reqIdWidth = reqIdWidth,
    bytesPerBlock = l2BytesPerBlock,
    bytesPerSubBlock = l2BytesPerSubBlock,
    bytesPerBurst = l2BytesPerBlock / cacheMemAccessBurstLen,
    l2RepPolicy = contL2RepPolicy
  )

  println("Generating the L2 cache hardware with the OCP interface...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new OcpCacheWrapper(
      nCores = nCores,
      addrWidth = addressWidth,
      coreDataWidth = (l2BytesPerSubBlock / coreCacheAccessBurstLen) * 8,
      coreBurstLen = coreCacheAccessBurstLen,
      memDataWidth = (l2BytesPerSubBlock / cacheMemAccessBurstLen) * 8,
      memBurstLen = cacheMemAccessBurstLen,
      l2Cache = l2Cache
    ),
    Array("--target-dir", "generated")
  )
}
