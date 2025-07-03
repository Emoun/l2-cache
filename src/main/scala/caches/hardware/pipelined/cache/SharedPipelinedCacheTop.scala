package caches.hardware.pipelined.cache

import chisel3._
import caches.hardware.reppol._

class SharedPipelinedCacheTop(
                         sizeInBytes: Int,
                         nWays: Int,
                         nCores: Int,
                         addressWidth: Int,
                         bytesPerBlock: Int,
                         bytesPerSubBlock: Int,
                         bytesPerBurst: Int,
                         l2RepPolicy: () => SharedCacheReplacementPolicyType,
                       ) extends Module {

  val io = IO(new Bundle {
    val cache = new CacheIO(nCores, addressWidth, bytesPerSubBlock * 8)
    val mem = new MemoryControllerIO(addressWidth, bytesPerBurst * 8)
    val scheduler = new SchedulerIO(nCores)
  })

  val pipelineCache = Module(new SharedPipelinedCache(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    bytesPerBurst = bytesPerBurst
  ))

  val replacementPolicy = Module(l2RepPolicy())

  pipelineCache.io.repPol <> replacementPolicy.io.control
  pipelineCache.io.memController <> io.mem
  pipelineCache.io.cache <> io.cache

  replacementPolicy.io.scheduler <> io.scheduler
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
