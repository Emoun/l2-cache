package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._
import caches.hardware.reppol._

class SharedPipelinedCacheTopDe2115 (
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

  val io = IO(new Bundle {
    val scheduler = new SchedulerIO(nCores)
    val cache = new CacheIO(nCores, addressWidth, bytesPerSubBlock * 8)
  })

  val l2Cache = Module(new SharedPipelinedCacheTop(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    bytesPerBurst = bytesPerBurst,
    l2RepPolicy = l2RepPolicy,
    dataFile = dataFile
  ))

  io.scheduler <> l2Cache.io.scheduler
  io.cache <> l2Cache.io.cache
}

object SharedPipelinedCacheTopDe2115 extends App {
  val useLru = false

  val l2Size = 262144
  val l2Ways = 8
  val nCores = 4
  val addressWidth = 32
  val l2BytesPerBlock = 64
  val l2BytesPerSubBlock = 16
  val l2BytesPerMemBurst = 4

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)
  val l2BasePolicy = () => new BitPlruReplacementAlgorithm(l2Ways)

  val plruL2RepPolicy = new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)
  val contL2RepPolicy = () => new ContentionReplacementPolicy(l2Ways, l2nSets, nCores, l2BasePolicy)

  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheTopDe2115(
      sizeInBytes = l2Size,
      nWays = l2Ways,
      nCores = nCores,
      addressWidth = addressWidth,
      bytesPerBlock = l2BytesPerBlock,
      bytesPerSubBlock = l2BytesPerSubBlock,
      bytesPerBurst = l2BytesPerMemBurst,
      l2RepPolicy = contL2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}