package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._
import caches.hardware.reppol._

class SharedPipelinedCacheTopDe2115 (
                               sizeInBytes: Int,
                               nWays: Int,
                               nCores: Int,
                               reqIdWidth: Int,
                               addressWidth: Int,
                               bytesPerBlock: Int,
                               bytesPerSubBlock: Int,
                               bytesPerBurst: Int,
                               freq: Int,
                               uartBaud: Int,
                               l2RepPolicy: () => SharedCacheReplacementPolicyType
                             ) extends Module {
  val io = IO(new Bundle {
    val rxd = Input(UInt(1.W))
    val txd = Output(UInt(1.W))
    val scheduler = new SchedulerIO(nCores)
    val mem = new MemoryControllerIO(addressWidth, bytesPerBurst * 8)
  })

  def negEdge(din: Bool): Bool = !din & RegNext(din)

  val debSchCoreId = Module(new DebounceSw(log2Up(nCores), freq))
  val debSchContLimit = Module(new DebounceSw(log2Up(nCores), freq))
  val debSetCrit = Module(new DebounceSw(1, freq))
  val debUnsetCrit = Module(new DebounceSw(1, freq))

  val cacheReqCtrl = Module(new CacheRequestController(nCores, addressWidth, reqIdWidth, bytesPerSubBlock, freq, uartBaud))

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

  debSchCoreId.io.sw := io.scheduler.coreId.bits
  debSchContLimit.io.sw := io.scheduler.contentionLimit
  debSetCrit.io.sw := io.scheduler.setCritical.asUInt
  debUnsetCrit.io.sw := io.scheduler.unsetCritical.asUInt

  l2Cache.io.scheduler.coreId.valid := negEdge(!io.scheduler.coreId.valid)
  l2Cache.io.scheduler.coreId.bits := debSchCoreId.io.swDb
  l2Cache.io.scheduler.setCritical := debSetCrit.io.swDb
  l2Cache.io.scheduler.unsetCritical := debUnsetCrit.io.swDb
  l2Cache.io.scheduler.contentionLimit := debSchContLimit.io.swDb

  l2Cache.io.cache <> cacheReqCtrl.io.cache
  l2Cache.io.mem <> io.mem

  io.rxd <> cacheReqCtrl.io.rxd
  io.txd <> cacheReqCtrl.io.txd
}

object SharedPipelinedCacheTopDe2115 extends App {
  val useLru = false

  val freq = 50000000
  val uartBaud = 115200

  val l2Size = 524288 // 256 KiB
//    val l2Size = 131072 // 128 KiB
  //  val l2Size = 16384 // 16 KiB
  val l2Ways = 8
  val nCores = 4
  val reqIdWidth = 6
  val addressWidth = 32
  val l2BytesPerBlock = 64
  val l2BytesPerSubBlock = 16
  val l2BytesPerMemBurst = 1

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)

  val plruL2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)
  val contL2RepPolicy = () => new ContentionReplacementPolicy(l2Ways, l2nSets, nCores, plruL2RepPolicy)

  println("Generating the L2 cache hardware for the DE2-115 board...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheTopDe2115(
      sizeInBytes = l2Size,
      nWays = l2Ways,
      nCores = nCores,
      addressWidth = addressWidth,
      reqIdWidth = reqIdWidth,
      bytesPerBlock = l2BytesPerBlock,
      bytesPerSubBlock = l2BytesPerSubBlock,
      bytesPerBurst = l2BytesPerMemBurst,
      freq = freq,
      uartBaud = uartBaud,
      l2RepPolicy = contL2RepPolicy
    ),
    Array("--target-dir", "generated")
  )
}