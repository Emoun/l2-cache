package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import caches.hardware.reppol._
import caches.hardware.util.DebounceSw

/**
 * Top level module for the shared pipelined cache on the DE2-115 board.
 */
class SharedPipelinedCacheDe2115Top(
                                   sizeInBytes: Int,
                                   nWays: Int,
                                   nCores: Int,
                                   reqIdWidth: Int,
                                   addressWidth: Int,
                                   bytesPerBlock: Int,
                                   bytesPerSubBlock: Int,
                                   memBeatSize: Int,
                                   memBurstLen: Int,
                                   freq: Int,
                                   uartBaud: Int,
                                   l2RepPolicy: () => SharedCacheReplacementPolicyType,
                                   dataFile: Option[String] = None
                                 ) extends Module {
  require(isPow2(memBeatSize), "Bytes per burst need to be a power of 2.")

  val cacheReqCtrl = Module(new CacheRequestController(nCores, addressWidth, reqIdWidth, bytesPerSubBlock, freq, uartBaud))

  val l2CacheTop = Module(new SharedPipelinedCacheTestTop(
    sizeInBytes = sizeInBytes,
    nWays = nWays,
    nCores = nCores,
    reqIdWidth = reqIdWidth,
    addressWidth = addressWidth,
    bytesPerBlock = bytesPerBlock,
    bytesPerSubBlock = bytesPerSubBlock,
    memBeatSize = memBeatSize,
    memBurstLen = memBurstLen,
    l2RepPolicyGen = l2RepPolicy
  ))

  val debSchAddr = Module(new DebounceSw(log2Up(nCores), freq))
  val debSchWData = Module(new DebounceSw(l2CacheTop.l2Cache.schedulerDataWidth, freq))
  val debSchCmd = Module(new DebounceSw(SchedulerCmd.schedulerCmdWidth, freq))

  val io = IO(new Bundle {
    val rxd = Input(UInt(1.W))
    val txd = Output(UInt(1.W))
    val scheduler = new SchedulerControlIO(nCores, l2CacheTop.l2Cache.schedulerDataWidth)
  })

  // Debounced signals into the scheduler
  debSchCmd.io.sw := io.scheduler.cmd
  debSchAddr.io.sw := io.scheduler.addr
  debSchWData.io.sw := io.scheduler.wData

  // Request controller connection
  l2CacheTop.io.requests <> cacheReqCtrl.io.cache

  // Scheduler connection
  l2CacheTop.io.scheduler.cmd := debSchCmd.io.swDb
  l2CacheTop.io.scheduler.addr := debSchAddr.io.swDb
  l2CacheTop.io.scheduler.wData := debSchWData.io.swDb

  // Request scheduler connection to rxd and txd lines
  io.rxd <> cacheReqCtrl.io.rxd
  io.txd <> cacheReqCtrl.io.txd
}

object SharedPipelinedCacheDe2115Top extends App {
  val freq = 50000000
  val uartBaud = 115200

  val l2Size = 131072 // 128 KiB
  val l2Ways = 8
  val nCores = 4
  val reqIdWidth = 2
  val addressWidth = 15
  val l2BytesPerBlock = 64
  val l2BytesPerSubBlock = 16
  val memBeatSize = 2
  val memBurstLen = 4

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)

  val plruL2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)
  val contL2RepPolicy = () => new ContentionReplacementPolicy(l2Ways, l2nSets, nCores, plruL2RepPolicy)

  println("Generating the L2 cache hardware for the DE2-115 board...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheDe2115Top(
      sizeInBytes = l2Size,
      nWays = l2Ways,
      nCores = nCores,
      addressWidth = addressWidth,
      reqIdWidth = reqIdWidth,
      bytesPerBlock = l2BytesPerBlock,
      bytesPerSubBlock = l2BytesPerSubBlock,
      memBeatSize = memBeatSize,
      memBurstLen = memBurstLen,
      freq = freq,
      uartBaud = uartBaud,
      l2RepPolicy = plruL2RepPolicy,
      dataFile = Some("./test_mem_32w.hex")
    ),
    Array("--target-dir", "generated")
  )
}