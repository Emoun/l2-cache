package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._
import caches.hardware.reppol._

/**
 * Test module with a test memory implemented as block RAM.
 */
class SharedPipelinedCacheTestTopDe2115(
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
                                   l2RepPolicy: () => SharedCacheReplacementPolicyType,
                                   dataFile: Option[String] = None
                                 ) extends Module {
  require(isPow2(bytesPerBurst), "Bytes per burst need to be a power of 2.")

  val io = IO(new Bundle {
    val rxd = Input(UInt(1.W))
    val txd = Output(UInt(1.W))
    val scheduler = new SchedulerIO(nCores)
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

  // The dummy memory is sub-block addressable
  val memory = Module(new DummyMemory(addressWidth - log2Ceil(bytesPerBurst), bytesPerBlock * 8, bytesPerBurst * 8, dataFile))

  // Debounced signals into the scheduler
  debSchCoreId.io.sw := io.scheduler.coreId.bits
  debSchContLimit.io.sw := io.scheduler.contentionLimit
  debSetCrit.io.sw := io.scheduler.setCritical.asUInt
  debUnsetCrit.io.sw := io.scheduler.unsetCritical.asUInt

  // Request controller connection
  l2Cache.io.cache <> cacheReqCtrl.io.cache

  // Scheduler connection
  l2Cache.io.scheduler.coreId.valid := negEdge(!io.scheduler.coreId.valid)
  l2Cache.io.scheduler.coreId.bits := debSchCoreId.io.swDb
  l2Cache.io.scheduler.setCritical := debSetCrit.io.swDb
  l2Cache.io.scheduler.unsetCritical := debUnsetCrit.io.swDb
  l2Cache.io.scheduler.contentionLimit := debSchContLimit.io.swDb

  // Memory read address channel
  memory.io.rChannel.rAddr.valid := l2Cache.io.mem.rChannel.rAddr.valid
  memory.io.rChannel.rAddr.bits := l2Cache.io.mem.rChannel.rAddr.bits(addressWidth - 1, log2Ceil(bytesPerBurst)) // Dummy memory accepts the address bits for bursts addresses only
  l2Cache.io.mem.rChannel.rAddr.ready := memory.io.rChannel.rAddr.ready

  // Memory write address channel
  memory.io.wChannel.wAddr.valid := l2Cache.io.mem.wChannel.wAddr.valid
  memory.io.wChannel.wAddr.bits := l2Cache.io.mem.wChannel.wAddr.bits(addressWidth - 1, log2Ceil(bytesPerBurst)) // Dummy memory accepts the address bits for bursts addresses only
  l2Cache.io.mem.wChannel.wAddr.ready := memory.io.wChannel.wAddr.ready

  // Memory read and write data channels
  memory.io.rChannel.rData <> l2Cache.io.mem.rChannel.rData
  memory.io.rChannel.rLast <> l2Cache.io.mem.rChannel.rLast
  memory.io.wChannel.wData <> l2Cache.io.mem.wChannel.wData
  memory.io.wChannel.wLast <> l2Cache.io.mem.wChannel.wLast

  // Request scheduler connection to rxd and txd lines
  io.rxd <> cacheReqCtrl.io.rxd
  io.txd <> cacheReqCtrl.io.txd
}

object SharedPipelinedCacheTestTopDe2115 extends App {
  val freq = 50000000
  val uartBaud = 115200

  //val l2Size = 524288 // 256 KiB
  //val l2Size = 131072 // 128 KiB
  val l2Size = 16384 // 16 KiB
  val l2Ways = 8
  val nCores = 4
  val reqIdWidth = 2
  val addressWidth = 15
  val l2BytesPerBlock = 64
  val l2BytesPerSubBlock = 16
  val l2BytesPerMemBurst = 2

  val l2nSets = l2Size / (l2Ways * l2BytesPerBlock)

  val plruL2RepPolicy = () => new BitPlruReplacementPolicy(l2Ways, l2nSets, nCores)
  val contL2RepPolicy = () => new ContentionReplacementPolicy(l2Ways, l2nSets, nCores, plruL2RepPolicy)

  println("Generating the L2 cache hardware for the DE2-115 board...")
  (new chisel3.stage.ChiselStage).emitVerilog(
    new SharedPipelinedCacheTestTopDe2115(
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
      l2RepPolicy = plruL2RepPolicy,
      dataFile = Some("./test_mem_32w.hex")
    ),
    Array("--target-dir", "generated")
  )
}