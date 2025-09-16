package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import caches.hardware.reppol._
import caches.hardware.pipelined.stages.{Dec, Read, Rep, Tag, UpdateUnit}

class CacheRequestIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Flipped(Decoupled(UInt(reqIdWidth.W)))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool()) // 0 - Read, 1 - Write
  val byteEn = Input(UInt((dataWidth / 8).W))
  val wData = Input(UInt(dataWidth.W))
}

class CacheResponseIO(dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Valid(UInt(reqIdWidth.W))
  val rData = Output(UInt(dataWidth.W))
}

class CacheCorePortIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val req = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
  val resp = new CacheResponseIO(dataWidth, reqIdWidth)
}

/**
 * @param memBeatSize beat size in bytes
 * @param memBurstLen number of beats in a burst
 */
class SharedPipelinedCache(
                            sizeInBytes: Int,
                            nWays: Int,
                            nCores: Int,
                            reqIdWidth: Int,
                            addressWidth: Int,
                            bytesPerBlock: Int,
                            bytesPerSubBlock: Int,
                            memBeatSize: Int,
                            memBurstLen: Int,
                            l2RepPolicy: () => SharedCacheReplacementPolicyType
                          ) extends Module {
  require(isPow2(memBeatSize), "Number of bytes per beat must be a power of 2.")
  require(isPow2(bytesPerBlock), "Number of bytes per block must be a power of 2.")
  require(isPow2(bytesPerBlock / bytesPerSubBlock), "The remainder of bytes per block divided by bytes per sub-block must be a power of 2.")

  private val nSets = sizeInBytes / (nWays * bytesPerBlock)
  private val subBlocksPerBlock = bytesPerBlock / bytesPerSubBlock
  private val byteOffsetWidth = log2Up(bytesPerSubBlock)
  private val blockOffsetWidth = log2Up(subBlocksPerBlock)
  private val indexWidth = log2Up(nSets)
  private val tagWidth = addressWidth - indexWidth - blockOffsetWidth - byteOffsetWidth
  private val nMshrs = nWays / 2 // nMshrs = nWays for now
  private val nHalfMissCmds = 8 // Number of half miss commands

  val missQueue = Module(new MissFifo(nCores, nHalfMissCmds, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, bytesPerSubBlock * 8, bytesPerBlock * 8))
  val wbQueue = Module(new WriteBackFifo(nMshrs, tagWidth, indexWidth, bytesPerBlock * 8))
  val updateLogic = Module(new UpdateUnit(nCores, nWays, reqIdWidth, tagWidth, indexWidth, bytesPerBlock * 8, bytesPerSubBlock * 8))
  val repPol = Module(l2RepPolicy())

  val schedulerDataWidth = repPol.schedulerDataWidth
  val l2CacheBytesPerSubBlock = bytesPerSubBlock

  val invalidateLine = WireDefault(false.B)
  val invalidateWay = WireDefault(0.U(log2Up(nWays).W))
  val invalidateIndex = WireDefault(0.U(indexWidth.W))
  val missFifoCmdCapacity = WireDefault(false.B)

  println(
    s"L2 Cache Configuration: " +
      s"Size = $sizeInBytes bytes, " +
      s"Replacement policy = ${repPol.getClass.getSimpleName}, " +
      s"Associativity = $nWays, " +
      s"Block Size = $bytesPerBlock bytes, " +
      s"Sub-block Size = $bytesPerSubBlock bytes, " +
      s"Memory Beat Size = $memBeatSize bytes, " +
      s"Memory Burst Length = $memBurstLen beats, " +
      s"Number of Cores = $nCores" + "\n"
  )

  val io = IO(new Bundle{
    val core = new CacheCorePortIO(addressWidth, bytesPerSubBlock * 8, reqIdWidth)
    val inCoreId = Input(UInt(log2Up(nCores).W))
    val outCoreId = Output(UInt(log2Up(nCores).W))
    val mem = new CacheMemoryControllerIO(addressWidth, memBeatSize)
    val scheduler = new SchedulerControlIO(nCores, schedulerDataWidth)
  })

  val rejectionQueue = Module(new RejectionQueue(nCores = nCores, addrWidth = addressWidth, dataWidth = bytesPerSubBlock * 8, reqIdWidth = reqIdWidth, depth = nCores))
  val coreReqMux = Module(new CoreReqMux(nCores = nCores, addrWidth = addressWidth, dataWidth = bytesPerSubBlock * 8, reqIdWidth = reqIdWidth))

  val pipeStall = updateLogic.io.stall || missQueue.io.full // || missFifoCmdCapacity
  val reqAccept = !pipeStall

  // Connect core request and rejection queue to the core request multiplexer that feeds into the cache pipeline
  coreReqMux.io.req1 <> io.core.req
  coreReqMux.io.req2 <> rejectionQueue.io.popEntry
  coreReqMux.io.req1CoreID := io.inCoreId
  coreReqMux.io.req2CoreID := rejectionQueue.io.popCoreId
  coreReqMux.io.out.reqId.ready := reqAccept

  // Connect replacement policy with the rejection queue
  rejectionQueue.io.popRejQueue <> repPol.io.control.popRejQueue
  repPol.io.scheduler <> io.scheduler

  // ---------------- Decode ----------------
  val decLogic = Module(new Dec(nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, byteOffWidth = byteOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  decLogic.io.stall := pipeStall
  decLogic.io.dec.coreId := coreReqMux.io.outCoreID
  decLogic.io.dec.reqValid := coreReqMux.io.out.reqId.valid
  decLogic.io.dec.reqId := coreReqMux.io.out.reqId.bits
  decLogic.io.dec.reqRw := coreReqMux.io.out.rw
  decLogic.io.dec.addr := coreReqMux.io.out.addr
  decLogic.io.dec.wData := coreReqMux.io.out.wData
  decLogic.io.dec.byteEn := coreReqMux.io.out.byteEn

  // ---------------- Tag and Dirty Lookup ----------------

  val tagLogic = Module(new Tag(nCores = nCores, nSets = nSets, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  tagLogic.io.stall := pipeStall
  tagLogic.io.tag <> decLogic.io.tag
  tagLogic.io.tagCtrl <> updateLogic.io.tagUpdate
  tagLogic.io.invalidate.invalidate := invalidateLine
  tagLogic.io.invalidate.way := invalidateWay
  tagLogic.io.invalidate.index := invalidateIndex
  tagLogic.io.setValidLine := updateLogic.io.setValidLine

  // ---------------- Replacement ----------------

  val repLogic = Module(new Rep(nCores = nCores, nSets = nSets, nWays = nWays, nMshrs = nMshrs, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, blockWidth = bytesPerBlock * 8, subBlockWidth = bytesPerSubBlock * 8))
  repLogic.io.stall := pipeStall
  repLogic.io.rep <> tagLogic.io.rep
  repLogic.io.missFifoPush <> missQueue.io.push
  repLogic.io.missCritInfo <> missQueue.io.critInfo
  repLogic.io.missNonCritInfo <> missQueue.io.nonCritInfo
  missQueue.io.isCrit := repLogic.io.isMissPushCrit
  repLogic.io.repPol <> repPol.io.control
  repLogic.io.setLineValid := updateLogic.io.setValidLine
  invalidateLine := repLogic.io.invalidate.invalidate
  invalidateWay := repLogic.io.invalidate.way
  invalidateIndex := repLogic.io.invalidate.index
  missFifoCmdCapacity := repLogic.io.halfMissCapacity
  rejectionQueue.io.push := repLogic.io.pushReject
  rejectionQueue.io.pushEntry := repLogic.io.pushRejectEntry

  // ---------------- Read ----------------

  val readLogic = Module(new Read(memSizeInBytes = sizeInBytes, nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, blockWidth = bytesPerBlock * 8, subBlockWidth = bytesPerSubBlock * 8))
  readLogic.io.stall := pipeStall
  readLogic.io.read <> repLogic.io.read
  readLogic.io.wbQueue <> wbQueue.io.push
  readLogic.io.memUpdate <> updateLogic.io.memUpdate
  readLogic.io.dirtyCtrl <> tagLogic.io.dirtyCtrl

  // ---------------- Update ----------------

  val memInterface = Module(new MemoryInterface(nCores, nWays, nHalfMissCmds, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, bytesPerBlock * 8, bytesPerSubBlock * 8, beatSize = memBeatSize, burstLen = memBurstLen))
  memInterface.io.missFifo <> missQueue.io.pop
  memInterface.io.wbFifo <> wbQueue.io.pop
  memInterface.io.memController <> io.mem
  missQueue.io.memIntIdle := memInterface.io.idle

  updateLogic.io.readStage <> readLogic.io.update
  updateLogic.io.memoryInterface <> memInterface.io.updateLogic
  updateLogic.io.pipeStall := pipeStall
  io.core.resp <> updateLogic.io.coreResp
  io.outCoreId := updateLogic.io.outCoreId
}
