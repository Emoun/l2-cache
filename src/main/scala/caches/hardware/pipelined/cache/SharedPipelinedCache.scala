package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._
import caches.hardware.reppol._
import caches.hardware.pipelined.cache.stages._

class CacheRequestIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Flipped(Decoupled(UInt(reqIdWidth.W)))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool()) // 0 - Read, 1 - Write
  val wData = Input(UInt(dataWidth.W))
}

class CacheResponseIO(dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Valid(UInt(reqIdWidth.W))
  val rData = Output(UInt(dataWidth.W))
  val responseStatus = Output(UInt(1.W)) // 1 - OK, 0 - Rejected
}

class CacheCorePortIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val req = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
  val resp = new CacheResponseIO(dataWidth, reqIdWidth)
}

class CacheIO(nCores: Int, reqIdWidth: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val cores = Vec(nCores, new CacheCorePortIO(addrWidth, dataWidth, reqIdWidth))
}

class SharedPipelinedCache(sizeInBytes: Int, nWays: Int, nCores: Int, reqIdWidth: Int, addressWidth: Int, bytesPerBlock: Int, bytesPerSubBlock: Int, bytesPerBurst: Int) extends Module {
  private val nSets = sizeInBytes / (nWays * bytesPerBlock)
  private val wordsPerBlock = bytesPerBlock / bytesPerSubBlock
  private val byteOffsetWidth = log2Up(bytesPerSubBlock)
  private val blockOffsetWidth = log2Up(wordsPerBlock)
  private val indexWidth = log2Up(nSets)
  private val tagWidth = addressWidth - indexWidth - blockOffsetWidth - byteOffsetWidth
  private val nMshrs = nWays // nMshrs = nWays for now

  val io = IO(new Bundle{
    val cache = new CacheIO(nCores, reqIdWidth, addressWidth, bytesPerSubBlock * 8)
    val repPol = Flipped(new ReplacementPolicyIO(nWays, nSets, nCores))
    val mem = new CacheMemoryControllerIO(addressWidth, bytesPerBurst * 8)
  })

  val missQueue = Module(new MissFifo(nCores, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, bytesPerSubBlock * 8))
  val wbQueue = Module(new WriteBackFifo(nMshrs, tagWidth, indexWidth, bytesPerBlock * 8))
  val updateLogic = Module(new UpdateUnit(nCores, nWays, reqIdWidth, tagWidth, indexWidth, bytesPerBlock * 8, bytesPerSubBlock * 8))

  val pipeStall = updateLogic.io.stall

  // ---------------- Decode ----------------
  val arbiter = Module(new RequestArbiter(nCores, addressWidth, bytesPerSubBlock * 8, reqIdWidth))
  val reqAccept = !pipeStall && !missQueue.io.push.full

  for (coreIdx <- 0 until nCores) {
    arbiter.io.ports(coreIdx).reqId <> io.cache.cores(coreIdx).req.reqId
    arbiter.io.ports(coreIdx).addr := io.cache.cores(coreIdx).req.addr
    arbiter.io.ports(coreIdx).rw := io.cache.cores(coreIdx).req.rw
    arbiter.io.ports(coreIdx).wData := io.cache.cores(coreIdx).req.wData
  }

  arbiter.io.out.reqId.ready := reqAccept

  val decLogic = Module(new Dec(nCores = nCores, nSets = nSets, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, byteOffWidth = byteOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  decLogic.io.stall := pipeStall
  decLogic.io.dec.coreId := arbiter.io.chosen
  decLogic.io.dec.reqId := arbiter.io.out.reqId.bits
  decLogic.io.dec.reqValid := arbiter.io.out.reqId.valid
  decLogic.io.dec.reqRw := arbiter.io.out.rw
  decLogic.io.dec.addr := arbiter.io.out.addr
  decLogic.io.dec.wData := arbiter.io.out.wData
  decLogic.io.update <> updateLogic.io.tagUpdate

  // ---------------- Tag and Dirty Lookup ----------------

  val tagLogic = Module(new Tag(nCores = nCores, nSets = nSets, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  tagLogic.io.stall := pipeStall
  tagLogic.io.tag <> decLogic.io.tag
  tagLogic.io.update <> updateLogic.io.tagUpdate

  // ---------------- Replacement ----------------

  val repLogic = Module(new Rep(nCores = nCores, nSets = nSets, nWays = nWays, nMshrs = nMshrs, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, subBlockWidth = bytesPerSubBlock * 8))
  repLogic.io.stall := pipeStall
  repLogic.io.rep <> tagLogic.io.rep
  repLogic.io.missFifo <> missQueue.io.push
  repLogic.io.repPol <> io.repPol

  // ---------------- Read ----------------

  val readLogic = Module(new Read(memSizeInBytes = sizeInBytes, nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffWidth = blockOffsetWidth, blockWidth = bytesPerBlock * 8, subBlockWidth = bytesPerSubBlock * 8))
  readLogic.io.stall := pipeStall
  readLogic.io.read <> repLogic.io.read
  readLogic.io.wbQueue <> wbQueue.io.push
  readLogic.io.memUpdate <> updateLogic.io.memUpdate

  // ---------------- Update ----------------

  val memInterface = Module(new MemoryInterface(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, bytesPerBlock * 8, bytesPerSubBlock * 8, bytesPerBurst * 8))
  memInterface.io.missFifo <> missQueue.io.pop
  memInterface.io.wbFifo <> wbQueue.io.pop
  memInterface.io.memController <> io.mem

  updateLogic.io.readStage <> readLogic.io.update
  updateLogic.io.memoryInterface <> memInterface.io.updateLogic

  for (coreIdx <- 0 until nCores) {
    io.cache.cores(coreIdx).resp <> updateLogic.io.coreResps(coreIdx)
  }
}
