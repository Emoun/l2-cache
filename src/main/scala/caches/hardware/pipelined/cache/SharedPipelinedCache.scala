package caches.hardware.pipelined.cache

import chisel3._
import caches.hardware.MemBlock
import caches.hardware.reppol.ReplacementPolicyIO
import chisel3.util.{Decoupled, PriorityEncoder, log2Up}

class CacheRequestIO(addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Flipped(Decoupled(UInt(reqIdWidth.W)))
  val addr = Input(UInt(addrWidth.W))
  val rw = Input(Bool())
  val wData = Input(UInt(dataWidth.W))
}

class CacheResponseIO(dataWidth: Int, reqIdWidth: Int) extends Bundle {
  val reqId = Decoupled(UInt(reqIdWidth.W))
  val rData = Output(UInt(dataWidth.W))
  val responseStatus = Output(UInt(1.W)) // 1 - OK, 0 - Rejected
}

class CacheIO(nCores: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val coreReqs = Vec(nCores, new CacheRequestIO(addrWidth, dataWidth, log2Up(nCores)))
  val coreResps = Vec(nCores, new CacheResponseIO(dataWidth, log2Up(nCores)))
}

class SharedPipelinedCache(sizeInBytes: Int, nWays: Int, nCores: Int, addressWidth: Int, bytesPerBlock: Int, bytesPerSubBlock: Int, bytesPerBurst: Int) extends Module {
  private val nSets = sizeInBytes / (nWays * bytesPerBlock)
  private val wordsPerBlock = bytesPerBlock / bytesPerSubBlock
  private val byteOffsetWidth = log2Up(bytesPerSubBlock)
  private val blockOffsetWidth = log2Up(wordsPerBlock)
  private val indexWidth = log2Up(nSets)
  private val tagWidth = addressWidth - indexWidth - blockOffsetWidth - byteOffsetWidth
  private val nMshrs = nWays // nMshrs = nWays for now

  val io = IO(new Bundle{
    val cache = new CacheIO(nCores, addressWidth, bytesPerSubBlock * 8)
    val repPol = Flipped(new ReplacementPolicyIO(nWays, nSets, nCores))
    val memController = new MemoryControllerIO(addressWidth, bytesPerBurst * 8)
  })

  def pipelineReg[T <: Data](next: T, init: T, en: Bool): T = {
    val pipelineReg = RegInit(init)
    when(en) {
      pipelineReg := next
    }
    pipelineReg
  }

  val missQueue = Module(new MissFifo(nMshrs, nWays, log2Up(nCores), tagWidth, indexWidth, blockOffsetWidth, bytesPerSubBlock * 8))
  val updateLogic = Module(new UpdateUnit(nWays, log2Up(nCores), tagWidth, indexWidth, bytesPerBlock * 8, bytesPerSubBlock * 8))

  val pipeStall = updateLogic.io.cacheUpdateControl.stall

  // ---------------- Decode ----------------
  val arbiter = Module(new RequestArbiter(nCores, addressWidth, bytesPerSubBlock * 8, log2Up(nCores)))
  val reqAccept = !pipeStall && !missQueue.io.full

  arbiter.io.out.reqId.ready := reqAccept

  for (coreIdx <- 0 until nCores) {
    arbiter.io.ports(coreIdx).reqId <> io.cache.coreReqs(coreIdx).reqId
    arbiter.io.ports(coreIdx).addr := io.cache.coreReqs(coreIdx).addr
    arbiter.io.ports(coreIdx).rw := io.cache.coreReqs(coreIdx).rw
    arbiter.io.ports(coreIdx).wData := io.cache.coreReqs(coreIdx).wData
  }

  // val byteOffset = io.addr(byteOffsetWidth - 1, 0)
  val blockOffset = arbiter.io.out.addr((blockOffsetWidth - 1) + byteOffsetWidth, byteOffsetWidth)
  val index = arbiter.io.out.addr((indexWidth - 1) + blockOffsetWidth + byteOffsetWidth, blockOffsetWidth + byteOffsetWidth)
  val tag = arbiter.io.out.addr(addressWidth - 1, indexWidth + blockOffsetWidth + byteOffsetWidth)

  val tagMem = Array.fill(nWays)(Module(new MemBlock(nSets, tagWidth)))
  val readTags = Wire(Vec(nWays, UInt(tagWidth.W)))

  for (wayIdx <- 0 until nWays) {
    val isUpdateWay = updateLogic.io.cacheUpdateControl.way === wayIdx.U

    // Assign the signals for the tag memories
    tagMem(wayIdx).io.readAddr := index
    tagMem(wayIdx).io.writeData := updateLogic.io.cacheUpdateControl.tag
    tagMem(wayIdx).io.writeAddr := updateLogic.io.cacheUpdateControl.index
    tagMem(wayIdx).io.wrEn := updateLogic.io.cacheUpdateControl.refill && isUpdateWay

    readTags(wayIdx) := tagMem(wayIdx).io.readData
  }

  val reqValidTagReg = pipelineReg(arbiter.io.out.reqId.valid && reqAccept, false.B, !pipeStall)
  val reqIdTagReg = pipelineReg(arbiter.io.out.reqId.bits, 0.U, !pipeStall)
  val reqRwTagReg = pipelineReg(arbiter.io.out.rw, false.B, !pipeStall)
  val wDataTagReg = pipelineReg(arbiter.io.out.wData, 0.U, !pipeStall)
  val blockTagReg = pipelineReg(blockOffset, 0.U, !pipeStall)
  val indexTagReg = pipelineReg(index, 0.U, !pipeStall)
  val tagTagReg = pipelineReg(tag, 0.U, !pipeStall)

  // ---------------- Tag and Dirty Lookup ----------------

  val dirtyBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))
  val validBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))

  val hits = Wire(Vec(nWays, Bool()))
  val dirty = Wire(Vec(nWays, Bool()))

  // Compare tags and check if there is a hit and where
  for (wayIdx <- 0 until nWays) {
    hits(wayIdx) := validBits(wayIdx)(indexTagReg) && (tagTagReg === readTags(wayIdx))
    dirty(wayIdx) := dirtyBits(wayIdx)(indexTagReg)

    val isUpdateWay = updateLogic.io.cacheUpdateControl.way === wayIdx.U

    // Set the valid bit when we replace a line and unset the dirty bit
    when(updateLogic.io.cacheUpdateControl.refill && isUpdateWay) {
      validBits(wayIdx)(updateLogic.io.cacheUpdateControl.index) := true.B

      when(updateLogic.io.cacheUpdateControl.update) {
        dirtyBits(wayIdx)(updateLogic.io.cacheUpdateControl.index) := true.B
      } .otherwise {
        dirtyBits(wayIdx)(updateLogic.io.cacheUpdateControl.index) := false.B
      }
    }

    // Set the dirty bit when updating, else when evicting reset it
    when(!updateLogic.io.cacheUpdateControl.refill && updateLogic.io.cacheUpdateControl.update && isUpdateWay) {
      dirtyBits(wayIdx)(updateLogic.io.cacheUpdateControl.index) := true.B
    }
  }

  val hit = hits.reduce((x, y) => x || y)

  // TODO: Check all replacement policy signals are correct
  io.repPol.reqId := reqIdTagReg

  when(hit) {
    io.repPol.update.valid := true.B
    io.repPol.update.bits := hit
    io.repPol.setIdx := indexTagReg
  } .otherwise {
    io.repPol.update.valid := false.B
    io.repPol.update.bits := 0.U
    io.repPol.setIdx := 0.U
  }

  val reqValidRepReg = pipelineReg(reqValidTagReg, false.B, !pipeStall)
  val reqIdRepReg = pipelineReg(reqIdTagReg, 0.U, !pipeStall)
  val reqRwRepReg = pipelineReg(reqRwTagReg, false.B, !pipeStall)
  val wDataRepReg = pipelineReg(wDataTagReg, 0.U, !pipeStall)
  val hitRepReg = pipelineReg(hit, false.B, !pipeStall)
  val hitWayRepReg = pipelineReg(PriorityEncoder(hits), 0.U, !pipeStall)
  val dirtyRepReg = pipelineReg(dirty, VecInit(Seq.fill(nWays)(false.B)), !pipeStall)
  val readTagsRepReg = pipelineReg(readTags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !pipeStall)
  val blockRepReg = pipelineReg(blockTagReg, 0.U, !pipeStall)
  val indexRepReg = pipelineReg(indexTagReg, 0.U, !pipeStall)
  val tagRepReg = pipelineReg(tagTagReg, 0.U, !pipeStall)

  // ---------------- Replacement ----------------

  val missQueuePush = WireDefault(false.B)
  val replacementWay = io.repPol.replaceWay
  val isRepDirty = dirtyRepReg(replacementWay)
  val dirtyTag = readTagsRepReg(replacementWay)

  // Signal for checking if a current request is accessing a line that is currently an outstanding miss
  val previousMissesCheck = Wire(Vec(nMshrs, Bool()))

  for (mshr <- 0 until nMshrs) {
    val mshrIndex = missQueue.io.currentIndexes(mshr)
    val mshrWay = missQueue.io.currentWays(mshr)
    val validMshr = missQueue.io.validMSHRs(mshr)

    previousMissesCheck(mshr) := mshrIndex === indexRepReg && mshrWay === replacementWay && validMshr
  }

  val isPreviousMiss = previousMissesCheck.reduce((x, y) => x || y)
  val evict = WireDefault(false.B)
  val isValidRep = io.repPol.isValid

  // NOTE: We could allow a previous miss hit to access the cache line if it is only a read request, and the line has not yet been evicted

  // If the reqeust is trying to access a line of a previous miss or if it is just a miss we push to the queue
  when(isPreviousMiss || !hitRepReg && reqValidRepReg && isValidRep) {
    missQueuePush := true.B
    evict := true.B // For some policies it is necessary to know if we are evicting a line
  }

  io.repPol.evict := evict

  missQueue.io.push := missQueuePush
  missQueue.io.pushEntry.rw := reqRwRepReg
  missQueue.io.pushEntry.wData := wDataRepReg
  missQueue.io.pushEntry.replaceWay := replacementWay
  missQueue.io.pushEntry.tag := tagRepReg
  missQueue.io.pushEntry.index := indexRepReg
  missQueue.io.pushEntry.blockOffset := blockRepReg
  missQueue.io.pushEntry.reqId := reqIdRepReg

  val dataMem = Module(new CacheMemory(sizeInBytes, nWays, bytesPerBlock, bytesPerSubBlock))

  dataMem.io.rIndex := indexRepReg
  dataMem.io.rWayIdx := Mux(hitRepReg, hitWayRepReg, replacementWay)
  dataMem.io.wrIndex := updateLogic.io.cacheUpdateControl.index
  dataMem.io.wrWayIdx := updateLogic.io.cacheUpdateControl.way
  dataMem.io.wrEn := updateLogic.io.cacheUpdateControl.wrEn
  dataMem.io.wrData := updateLogic.io.cacheUpdateControl.memWriteData

  val repValidReadReg = pipelineReg(isValidRep, true.B, !pipeStall)
  val reqValidReadReg = pipelineReg(reqValidRepReg, false.B, !pipeStall)
  val reqIdReadReg = pipelineReg(reqIdRepReg, 0.U, !pipeStall)
  val reqRwReadReg = pipelineReg(reqRwRepReg, false.B, !pipeStall)
  val wDataReadReg = pipelineReg(wDataRepReg, 0.U, !pipeStall)
  val hitReadReg = pipelineReg(hitRepReg, false.B, !pipeStall)
  val hitWayReadReg = pipelineReg(hitWayRepReg, 0.U, !pipeStall)
  val isRepDirtyReadReg = pipelineReg(isRepDirty, false.B, !pipeStall)
  val dirtyTagReadReg = pipelineReg(dirtyTag, 0.U, !pipeStall)
  val blockReadReg = pipelineReg(blockRepReg, 0.U, !pipeStall)
  val indexReadReg = pipelineReg(indexRepReg, 0.U, !pipeStall)
  val tagReadReg = pipelineReg(tagRepReg, 0.U, !pipeStall)

  // ---------------- Read ----------------

  val wbQueue = Module(new WriteBackFifo(log2Up(nCores), tagWidth, indexWidth, blockOffsetWidth, bytesPerBlock * 8))

  wbQueue.io.push := isRepDirtyReadReg && !hitReadReg && reqValidReadReg
  wbQueue.io.pushEntry.wbData := dataMem.io.rData.asUInt
  wbQueue.io.pushEntry.tag := dirtyTagReadReg
  wbQueue.io.pushEntry.index := indexReadReg
  wbQueue.io.pushEntry.blockOffset := blockReadReg

//  val reqValidUpdateReg = RegNext(reqValidReadReg && hitReadReg, false.B)
//  val reqIdUpdateReg = RegNext(reqIdReadReg, 0.U)
//  val reqRwUpdateReg = RegNext(reqRwReadReg, false.B)
//  val wDataUpdateReg = RegNext(wDataReadReg, 0.U)
//  val cacheMemReadUpdateReg = RegNext(dataMem.io.rData, VecInit(Seq.fill(nWays)(0.U((bytesPerSubBlock * 8).W))))
//  val hitUpdateReg = RegNext(hitReadReg, false.B)
//  val hitWayUpdateReg = RegNext(hitWayReadReg, 0.U)
//  val blockUpdateReg = RegNext(blockReadReg, 0.U)
//  val indexUpdateReg = RegNext(indexReadReg, 0.U)
//  val tagUpdateReg = RegNext(tagReadReg, 0.U)

  // ---------------- Update ----------------

  val memInterface = Module(new MemoryInterface(nWays, log2Up(nCores), tagWidth, indexWidth, blockOffsetWidth, bytesPerBlock * 8, bytesPerSubBlock * 8, bytesPerBurst * 8))

  // Connections between memory interface and the miss and write-back FIFOs
  memInterface.io.missFifo.popEntry <> missQueue.io.popEntry
  missQueue.io.pop := memInterface.io.missFifo.pop
  memInterface.io.missFifo.empty := missQueue.io.empty
  memInterface.io.wbFifo.popEntry <> wbQueue.io.popEntry
  memInterface.io.wbFifo.empty := wbQueue.io.empty
  wbQueue.io.pop := memInterface.io.wbFifo.pop

  // Connection between the memory interface and the memory controller
  io.memController <> memInterface.io.memController

  updateLogic.io.readStage.valid := reqValidReadReg && hitReadReg
  updateLogic.io.readStage.rw := reqRwReadReg
  updateLogic.io.readStage.reqId := reqIdReadReg
  updateLogic.io.readStage.wData := wDataReadReg
  updateLogic.io.readStage.wWay := hitWayReadReg
  updateLogic.io.readStage.blockOffset := blockReadReg
  updateLogic.io.readStage.index := indexReadReg
  updateLogic.io.readStage.tag := tagReadReg
  updateLogic.io.readStage.memReadData := dataMem.io.rData

  updateLogic.io.memoryInterface <> memInterface.io.updateLogic

  // TODO: We should make use of the reqId.ready
  for (coreIdx <- 0 until nCores) {
    io.cache.coreResps(coreIdx).reqId.valid := (updateLogic.io.coreResp.reqId.bits === coreIdx.U) && updateLogic.io.coreResp.reqId.valid
    io.cache.coreResps(coreIdx).reqId.bits := updateLogic.io.coreResp.reqId.bits
    updateLogic.io.coreResp.reqId.ready := io.cache.coreResps(coreIdx).reqId.ready // TODO: This is wrong, assigning each cores ready line to a single input
    io.cache.coreResps(coreIdx).rData := updateLogic.io.coreResp.rData
    io.cache.coreResps(coreIdx).responseStatus := repValidReadReg // TODO: Move this to the update unit instead
  }
}
