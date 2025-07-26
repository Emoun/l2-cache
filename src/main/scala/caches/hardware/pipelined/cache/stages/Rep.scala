package caches.hardware.pipelined.cache.stages

import chisel3._
import chisel3.util._
import caches.hardware.pipelined.cache._
import caches.hardware.reppol.ReplacementPolicyIO
import caches.hardware.util._

class RepIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val isHit = Input(Bool())
  val hitWay = Input(UInt(log2Up(nWays).W))
  val dirtyBits = Input(Vec(nWays, Bool()))
  val setTags = Input(Vec(nWays, UInt(tagWidth.W)))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val index = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
}

class Rep(nCores: Int, nSets: Int, nWays: Int, nMshrs: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Module() {
  val io = IO(new Bundle {
      val rep = new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
      val read = Flipped(new ReadIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
      val missFifo = Flipped(new MissFifoPushIO(nCores = nCores, nMSHRs = nMshrs, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffsetWidth = blockOffWidth, subBlockWidth = subBlockWidth))
      val repPol = Flipped(new ReplacementPolicyIO(nWays = nWays, nSets = nSets, nCores = nCores))
      val stall = Input(Bool())
    }
  )

  def halfMissCheck(currIndex: UInt, currTag: UInt): Bool = {
    val missMatches = Wire(Vec(nMshrs, Bool()))

    for (mshr <- 0 until nMshrs) {
      val mshrIndex = io.missFifo.currentIndexes(mshr)
      val mshrTag = io.missFifo.currentTags(mshr)
      val validMshr = io.missFifo.validMSHRs(mshr)

      missMatches(mshr) := mshrIndex === currIndex && mshrTag === currTag && validMshr
    }

    missMatches.reduce((x, y) => x || y)
  }

  // ---------------- Compute Replace Way ----------------
  io.repPol.setIdx := io.rep.index
  io.repPol.stall := io.stall

  // TODO: We can compute a half miss check here, since we do not need to know the replacement way either way
  //val isHalfMiss = halfMissCheck(io.rep.index, io.rep.tag)

  val coreIdReg = PipelineReg(io.rep.coreId, 0.U, !io.stall)
  val reqValidReg = PipelineReg(io.rep.reqValid, false.B, !io.stall)
  val reqIdReg = PipelineReg(io.rep.reqId, 0.U, !io.stall)
  val reqRwReg = PipelineReg(io.rep.reqRw, false.B, !io.stall)
  val wDataReg = PipelineReg(io.rep.wData, 0.U, !io.stall)
  val isHitReg = PipelineReg(io.rep.isHit, false.B, !io.stall)
  val hitWayReg = PipelineReg(io.rep.hitWay, 0.U, !io.stall)
  val dirtyBitsReg = PipelineReg(io.rep.dirtyBits, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  val setTagsReg = PipelineReg(io.rep.setTags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall)
  val blockOffsetReg = PipelineReg(io.rep.blockOffset, 0.U, !io.stall)
  val indexReg = PipelineReg(io.rep.index, 0.U, !io.stall)
  val tagReg = PipelineReg(io.rep.tag, 0.U, !io.stall)

  // ---------------- Update Replacement policy ----------------

  // TODO: Need to add RSHR interface here, and push a request to it if it is rejected

  val repWay = io.repPol.replaceWay
  val repWayValid = io.repPol.isValid

  val isRepDirty = dirtyBitsReg(repWay)
  val dirtyTag = setTagsReg(repWay)

  val evict = reqValidReg && (!isHitReg && repWayValid)

  io.repPol.update.valid := reqValidReg // We update the replacement policy even on a miss, since this miss later turns into a hit anyway
  io.repPol.update.bits := Mux(isHitReg, hitWayReg, repWay)
  io.repPol.evict := evict
  io.repPol.coreId := coreIdReg

  io.missFifo.push := evict
  io.missFifo.pushEntry.rw := reqRwReg
  io.missFifo.pushEntry.wData := wDataReg
  io.missFifo.pushEntry.replaceWay := repWay
  io.missFifo.pushEntry.tag := tagReg
  io.missFifo.pushEntry.index := indexReg
  io.missFifo.pushEntry.blockOffset := blockOffsetReg
  io.missFifo.pushEntry.reqId := reqIdReg
  io.missFifo.pushEntry.coreId := coreIdReg

  io.read.coreId := coreIdReg
  io.read.reqValid := reqValidReg
  io.read.reqId := reqIdReg
  io.read.reqRw := reqRwReg
  io.read.wData := wDataReg
  io.read.repValid := repWayValid
  io.read.repWay := repWay
  io.read.isHit := isHitReg
  io.read.hitWay := hitWayReg
  io.read.isRepDirty := isRepDirty
  io.read.dirtyTag := dirtyTag
  io.read.blockOffset := blockOffsetReg
  io.read.index := indexReg
  io.read.tag := tagReg
}
