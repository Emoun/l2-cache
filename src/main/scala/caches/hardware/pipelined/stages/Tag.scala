package caches.hardware.pipelined.stages

import chisel3._
import chisel3.util._
import caches.hardware.util.{MemBlock, PipelineReg}

class ValidMem(nWays: Int, nSets: Int) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val readIndex = Input(UInt(log2Up(nSets).W))
    val set = Input(Bool())
    val unset = Input(Bool())
    val writeIndex = Input(UInt(log2Up(nSets).W))
    val writeWay = Input(UInt(log2Up(nWays).W))
    val rValid = Output(Vec(nWays, Bool()))
  })

  val validBits = Array.fill(nWays)(Module(new MemBlock(nSets, 1)))
  val writeData = WireDefault(0.U(1.W))
  val wrEn = io.set || io.unset

  when(io.set) {
    writeData := true.B
  } .elsewhen(io.unset) {
    writeData := false.B
  }

  val selValidBits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    validBits(wayIdx).io.readAddr := io.readIndex
    validBits(wayIdx).io.writeAddr := io.writeIndex
    validBits(wayIdx).io.writeData := writeData
    validBits(wayIdx).io.stall := io.stall
    validBits(wayIdx).io.wrEn := wrEn && io.writeWay === wayIdx.U

    selValidBits(wayIdx) := validBits(wayIdx).io.readData // Read out all the valid bits
  }

  io.rValid := selValidBits
}

class DirtyRegisterFile (nWays: Int, nSets: Int) extends Module() {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val rIndex = Input(UInt(log2Up(nSets).W))
    val set = Input(Bool())
    val unset = Input(Bool())
    val wIndex = Input(UInt(log2Up(nSets).W))
    val wWay = Input(UInt(log2Up(nWays).W))
    val rDirtyBits = Output(Vec(nWays, Bool()))
  })

  // NOTE: Consider if maybe it is worth storing a dirty bit per each sub-block instead of a block
  val dirtyBits = Array.fill(nWays)(Module(new MemBlock(nSets, 1)))
  val writeData = WireDefault(0.U(1.W))
  val wrEn = io.unset || io.set

  when(io.unset) {
    writeData := false.B
  } .elsewhen(io.set) {
    writeData := true.B
  }

  val selDirtyBits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    dirtyBits(wayIdx).io.readAddr := io.rIndex
    dirtyBits(wayIdx).io.writeAddr := io.wIndex
    dirtyBits(wayIdx).io.writeData := writeData
    dirtyBits(wayIdx).io.stall := io.stall
    dirtyBits(wayIdx).io.wrEn := wrEn && io.wWay === wayIdx.U

    selDirtyBits(wayIdx) := dirtyBits(wayIdx).io.readData // Read out all the dirty bits
  }

  io.rDirtyBits := selDirtyBits
}

class TagIO(nCores: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqId = Input(UInt(reqIdWidth.W))
  val reqValid = Input(Bool())
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((subBlockWidth / 8).W))
  val index = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val readIndex = Input(UInt(indexWidth.W))
}

class Tag(nCores: Int, nSets: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Module() {
  val io = IO(new Bundle {
    val tag = new TagIO(nCores, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
    val rep = Flipped(new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
    val tagCtrl = Flipped(new TagUpdateIO(nWays, indexWidth, tagWidth))
    val dirtyCtrl = Flipped(new DirtyCtrlIO(nWays, indexWidth))
    val setValidLine = Flipped(new SetLineValidIO(nWays, indexWidth, tagWidth))
    val invalidate = Flipped(new InvalidateLineIO(nWays = nWays, indexWidth = indexWidth))
    val stall = Input(Bool())
  })

  val tagMem = Array.fill(nWays)(Module(new MemBlock(nSets, tagWidth)))
  val validBitMem = Module(new ValidMem(nWays, nSets))
  val dirtyRegFile = Module(new DirtyRegisterFile(nWays, nSets))

  val readTags = Wire(Vec(nWays, UInt(tagWidth.W)))
  for (wayIdx <- 0 until nWays) {
    val isUpdateWay = io.tagCtrl.way === wayIdx.U

    // Assign the signals for the tag memories
    tagMem(wayIdx).io.readAddr := io.tag.readIndex
    tagMem(wayIdx).io.writeData := io.tagCtrl.tag
    tagMem(wayIdx).io.writeAddr := io.tagCtrl.index
    tagMem(wayIdx).io.wrEn := io.tagCtrl.refill && isUpdateWay
    tagMem(wayIdx).io.stall := io.stall

    readTags(wayIdx) := tagMem(wayIdx).io.readData
  }

  validBitMem.io.readIndex := io.tag.readIndex
  validBitMem.io.writeIndex := Mux(io.tagCtrl.refill, io.tagCtrl.index, io.invalidate.index)
  validBitMem.io.writeWay := Mux(io.tagCtrl.refill, io.tagCtrl.way, io.invalidate.way)
  validBitMem.io.set := io.tagCtrl.refill
  validBitMem.io.unset := io.invalidate.invalidate
  validBitMem.io.stall := io.stall

  dirtyRegFile.io.rIndex := io.tag.readIndex
  dirtyRegFile.io.wIndex := io.dirtyCtrl.wIndex
  dirtyRegFile.io.wWay := io.dirtyCtrl.wWay
  dirtyRegFile.io.unset := io.dirtyCtrl.unset
  dirtyRegFile.io.set := io.dirtyCtrl.set
  dirtyRegFile.io.stall := io.stall

  val validBitsForIndex = validBitMem.io.rValid
  val dirtyBitsForIndex = dirtyRegFile.io.rDirtyBits

  // Compare tags and check if there is a hit and where
  val hits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    hits(wayIdx) := validBitsForIndex(wayIdx) && (io.tag.tag === readTags(wayIdx))
  }

  val hit = hits.reduce((x, y) => x || y)
  val hitWay = PriorityEncoder(hits)

  // Check if a line has been either invalidated or has been set valid in the meantime
  val isMissNowHit = io.setValidLine.tag === io.tag.tag && io.setValidLine.index === io.tag.index && io.setValidLine.refill && !hit
  val isLineNowInvalid = io.invalidate.invalidate && hit && (io.invalidate.way === hitWay) && (io.invalidate.index === io.tag.index)
  val isHit = !isLineNowInvalid && (isMissNowHit || hit)
  val trueHitWay = Mux(isMissNowHit, io.setValidLine.way, hitWay)

  io.rep.coreId := PipelineReg(io.tag.coreId, 0.U, !io.stall)
  io.rep.reqValid := PipelineReg(io.tag.reqValid, false.B, !io.stall)
  io.rep.reqId := PipelineReg(io.tag.reqId, 0.U, !io.stall)
  io.rep.reqRw := PipelineReg(io.tag.reqRw, false.B, !io.stall)
  io.rep.wData := PipelineReg(io.tag.wData, 0.U, !io.stall)
  io.rep.byteEn := PipelineReg(io.tag.byteEn, 0.U, !io.stall)
  io.rep.isHit := PipelineReg(isHit, false.B, !io.stall)
  io.rep.hitWay := PipelineReg(trueHitWay, 0.U, !io.stall)
  io.rep.dirtyBits := PipelineReg(dirtyBitsForIndex, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  io.rep.setTags := PipelineReg(readTags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall) // Need this to get the dirty tag later on
  io.rep.blockOffset := PipelineReg(io.tag.blockOffset, 0.U, !io.stall)
  io.rep.index := PipelineReg(io.tag.index, 0.U, !io.stall)
  io.rep.repPolReadIndex := io.tag.index
  io.rep.tag := PipelineReg(io.tag.tag, 0.U, !io.stall)
}
