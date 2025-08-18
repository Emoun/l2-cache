package caches.hardware.pipelined.stages

import chisel3._
import chisel3.util._
import caches.hardware.util.{MemBlock, PipelineReg}

class ValidMem(nWays: Int, nSets: Int) extends Module {
  val io = IO(new Bundle {
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
    validBits(wayIdx).io.wrEn := wrEn && io.writeWay === wayIdx.U

    selValidBits(wayIdx) := validBits(wayIdx).io.readData // Read out all the valid bits
  }

  io.rValid := selValidBits
}

class DirtyRegisterFile (nWays: Int, nSets: Int) extends Module() {
  val io = IO(new Bundle {
    val readIndex = Input(UInt(log2Up(nSets).W))
    val writeIndex = Input(UInt(log2Up(nSets).W))
    val writeWay = Input(UInt(log2Up(nWays).W))
    val refill = Input(Bool())
    val update = Input(Bool())
    val rDirtyBits = Output(Vec(nWays, Bool()))
  })

  val dirtyBits = Array.fill(nWays)(Module(new MemBlock(nSets, 1)))
  val writeData = WireDefault(0.U(1.W))
  val wrEn = io.refill || io.update

  val ctrl = Cat(io.refill, io.update)
  switch(ctrl) {
    is("b10".U) { writeData := false.B }
    is("b01".U) { writeData := true.B }
    is("b11".U) { writeData := true.B }
  }

  val selDirtyBits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    dirtyBits(wayIdx).io.readAddr := io.readIndex
    dirtyBits(wayIdx).io.writeAddr := io.writeIndex
    dirtyBits(wayIdx).io.writeData := writeData
    dirtyBits(wayIdx).io.wrEn := wrEn && io.writeWay === wayIdx.U

    selDirtyBits(wayIdx) := dirtyBits(wayIdx).io.readData // Read out all the dirty bits
  }

  io.rDirtyBits := selDirtyBits
}

class TagIO(nWays: Int, nCores: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
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
    val tag = new TagIO(nWays, nCores, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
    val rep = Flipped(new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
    val update = Flipped(new TagUpdateIO(nWays, indexWidth, tagWidth))
    val stall = Input(Bool())
  })

  val tagMem = Array.fill(nWays)(Module(new MemBlock(nSets, tagWidth)))
  val validBitMem = Module(new ValidMem(nWays, nSets))
  val dirtyRegFile = Module(new DirtyRegisterFile(nWays, nSets))

  val readTags = Wire(Vec(nWays, UInt(tagWidth.W)))
  for (wayIdx <- 0 until nWays) {
    val isUpdateWay = io.update.way === wayIdx.U

    // Assign the signals for the tag memories
    tagMem(wayIdx).io.readAddr := io.tag.readIndex
    tagMem(wayIdx).io.writeData := io.update.tag
    tagMem(wayIdx).io.writeAddr := io.update.index
    tagMem(wayIdx).io.wrEn := io.update.refill && isUpdateWay

    readTags(wayIdx) := tagMem(wayIdx).io.readData
  }

  validBitMem.io.readIndex := io.tag.readIndex
  validBitMem.io.writeIndex := io.update.index
  validBitMem.io.writeWay := io.update.way
  validBitMem.io.set := io.update.refill
  validBitMem.io.unset := io.update.invalidate

  dirtyRegFile.io.readIndex := io.tag.readIndex
  dirtyRegFile.io.writeIndex := io.update.index
  dirtyRegFile.io.writeWay := io.update.way
  dirtyRegFile.io.refill := io.update.refill
  dirtyRegFile.io.update := io.update.update

  val validBitsForWay = validBitMem.io.rValid
  val dirtyBitsForWay = dirtyRegFile.io.rDirtyBits

  // Compare tags and check if there is a hit and where
  val hits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    hits(wayIdx) := validBitsForWay(wayIdx) && (io.tag.tag === readTags(wayIdx))
  }

  val hit = hits.reduce((x, y) => x || y)
  val hitWay = PriorityEncoder(hits)

  // Check if the hit way is valid and if it is the same as the one being currently refilled
  //  if so then turn this request into a miss
  val isHitIntoEvictedLine = (io.update.invalidate || io.update.refill) && hit && (io.tag.index === io.update.index) && (hitWay === io.update.way)

  io.rep.coreId := PipelineReg(io.tag.coreId, 0.U, !io.stall)
  io.rep.reqValid := PipelineReg(io.tag.reqValid, false.B, !io.stall)
  io.rep.reqId := PipelineReg(io.tag.reqId, 0.U, !io.stall)
  io.rep.reqRw := PipelineReg(io.tag.reqRw, false.B, !io.stall)
  io.rep.wData := PipelineReg(io.tag.wData, 0.U, !io.stall)
  io.rep.byteEn := PipelineReg(io.tag.byteEn, 0.U, !io.stall)
  io.rep.isHit := PipelineReg(Mux(isHitIntoEvictedLine, false.B, hit), false.B, !io.stall)
  io.rep.hitWay := PipelineReg(hitWay, 0.U, !io.stall)
  io.rep.dirtyBits := PipelineReg(dirtyBitsForWay, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  io.rep.setTags := PipelineReg(readTags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall) // Need this to get the dirty tag later on
  io.rep.blockOffset := PipelineReg(io.tag.blockOffset, 0.U, !io.stall)
  io.rep.index := PipelineReg(io.tag.index, 0.U, !io.stall)
  io.rep.tag := PipelineReg(io.tag.tag, 0.U, !io.stall)
}
