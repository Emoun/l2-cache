package caches.hardware.pipelined.stages

import chisel3._
import chisel3.util._
import caches.hardware.util.PipelineReg

class ValidRegisterFile(nWays: Int, nSets: Int) extends Module {
  val io = IO(new Bundle {
    val readIndex = Input(UInt(log2Up(nSets).W))
    val set = Input(Bool())
    val unset = Input(Bool())
    val writeIndex = Input(UInt(log2Up(nSets).W))
    val writeWay = Input(UInt(log2Up(nWays).W))
    val rValid = Output(Vec(nWays, Bool()))
  })

  val validBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))

  val selValidBits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    selValidBits(wayIdx) := validBits(wayIdx)(io.readIndex) // Read out all the valid bits

    val isUpdateWay = io.writeWay === wayIdx.U

    when(isUpdateWay) {
      when(io.set) {
        validBits(wayIdx)(io.writeIndex) := true.B
      } .elsewhen(io.unset) {
        validBits(wayIdx)(io.writeIndex) := false.B
      }
    }
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

  val dirtyBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))

  val selDirtyBits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    selDirtyBits(wayIdx) := dirtyBits(wayIdx)(io.readIndex) // Read out all the dirty bits

    when(io.writeWay === wayIdx.U) {
      val ctrl = Cat(io.refill, io.update)
      switch(ctrl) {
        is("b10".U) { dirtyBits(wayIdx)(io.writeIndex) := false.B }
        is("b01".U) { dirtyBits(wayIdx)(io.writeIndex) := true.B }
        is("b11".U) { dirtyBits(wayIdx)(io.writeIndex) := true.B }
      }
    }
  }

  io.rDirtyBits := selDirtyBits
}

class TagIO(nWays: Int, nCores: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val readTags = Input(Vec(nWays, UInt(tagWidth.W)))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((subBlockWidth / 8).W))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val index = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
}

class Tag(nCores: Int, nSets: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Module() {
  val io = IO(new Bundle {
    val tag = new TagIO(nWays, nCores, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
    val rep = Flipped(new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
    val update = Flipped(new TagUpdateIO(nWays, tagWidth))
    val stall = Input(Bool())
  })

  val validRegFile = Module(new ValidRegisterFile(nWays, nSets))
  val dirtyRegFile = Module(new DirtyRegisterFile(nWays, nSets))

  validRegFile.io.readIndex := io.tag.index
  validRegFile.io.writeIndex := io.update.index
  validRegFile.io.writeWay := io.update.way
  validRegFile.io.set := io.update.refill
  validRegFile.io.unset := io.update.invalidate

  dirtyRegFile.io.readIndex := io.tag.index
  dirtyRegFile.io.writeIndex := io.update.index
  dirtyRegFile.io.writeWay := io.update.way
  dirtyRegFile.io.refill := io.update.refill
  dirtyRegFile.io.update := io.update.update

  val validBitsForWay = validRegFile.io.rValid
  val dirtyBitsForWay = dirtyRegFile.io.rDirtyBits

  // Compare tags and check if there is a hit and where
  val hits = Wire(Vec(nWays, Bool()))
  for (wayIdx <- 0 until nWays) {
    hits(wayIdx) := validBitsForWay(wayIdx) && (io.tag.tag === io.tag.readTags(wayIdx))
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
  io.rep.setTags := PipelineReg(io.tag.readTags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall) // Need this to get the dirty tag later on
  io.rep.blockOffset := PipelineReg(io.tag.blockOffset, 0.U, !io.stall)
  io.rep.index := PipelineReg(io.tag.index, 0.U, !io.stall)
  io.rep.tag := PipelineReg(io.tag.tag, 0.U, !io.stall)
}
