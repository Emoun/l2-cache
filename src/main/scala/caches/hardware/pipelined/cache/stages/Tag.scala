package caches.hardware.pipelined.cache.stages

import caches.hardware.pipelined.cache.TagUpdateIO
import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

class TagIO(nWays: Int, nCores: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val readTags = Input(Vec(nWays, UInt(tagWidth.W)))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
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

  val dirtyBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))
  val validBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))

  val hits = Wire(Vec(nWays, Bool()))
  val selDirtyBits = Wire(Vec(nWays, Bool()))

  // Compare tags and check if there is a hit and where
  for (wayIdx <- 0 until nWays) {
    hits(wayIdx) := validBits(wayIdx)(io.tag.index) && (io.tag.tag === io.tag.readTags(wayIdx))
    selDirtyBits(wayIdx) := dirtyBits(wayIdx)(io.tag.index)

    val isUpdateWay = io.update.way === wayIdx.U

    // Set the valid bit when we replace a line and unset the dirty bit
    when(io.update.refill && isUpdateWay) {
      validBits(wayIdx)(io.update.index) := true.B

      when(io.update.update) {
        dirtyBits(wayIdx)(io.update.index) := true.B
      } .otherwise {
        dirtyBits(wayIdx)(io.update.index) := false.B
      }
    }

    // Set the dirty bit when updating, else when evicting reset it
    when(!io.update.refill && io.update.update && isUpdateWay) {
      dirtyBits(wayIdx)(io.update.index) := true.B
    }
  }

  val hit = hits.reduce((x, y) => x || y)
  val hitWay = PriorityEncoder(hits)

  io.rep.coreId := PipelineReg(io.tag.coreId, 0.U, !io.stall)
  io.rep.reqValid := PipelineReg(io.tag.reqValid, false.B, !io.stall)
  io.rep.reqId := PipelineReg(io.tag.reqId, 0.U, !io.stall)
  io.rep.reqRw := PipelineReg(io.tag.reqRw, false.B, !io.stall)
  io.rep.wData := PipelineReg(io.tag.wData, 0.U, !io.stall)
  io.rep.isHit := PipelineReg(hit, false.B, !io.stall)
  io.rep.hitWay := PipelineReg(hitWay, 0.U, !io.stall)
  io.rep.dirtyBits := PipelineReg(selDirtyBits, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  io.rep.setTags := PipelineReg(io.tag.readTags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall) // Need this to get the dirty tag later on
  io.rep.blockOffset := PipelineReg(io.tag.blockOffset, 0.U, !io.stall)
  io.rep.index := PipelineReg(io.tag.index, 0.U, !io.stall)
  io.rep.tag := PipelineReg(io.tag.tag, 0.U, !io.stall)
}
