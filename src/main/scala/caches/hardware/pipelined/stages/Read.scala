package caches.hardware.pipelined.stages

import caches.hardware.pipelined.{CacheMemory, WbFifoPushIO}
import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

class DirtyCtrlIO(nWays: Int, indexWidth: Int) extends Bundle {
  val set = Output(Bool())
  val unset = Output(Bool())
  val wWay = Output(UInt(log2Up(nWays).W))
  val wIndex = Output(UInt(indexWidth.W))
}

class ReadIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((blockWidth / 8).W))
  val repValid = Input(Bool())
  val isHit = Input(Bool())
  val hitWay = Input(UInt(log2Up(nWays).W))
  val repWay = Input(UInt(log2Up(nWays).W))
  val isRepDirty = Input(Bool())
  val dirtyTag = Input(UInt(tagWidth.W))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val index = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
}

class Read(memSizeInBytes: Int, nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val read = new ReadIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, blockWidth, subBlockWidth)
    val memUpdate = Flipped(new CacheMemUpdateIO(nWays = nWays, indexWidth = indexWidth, nSubBlocks = blockWidth / subBlockWidth, subBlockWidth = subBlockWidth))
    val stall = Input(Bool())
    val wbQueue = Flipped(new WbFifoPushIO(tagWidth = tagWidth, indexWidth = indexWidth, blockWidth = blockWidth))
    val update = Flipped(new UpdateIO(nCores = nCores, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockWidth = blockWidth, subBlockWidth = subBlockWidth))
    val dirtyCtrl = new DirtyCtrlIO(nWays = nWays, indexWidth = indexWidth)
  })

  val dataMem = Module(new CacheMemory(memSizeInBytes, nWays, blockWidth / 8, subBlockWidth / 8))

  dataMem.io.rIndex := io.read.index
  dataMem.io.rWayIdx := Mux(io.read.isHit, io.read.hitWay, io.read.repWay)
  dataMem.io.wrIndex := io.memUpdate.index
  dataMem.io.wrWayIdx := io.memUpdate.way
  dataMem.io.wrEn := io.memUpdate.wrEn
  dataMem.io.wrData := io.memUpdate.memWriteData
  dataMem.io.byteMask := io.memUpdate.byteMask
  dataMem.io.stall := io.stall

  val coreIdReg = PipelineReg(io.read.coreId, 0.U, !io.stall)
  val reqValidReg = PipelineReg(io.read.reqValid, false.B, !io.stall)
  val reqIdReg = PipelineReg(io.read.reqId, 0.U, !io.stall)
  val reqRwReg = PipelineReg(io.read.reqRw, false.B, !io.stall)
  val wDataReg = PipelineReg(io.read.wData, 0.U, !io.stall)
  val byteEnReg = PipelineReg(io.read.byteEn, 0.U, !io.stall)
  val repValidReg = PipelineReg(io.read.repValid, true.B, !io.stall)
  val isHitReg = PipelineReg(io.read.isHit, false.B, !io.stall)
  val hitWayReg = PipelineReg(io.read.hitWay, 0.U, !io.stall)
  val repWayReg = PipelineReg(io.read.repWay, 0.U, !io.stall)
  val isRepDirtyReg = PipelineReg(io.read.isRepDirty, false.B, !io.stall)
  val dirtyTagReg = PipelineReg(io.read.dirtyTag, 0.U, !io.stall)
  val blockOffsetReg = PipelineReg(io.read.blockOffset, 0.U, !io.stall)
  val indexReg = PipelineReg(io.read.index, 0.U, !io.stall)
  val tagReg = PipelineReg(io.read.tag, 0.U, !io.stall)

  val wb = reqValidReg && !isHitReg && isRepDirtyReg && repValidReg

  io.wbQueue.push := wb && !io.stall
  io.wbQueue.pushEntry.wbData := dataMem.io.rData.asUInt
  io.wbQueue.pushEntry.tag := dirtyTagReg
  io.wbQueue.pushEntry.index := indexReg

  io.dirtyCtrl.unset := wb
  io.dirtyCtrl.set := reqValidReg && reqRwReg && (isHitReg || repValidReg)
  io.dirtyCtrl.wIndex := indexReg
  io.dirtyCtrl.wWay := Mux(isHitReg, hitWayReg, repWayReg)

  io.update.valid := reqValidReg
  io.update.isHit := isHitReg
  io.update.rw := reqRwReg
  io.update.coreId := coreIdReg
  io.update.reqId := reqIdReg
  io.update.wData := wDataReg
  io.update.byteEn := byteEnReg
  io.update.wWay := hitWayReg
  io.update.repWay := repWayReg
  io.update.responseStatus := repValidReg
  io.update.blockOffset := blockOffsetReg
  io.update.index := indexReg
  io.update.tag := tagReg
  io.update.memReadData := dataMem.io.rData
}
