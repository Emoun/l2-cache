package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._

class CacheUpdateEntryIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val valid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val rw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val wWay = Input(UInt(log2Up(nWays).W))
  val responseStatus = Input(UInt(1.W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
  val memReadData = Input(Vec(blockWidth / subBlockWidth, UInt(subBlockWidth.W)))
}

class TagUpdateIO(nWays: Int, tagWidth: Int) extends Bundle() {
  val tag = Output(UInt(tagWidth.W))
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(log2Up(nWays).W))
  val update = Output(Bool())
  val refill = Output(Bool())
}

class CacheMemUpdateIO(nWays: Int, nSubBlocks: Int, subBlockWidth: Int) extends Bundle() {
  val wrEn = Output(Bool())
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(log2Up(nWays).W))
  val memWriteData = Output(Vec(nSubBlocks, UInt(subBlockWidth.W)))
}

class UpdateUnit(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  private val nSubBlocks = blockWidth / subBlockWidth

  val io = IO(new Bundle {
    val readStage = new CacheUpdateEntryIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val memoryInterface = new CacheUpdateEntryIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val coreResp = new CacheResponseIO(subBlockWidth, reqIdWidth)
    val outCoreId = Output(UInt(log2Up(nCores).W))
    val memUpdate = new CacheMemUpdateIO(nWays, nSubBlocks, subBlockWidth)
    val tagUpdate = new TagUpdateIO(nWays, tagWidth)
    val stall = Output(Bool())
  })

  val tag = WireDefault(0.U(tagWidth.W))
  val index = WireDefault(0.U(log2Up(nWays).W))
  val way = WireDefault(0.U(log2Up(nWays).W))
  val refill = WireDefault(false.B)
  val update = WireDefault(false.B)
  val stall = WireDefault(false.B)
  val coreId = WireDefault(0.U(log2Up(nCores).W))
  val respReqId = WireDefault(0.U(reqIdWidth.W))
  val respRData = WireDefault(0.U(subBlockWidth.W))
  val responseStatus = WireDefault(0.U(1.W))
  val wrEn = WireDefault(false.B)
  val cacheWriteData = VecInit(Seq.fill(nSubBlocks)(0.U(subBlockWidth.W)))

  when(io.memoryInterface.valid) {
    refill := true.B
    tag := io.memoryInterface.tag
    index := io.memoryInterface.index
    way := io.memoryInterface.wWay
    wrEn := true.B

    when(io.memoryInterface.rw) {
      update := true.B

      for (i <- 0 until nSubBlocks) {
        cacheWriteData(i) := Mux(io.memoryInterface.blockOffset === i.U, io.memoryInterface.wData, io.memoryInterface.memReadData(i))
      }

    }.otherwise {
      cacheWriteData := io.memoryInterface.memReadData
    }

    respRData := io.memoryInterface.memReadData(io.memoryInterface.blockOffset)
    respReqId := io.memoryInterface.reqId
    coreId := io.memoryInterface.coreId
    responseStatus := io.memoryInterface.responseStatus
  }.elsewhen(io.readStage.valid) {
    tag := io.readStage.tag
    index := io.readStage.index
    way := io.readStage.wWay

    when(io.readStage.rw) {
      update := true.B
      wrEn := true.B

      for (i <- 0 until nSubBlocks) {
        cacheWriteData(i) := Mux(io.readStage.blockOffset === i.U, io.readStage.wData, io.readStage.memReadData(i))
      }
    }

    respRData := io.readStage.memReadData(io.readStage.blockOffset)
    respReqId := io.readStage.reqId
    coreId := io.readStage.coreId
    responseStatus := io.readStage.responseStatus
  }

  when (io.memoryInterface.valid && io.readStage.valid) {
    stall := true.B
  }

  io.stall := stall

  io.tagUpdate.tag := tag
  io.tagUpdate.index := index
  io.tagUpdate.way := way
  io.tagUpdate.refill := refill
  io.tagUpdate.update := update

  io.memUpdate.memWriteData := cacheWriteData
  io.memUpdate.wrEn := wrEn
  io.memUpdate.way := way
  io.memUpdate.index := index

  io.coreResp.reqId.valid := io.readStage.valid || io.memoryInterface.valid
  io.coreResp.reqId.bits := respReqId
  io.coreResp.rData := respRData
  io.coreResp.responseStatus := responseStatus
  io.outCoreId := coreId
}
