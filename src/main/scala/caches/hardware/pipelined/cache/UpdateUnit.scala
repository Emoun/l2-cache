package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._

class CacheUpdateEntryIO(nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val valid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val rw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val wWay = Input(UInt(log2Up(nWays).W))
  val responseStatus = Input(UInt(1.W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
  val memReadData = Input(Vec(blockWidth / subBlockWidth, UInt(subBlockWidth.W)))
}

class CacheUpdateControlIO(nWays: Int, tagWidth: Int, nSubBlocks: Int, subBlockWidth: Int) extends Bundle {
  val tag = Output(UInt(tagWidth.W))
  val index = Output(UInt(log2Up(nWays).W))
  val way = Output(UInt(log2Up(nWays).W))
  val refill = Output(Bool())
  val update = Output(Bool())
  val stall = Output(Bool())
  val memWriteData = Output(Vec(nSubBlocks, UInt(subBlockWidth.W)))
  val wrEn = Output(Bool())
}

class UpdateUnit(nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  private val nSubBlocks = blockWidth / subBlockWidth

  val io = IO(new Bundle {
    val readStage = new CacheUpdateEntryIO(nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val memoryInterface = new CacheUpdateEntryIO(nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val cacheUpdateControl = new CacheUpdateControlIO(nWays, tagWidth, nSubBlocks, subBlockWidth)
    val coreResp = new CacheResponseIO(subBlockWidth, reqIdWidth)
  })

  val tag = WireDefault(0.U(tagWidth.W))
  val index = WireDefault(0.U(log2Up(nWays).W))
  val way = WireDefault(0.U(log2Up(nWays).W))
  val refill = WireDefault(false.B)
  val update = WireDefault(false.B)
  val stall = WireDefault(false.B)
  val respReqId = WireDefault(0.U(reqIdWidth.W))
  val respRData = WireDefault(0.U(subBlockWidth.W))
  val responseStatus = WireDefault(0.U(1.W))
  val wrEn = WireDefault(false.B)
  val cacheWriteData = VecInit(Seq.fill(nSubBlocks)(0.U(subBlockWidth.W)))

  // TODO: Stall pipeline if core is not ready for accepting a response

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
    responseStatus := io.readStage.responseStatus
  }

  when (io.memoryInterface.valid && io.readStage.valid) {
    stall := true.B
  }

  io.cacheUpdateControl.stall := stall
  io.cacheUpdateControl.tag := tag
  io.cacheUpdateControl.index := index
  io.cacheUpdateControl.way := way
  io.cacheUpdateControl.refill := refill
  io.cacheUpdateControl.update := update
  io.cacheUpdateControl.memWriteData := cacheWriteData
  io.cacheUpdateControl.wrEn := wrEn

  io.coreResp.reqId.valid := io.readStage.valid || io.memoryInterface.valid
  io.coreResp.reqId.bits := respReqId
  io.coreResp.rData := respRData
  io.coreResp.responseStatus := responseStatus
}
