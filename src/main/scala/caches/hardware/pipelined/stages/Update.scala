package caches.hardware.pipelined.stages

import caches.hardware.pipelined.CacheResponseIO
import chisel3._
import chisel3.util._

/**
 * IO interface from the read stage to the update stage
 */
class UpdateIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val valid = Input(Bool())
  val isHit = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val rw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((subBlockWidth / 8).W))
  val wWay = Input(UInt(log2Up(nWays).W))
  val repWay = Input(UInt(log2Up(nWays).W))
  val responseStatus = Input(UInt(1.W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
  val memReadData = Input(Vec(blockWidth / subBlockWidth, UInt(subBlockWidth.W)))
}

/**
 * IO interface from the memory interface to the update stage
 */
class MemInterfaceToUpdateIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle() {
  val valid = Input(Bool())
  val rw = Input(Bool())
  val byteEnInverse = Input(UInt((subBlockWidth / 8).W))
  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val wWay = Input(UInt(log2Up(nWays).W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
  val memReadData = Input(Vec(blockWidth / subBlockWidth, UInt(subBlockWidth.W)))
}

/**
 * IO interface from the update stage to the tag stage
 */
class PipelineCtrlIO(nWays: Int, tagWidth: Int) extends Bundle() {
  val tag = Output(UInt(tagWidth.W))
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(log2Up(nWays).W))
  val refill = Output(Bool()) // For setting the line as valid
  val invalidate = Output(Bool()) // For setting the line as invalid
}

class TagUpdateIO(nWays: Int, tagWidth: Int) extends PipelineCtrlIO(nWays, tagWidth) {
  val update = Output(Bool()) // For setting the line as dirty
}

/**
 * IO interface for control signals from the update stage to the cache memory
 */
class CacheMemUpdateIO(nWays: Int, nSubBlocks: Int, subBlockWidth: Int) extends Bundle() {
  val wrEn = Output(Bool())
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(log2Up(nWays).W))
  val memWriteData = Output(Vec(nSubBlocks, UInt(subBlockWidth.W)))
  val byteMask = Output(UInt(((nSubBlocks * subBlockWidth) / 8).W))
}

// TODO: No need to respond to a write request, since it does not return any data back
class UpdateUnit(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  private val nSubBlocks = blockWidth / subBlockWidth

  val io = IO(new Bundle {
    val readStage = new UpdateIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val memoryInterface = new MemInterfaceToUpdateIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth)
    val coreResp = new CacheResponseIO(subBlockWidth, reqIdWidth)
    val memUpdate = new CacheMemUpdateIO(nWays, nSubBlocks, subBlockWidth)
    val pipelineCtrl = new PipelineCtrlIO(nWays, tagWidth)
    val tagUpdate = new TagUpdateIO(nWays, tagWidth)
    val outCoreId = Output(UInt(log2Up(nCores).W))
    val stall = Output(Bool())
  })

  val refill = WireDefault(false.B)
  val update = WireDefault(false.B)
  val invalidate = WireDefault(false.B)
  val stall = WireDefault(false.B)
  val wrEn = WireDefault(false.B)

  val updateTag = WireDefault(0.U(tagWidth.W))
  val updateIndex = WireDefault(0.U(log2Up(nWays).W))
  val updateWay = WireDefault(0.U(log2Up(nWays).W))
  val coreRespCoreId = WireDefault(0.U(log2Up(nCores).W))

  val coreRespValid = WireDefault(false.B)
  val coreRespId = WireDefault(0.U(reqIdWidth.W))
  val coreRespData = WireDefault(0.U(subBlockWidth.W))
  val coreRespStatus = WireDefault(0.U(1.W))
  val updateWriteData = VecInit(Seq.fill(nSubBlocks)(0.U(subBlockWidth.W)))
  val updateWriteByteMask = WireDefault(0.U((blockWidth / 8).W))

  when(io.memoryInterface.valid) {
    refill := true.B
    wrEn := true.B
    updateTag := io.memoryInterface.tag
    updateIndex := io.memoryInterface.index
    updateWay := io.memoryInterface.wWay

    updateWriteData := io.memoryInterface.memReadData
    coreRespData := io.memoryInterface.memReadData(io.memoryInterface.blockOffset)
    coreRespId := io.memoryInterface.reqId
    coreRespCoreId := io.memoryInterface.coreId
    coreRespStatus := 1.U
    coreRespValid := true.B

    when(io.memoryInterface.rw) {
      update := true.B
      // Disable writing to all the bytes except to the ones the core wrote to
      val byteShift = Cat(io.memoryInterface.blockOffset, 0.U(log2Up(subBlockWidth / 8).W))
      val byteMask = (io.memoryInterface.byteEnInverse << byteShift).asUInt
      updateWriteByteMask := byteMask((blockWidth / 8) - 1, 0)
    } .otherwise {
      updateWriteByteMask := ((1 << (blockWidth / 8)) - 1).U // All bytes enabled
    }
  } .elsewhen(io.readStage.valid) {
    when(io.readStage.isHit) {
      updateTag := io.readStage.tag
      updateIndex := io.readStage.index
      updateWay := io.readStage.wWay

      coreRespData := io.readStage.memReadData(io.readStage.blockOffset)
      coreRespId := io.readStage.reqId
      coreRespCoreId := io.readStage.coreId
      coreRespStatus := io.readStage.responseStatus
      coreRespValid := true.B

      when(io.readStage.rw) {
        update := true.B
        wrEn := true.B

        val byteShift = Cat(io.readStage.blockOffset, 0.U(log2Up(subBlockWidth / 8).W))
        val byteMask = (io.readStage.byteEn << byteShift).asUInt
        updateWriteByteMask := byteMask((blockWidth / 8) - 1, 0)
        updateWriteData(io.readStage.blockOffset) := io.readStage.wData
      }
    } .otherwise {
      updateTag := io.readStage.tag
      updateIndex := io.readStage.index
      updateWay := io.readStage.repWay

      // TODO: This is only used for a test, normally a rejected miss request would be kept in a queue
      //  until it can be serviced
      when(io.readStage.responseStatus === 0.U) {
        coreRespData := io.readStage.memReadData(io.readStage.blockOffset)
        coreRespId := io.readStage.reqId
        coreRespCoreId := io.readStage.coreId
        coreRespStatus := io.readStage.responseStatus
        coreRespValid := true.B
      }

      // If it is a wr request and a miss we invalidate the cache line, store the wData, and
      // wait for the remainder of the line to be brought in
      when(io.readStage.rw) {
        wrEn := true.B
        invalidate := true.B

        val byteShift = Cat(io.readStage.blockOffset, 0.U(log2Up(subBlockWidth / 8).W))
        val byteMask = (io.readStage.byteEn << byteShift).asUInt
        updateWriteByteMask := byteMask((blockWidth / 8) - 1, 0)
        updateWriteData(io.readStage.blockOffset) := io.readStage.wData
      }
    }
  }

  when (io.memoryInterface.valid && io.readStage.valid) {
    stall := true.B
  }

  io.stall := stall

  io.tagUpdate.tag := updateTag
  io.tagUpdate.index := updateIndex
  io.tagUpdate.way := updateWay
  io.tagUpdate.invalidate := invalidate
  io.tagUpdate.refill := refill
  io.tagUpdate.update := update

  io.pipelineCtrl.tag := updateTag
  io.pipelineCtrl.index := updateIndex
  io.pipelineCtrl.way := updateWay
  io.pipelineCtrl.invalidate := invalidate
  io.pipelineCtrl.refill := refill

  io.memUpdate.wrEn := wrEn
  io.memUpdate.way := updateWay
  io.memUpdate.index := updateIndex
  io.memUpdate.memWriteData := updateWriteData
  io.memUpdate.byteMask := updateWriteByteMask

  io.coreResp.reqId.valid := coreRespValid
  io.coreResp.reqId.bits := coreRespId
  io.coreResp.rData := coreRespData
  io.coreResp.responseStatus := coreRespStatus
  io.outCoreId := coreRespCoreId
}
