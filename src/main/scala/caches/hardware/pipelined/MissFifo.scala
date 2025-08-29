package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import caches.hardware.util._

class CacheCmdIO(nCores: Int, reqIdWidth: Int, blockOffsetWidth: Int) extends Bundle {
  val coreIdWidth = log2Up(nCores)

  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(coreIdWidth.W))
  val blockOffset = Input(UInt(blockOffsetWidth.W))
}

class LineRequestIO(nWays: Int, tagWidth: Int, indexWidth: Int, dataWidth: Int) extends Bundle {
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val byteEn = Input(UInt((dataWidth / 8).W))
  val replaceWay = Input(UInt(log2Up(nWays).W))
}

class MshrInfoIO(nMshrs: Int, nWays: Int, indexWidth: Int, tagWidth: Int) extends Bundle {
  val currentIndexes = Output(Vec(nMshrs, UInt(indexWidth.W)))
  val currentTags = Output(Vec(nMshrs, UInt(tagWidth.W)))
  val replacementWays = Output(Vec(nMshrs, UInt(log2Up(nWays).W)))
  val validMSHRs = Output(Vec(nMshrs, Bool()))
  val fullCmds = Output(Vec(nMshrs, Bool()))
  val wrPtr = Output(UInt(log2Ceil(nMshrs).W))
}

class MissFifoPushIO(nCores: Int, nMshrs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle() {
  // For inserting a new MSHR entry
  val pushReq = Input(Bool())
  val withCmd = Input(Bool()) // If true, the pushReqEntry will be pushed with a command
  val pushReqEntry = new LineRequestIO(nWays, tagWidth, indexWidth, subBlockWidth)
  // For pushing new command into MSHR entry
  val pushCmd = Input(Bool())
  val mshrIdx = Input(UInt(log2Up(nMshrs).W))
  val pushCmdEntry = new CacheCmdIO(nCores, reqIdWidth, blockOffsetWidth)
  // For updating the byte enable mask of the MSHR entry
  val updateByteEn = Input(Bool())
  val updateByteEnRow = Input(UInt(log2Up(nMshrs).W))
  val updateByteEnCol = Input(UInt(blockOffsetWidth.W))
  val updateByteEnVal = Input(UInt((subBlockWidth / 8).W))
  // Info about the current state of the MSHR array
  val info = new MshrInfoIO(nMshrs, nWays, indexWidth, tagWidth)
  val full = Output(Bool())
}

class MissFifoPopIO(nCores: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int) extends Bundle() {
  val pop = Input(Bool())
  val popEntry = Flipped(new LineRequestIO(nWays, tagWidth, indexWidth, blockWidth))
  val cmds = Output(Vec(nCmds, new CacheCmdIO(nCores, reqIdWidth, blockOffsetWidth)))
  val cmdCnt = Output(UInt((log2Up(nCmds) + 1).W))
  val empty = Output(Bool())
}

class MissFifoIO(nCores: Int, nMSHRs: Int, nCmds: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  val push = new MissFifoPushIO(nCores, nMSHRs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
  val pop = new MissFifoPopIO(nCores, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth)
}

class RequestMshrQueue(nMshrs: Int, nWays: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val pushEntry = new LineRequestIO(nWays, tagWidth, indexWidth, blockWidth)
    val blockOffset = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
    val updateByteEn = Input(Bool())
    val updateByteEnRow = Input(UInt(log2Up(nMshrs).W))
    val updateByteEnCol = Input(UInt(log2Up(blockWidth / subBlockWidth).W))
    val updateByteEnVal = Input(UInt((subBlockWidth / 8).W))
    val pop = Input(Bool())
    val popEntry = Flipped(new LineRequestIO(nWays, tagWidth, indexWidth, blockWidth))
    val empty = Output(Bool())
    val full = Output(Bool())
    val wrPtr = Output(UInt(log2Up(nMshrs).W))
    val rdPtr = Output(UInt(log2Up(nMshrs).W))
    val currentIndexes = Output(Vec(nMshrs, UInt(indexWidth.W)))
    val currentTags = Output(Vec(nMshrs, UInt(tagWidth.W)))
    val replacementWays = Output(Vec(nMshrs, UInt(log2Up(nWays).W)))
    val validMSHRs = Output(Vec(nMshrs, Bool()))
  })

  val nSubBlocks = blockWidth / subBlockWidth

  val tagQueue = Module(new RegFifoWithStatus(UInt(tagWidth.W), nMshrs))
  val idxQueue = Module(new RegFifoWithStatus(UInt(indexWidth.W), nMshrs))
  val wayQueue = Module(new RegFifoWithStatus(UInt(log2Up(nWays).W), nMshrs))
  val currWrPtr = tagQueue.io.wrPtr
  val currRdPtr = tagQueue.io.rdPtr

  val byteEnQueue = Module(new BlockRegFifoWithUpdate(blockWidth / 8, nSubBlocks, nMshrs,
    updateFun = (newData, oldData) => {
      // Update the byte enable mask for the sub-blocks
      val newByteEn = WireDefault(oldData)
      newByteEn := oldData | newData
      newByteEn
    }
  ))

  val full = !tagQueue.io.enq.ready || !idxQueue.io.enq.ready
  val empty = !tagQueue.io.deq.valid || !idxQueue.io.deq.valid

  // Connections for pushing data
  tagQueue.io.enq.valid := io.push
  tagQueue.io.enq.bits := io.pushEntry.tag

  idxQueue.io.enq.valid := io.push
  idxQueue.io.enq.bits := io.pushEntry.index

  wayQueue.io.enq.valid := io.push
  wayQueue.io.enq.bits := io.pushEntry.replaceWay

  val byteShift = Cat(io.blockOffset, 0.U(log2Up(subBlockWidth / 8).W))
  val byteMask = (io.pushEntry.byteEn << byteShift).asUInt
  val blockAlignedByteEn = byteMask((blockWidth / 8) - 1, 0)

  byteEnQueue.io.push := io.push
  byteEnQueue.io.update := io.updateByteEn
  byteEnQueue.io.rdPtr := currRdPtr
  byteEnQueue.io.wrPtr := currWrPtr
  byteEnQueue.io.wrData := blockAlignedByteEn
  byteEnQueue.io.updtPtr := io.updateByteEnRow
  byteEnQueue.io.updtBlockIdx := io.updateByteEnCol
  byteEnQueue.io.updtData := io.updateByteEnVal

  // Connections for popping data
  wayQueue.io.deq.ready := io.pop
  tagQueue.io.deq.ready := io.pop
  idxQueue.io.deq.ready := io.pop

  io.popEntry.tag := tagQueue.io.deq.bits
  io.popEntry.index := idxQueue.io.deq.bits
  io.popEntry.replaceWay := wayQueue.io.deq.bits
  io.popEntry.byteEn := byteEnQueue.io.rdData

  io.empty := empty
  io.full := full

  io.rdPtr := currRdPtr
  io.wrPtr := currWrPtr

  io.validMSHRs := tagQueue.io.validRegs
  io.currentTags := tagQueue.io.regOut
  io.currentIndexes := idxQueue.io.regOut
  io.replacementWays := wayQueue.io.regOut
}

class CmdMshrQueue(nCmds: Int, nCores: Int, nMshrs: Int, reqIdWidth: Int, blockOffsetWidth: Int) extends Module {
  val cmdWidth = log2Up(nCores) + reqIdWidth + blockOffsetWidth

  val io = IO(new Bundle {
    val push = Input(Bool())
    val withCmd = Input(Bool())
    val update = Input(Bool())
    val rdPtr = Input(UInt(log2Up(nMshrs).W))
    val wrPtr = Input(UInt(log2Up(nMshrs).W))
    val wrData = Input(UInt((cmdWidth * nCmds).W))
    val updtPtr = Input(UInt(log2Up(nMshrs).W))
    val updtData = Input(UInt(cmdWidth.W))
    val rdCmds = Output(Vec(nCmds, new CacheCmdIO(nCores, reqIdWidth, blockOffsetWidth)))
    val cmdCnt = Output(UInt((log2Up(nCmds) + 1).W))
    val full = Output(Vec(nMshrs, Bool()))
  })

  val cntRegs = RegInit(VecInit(Seq.fill(nMshrs)(0.U((log2Up(nCmds) + 1).W))))

  when(io.push) {
    cntRegs(io.wrPtr) := Mux(io.withCmd, 1.U, 0.U)
  } .elsewhen(io.update) {
    cntRegs(io.updtPtr) := cntRegs(io.updtPtr) + 1.U
  }

  val cmdBlockQueue = Module(new BlockRegFifoWithUpdate(cmdWidth * nCmds, nCmds, nMshrs))

  cmdBlockQueue.io.push := io.push
  cmdBlockQueue.io.update := io.update
  cmdBlockQueue.io.rdPtr := io.rdPtr
  cmdBlockQueue.io.wrPtr := io.wrPtr
  cmdBlockQueue.io.wrData := Mux(io.withCmd, io.wrData, 0.U)
  cmdBlockQueue.io.updtPtr := io.updtPtr
  cmdBlockQueue.io.updtBlockIdx := cntRegs(io.updtPtr)
  cmdBlockQueue.io.updtData := io.updtData

  for (mshrIdx <- 0 until nMshrs) {
    io.full(mshrIdx) := cntRegs(mshrIdx) === nCmds.U
  }

  for (cmdIdx <- 0 until nCmds) {
    val cmdOffset = cmdIdx * cmdWidth
    io.rdCmds(cmdIdx).blockOffset := cmdBlockQueue.io.rdData(blockOffsetWidth - 1 + cmdOffset, cmdOffset)
    io.rdCmds(cmdIdx).coreId := cmdBlockQueue.io.rdData(log2Up(nCores) - 1 + blockOffsetWidth + cmdOffset, blockOffsetWidth + cmdOffset)
    io.rdCmds(cmdIdx).reqId := cmdBlockQueue.io.rdData(reqIdWidth - 1 + log2Up(nCores) + blockOffsetWidth + cmdOffset, log2Up(nCores) + blockOffsetWidth + cmdOffset)
  }

  io.cmdCnt := cntRegs(io.rdPtr)
}

class MissFifo(nCores: Int, nCmds: Int, nMshrs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new MissFifoIO(nCores, nMshrs, nCmds, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, blockWidth, subBlockWidth))

  val reqQueue = Module(new RequestMshrQueue(nMshrs, nWays, tagWidth, indexWidth, blockWidth, subBlockWidth))
  val cmdQueue = Module(new CmdMshrQueue(nCmds, nCores, nMshrs, reqIdWidth, blockOffsetWidth))

  reqQueue.io.push := io.push.pushReq
  reqQueue.io.pushEntry := io.push.pushReqEntry
  reqQueue.io.blockOffset := io.push.pushCmdEntry.blockOffset
  reqQueue.io.updateByteEn := io.push.updateByteEn
  reqQueue.io.updateByteEnRow := io.push.updateByteEnRow
  reqQueue.io.updateByteEnCol := io.push.updateByteEnCol
  reqQueue.io.updateByteEnVal := io.push.updateByteEnVal
  reqQueue.io.pop := io.pop.pop

  cmdQueue.io.push := io.push.pushReq
  cmdQueue.io.withCmd := io.push.withCmd
  cmdQueue.io.update := io.push.pushCmd
  cmdQueue.io.rdPtr := reqQueue.io.rdPtr
  cmdQueue.io.wrPtr := reqQueue.io.wrPtr
  cmdQueue.io.wrData := Cat(io.push.pushCmdEntry.reqId, io.push.pushCmdEntry.coreId, io.push.pushCmdEntry.blockOffset)
  cmdQueue.io.updtPtr := io.push.mshrIdx
  cmdQueue.io.updtData := Cat(io.push.pushCmdEntry.reqId, io.push.pushCmdEntry.coreId, io.push.pushCmdEntry.blockOffset)

  io.push.full := reqQueue.io.full
  io.push.info.wrPtr := reqQueue.io.wrPtr
  io.push.info.validMSHRs := reqQueue.io.validMSHRs
  io.push.info.currentTags := reqQueue.io.currentTags
  io.push.info.currentIndexes := reqQueue.io.currentIndexes
  io.push.info.replacementWays := reqQueue.io.replacementWays
  io.push.info.fullCmds := cmdQueue.io.full

  io.pop.empty := reqQueue.io.empty
  io.pop.cmdCnt := cmdQueue.io.cmdCnt
  io.pop.popEntry.tag := reqQueue.io.popEntry.tag
  io.pop.popEntry.index := reqQueue.io.popEntry.index
  io.pop.popEntry.replaceWay := reqQueue.io.popEntry.replaceWay
  io.pop.popEntry.byteEn := reqQueue.io.popEntry.byteEn
  io.pop.cmds := cmdQueue.io.rdCmds
}