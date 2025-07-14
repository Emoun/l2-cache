package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._
import chisel.lib.fifo._
import chisel3.util.experimental.BoringUtils

class MissFifoEntryIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle {
  val rw = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val wData = Input(UInt(subBlockWidth.W))
  val replaceWay = Input(UInt(log2Up(nWays).W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(blockOffsetWidth.W))
}

class MshrFifoIO[T <: Data](gen: T, depth: Int) extends FifoIO(gen) {
  val regOut = Output(Vec(depth, gen))
  val validRegs = Output(Vec(depth, Bool()))
}

class MshrQueue[T <: Data](gen: T, depth: Int) extends Module {
  val io = IO(new MshrFifoIO(gen, depth))

  val fifo = Module(new RegFifo(gen, depth))
  val memReg = VecInit(Seq.fill(depth)(WireDefault(0.U.asTypeOf(gen))))
  val validRegs = RegInit(VecInit(Seq.fill(depth)(false.B)))

  val rdPtr = WireDefault(0.U(log2Ceil(depth).W))
  val wrPtr = WireDefault(0.U(log2Ceil(depth).W))
  val incrRead = WireDefault(false.B)
  val incrWrite = WireDefault(false.B)

  BoringUtils.bore(fifo.memReg, Seq(memReg))
  BoringUtils.bore(fifo.writePtr, Seq(wrPtr))
  BoringUtils.bore(fifo.readPtr, Seq(rdPtr))
  BoringUtils.bore(fifo.incrRead, Seq(incrRead))
  BoringUtils.bore(fifo.incrWrite, Seq(incrWrite))

  when(incrRead) {
    validRegs(rdPtr) := false.B
  }

  when(incrWrite) {
    validRegs(wrPtr) := true.B
  }

  io.validRegs := validRegs
  io.regOut <> memReg
  io.deq <> fifo.io.deq
  io.enq <> fifo.io.enq
}

class MissFifo(nCores: Int, nMSHRs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val pushEntry = new MissFifoEntryIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
    val pop = Input(Bool())
    val popEntry = Flipped(new MissFifoEntryIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth))
    val currentIndexes = Output(Vec(nMSHRs, UInt(indexWidth.W)))
    val currentWays = Output(Vec(nMSHRs, UInt(log2Up(nWays).W)))
    val validMSHRs = Output(Vec(nMSHRs, Bool()))
    val full = Output(Bool())
    val empty = Output(Bool())
  })

  val rwQueue = Module(new RegFifo(Bool(), nMSHRs))
  val reqIdQueue = Module(new RegFifo(UInt(reqIdWidth.W), nMSHRs))
  val coreIdQueue = Module(new RegFifo(UInt(log2Up(nCores).W), nMSHRs))
  val wDataQueue = Module(new RegFifo(UInt(subBlockWidth.W), nMSHRs))
  val wayQueue = Module(new MshrQueue(UInt(log2Up(nWays).W), nMSHRs))
  val tagQueue = Module(new RegFifo(UInt(tagWidth.W), nMSHRs))
  val idxQueue = Module(new MshrQueue(UInt(indexWidth.W), nMSHRs))
  val blockOffQueue = Module(new RegFifo(UInt(blockOffsetWidth.W), nMSHRs))

  val full = !rwQueue.io.enq.ready ||
             !reqIdQueue.io.enq.ready ||
             !coreIdQueue.io.enq.ready ||
             !wDataQueue.io.enq.ready ||
             !wayQueue.io.enq.ready ||
             !tagQueue.io.enq.ready ||
             !idxQueue.io.enq.ready ||
             !blockOffQueue.io.enq.ready

  val empty = !rwQueue.io.deq.valid ||
              !reqIdQueue.io.deq.valid ||
              !coreIdQueue.io.deq.valid ||
              !wDataQueue.io.deq.valid ||
              !wayQueue.io.deq.valid ||
              !tagQueue.io.deq.valid ||
              !idxQueue.io.deq.valid ||
              !blockOffQueue.io.deq.valid

  // Connections for pushing data
  rwQueue.io.enq.valid := io.push
  rwQueue.io.enq.bits := io.pushEntry.rw
  reqIdQueue.io.enq.valid := io.push
  reqIdQueue.io.enq.bits := io.pushEntry.reqId
  coreIdQueue.io.enq.valid := io.push
  coreIdQueue.io.enq.bits := io.pushEntry.coreId
  wDataQueue.io.enq.valid := io.push
  wDataQueue.io.enq.bits := io.pushEntry.wData
  wayQueue.io.enq.valid := io.push
  wayQueue.io.enq.bits := io.pushEntry.replaceWay
  tagQueue.io.enq.valid := io.push
  tagQueue.io.enq.bits := io.pushEntry.tag
  idxQueue.io.enq.valid := io.push
  idxQueue.io.enq.bits := io.pushEntry.index
  blockOffQueue.io.enq.valid := io.push
  blockOffQueue.io.enq.bits := io.pushEntry.blockOffset

  // Connections for popping data
  rwQueue.io.deq.ready := io.pop
  reqIdQueue.io.deq.ready := io.pop
  coreIdQueue.io.deq.ready := io.pop
  wDataQueue.io.deq.ready := io.pop
  wayQueue.io.deq.ready := io.pop
  tagQueue.io.deq.ready := io.pop
  idxQueue.io.deq.ready := io.pop
  blockOffQueue.io.deq.ready := io.pop

  for (i <- 0 until nMSHRs) {
    io.currentWays(i) := wayQueue.io.regOut(i)
    io.currentIndexes(i) := idxQueue.io.regOut(i)
  }

  io.empty := empty
  io.full := full

  io.validMSHRs := wayQueue.io.validRegs
  io.popEntry.rw := rwQueue.io.deq.bits
  io.popEntry.reqId := reqIdQueue.io.deq.bits
  io.popEntry.coreId := coreIdQueue.io.deq.bits
  io.popEntry.wData := wDataQueue.io.deq.bits
  io.popEntry.replaceWay := wayQueue.io.deq.bits
  io.popEntry.tag := tagQueue.io.deq.bits
  io.popEntry.index := idxQueue.io.deq.bits
  io.popEntry.blockOffset := blockOffQueue.io.deq.bits
}