package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import chisel.lib.fifo._
import chisel3.util.experimental.BoringUtils

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

class MissFifoEntryIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle {
  val rw = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val byteEnInverse = Input(UInt((subBlockWidth / 8).W))
  val coreId = Input(UInt(log2Up(nCores).W))
  val replaceWay = Input(UInt(log2Up(nWays).W))
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val blockOffset = Input(UInt(blockOffsetWidth.W))
}

// TODO: The tags are used to see if it is half-miss (a request needs a data that is currently being fetched) while ways are used to turn an access into a miss pre-emptively
class MissFifoPushIO(nCores: Int, nMSHRs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle() {
  val push = Input(Bool())
  val pushEntry = new MissFifoEntryIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
  val currentIndexes = Output(Vec(nMSHRs, UInt(indexWidth.W)))
  val currentTags = Output(Vec(nMSHRs, UInt(tagWidth.W)))
  val validMSHRs = Output(Vec(nMSHRs, Bool()))
  val full = Output(Bool())
}

class MissFifoPopIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle() {
  val pop = Input(Bool())
  val popEntry = Flipped(new MissFifoEntryIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth))
  val empty = Output(Bool())
}

class MissFifoIO(nCores: Int, nMSHRs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Bundle {
  val push = new MissFifoPushIO(nCores, nMSHRs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
  val pop = new MissFifoPopIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
}

class MissFifo(nCores: Int, nMSHRs: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, subBlockWidth: Int) extends Module {
  val io = IO(new MissFifoIO(nCores, nMSHRs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth))

  val rwQueue = Module(new RegFifo(Bool(), nMSHRs))
  val reqIdQueue = Module(new RegFifo(UInt(reqIdWidth.W), nMSHRs))
  val coreIdQueue = Module(new RegFifo(UInt(log2Up(nCores).W), nMSHRs))
  val byteEnInverseQueue = Module(new RegFifo(UInt((subBlockWidth / 8).W), nMSHRs))
  val wayQueue = Module(new RegFifo(UInt(log2Up(nWays).W), nMSHRs))

  val tagQueue = Module(new MshrQueue(UInt(tagWidth.W), nMSHRs))
  val idxQueue = Module(new MshrQueue(UInt(indexWidth.W), nMSHRs))
  val blockOffQueue = Module(new RegFifo(UInt(blockOffsetWidth.W), nMSHRs))

  val full = !rwQueue.io.enq.ready ||
             !reqIdQueue.io.enq.ready ||
             !coreIdQueue.io.enq.ready ||
             !byteEnInverseQueue.io.enq.ready ||
             !wayQueue.io.enq.ready ||
             !tagQueue.io.enq.ready ||
             !idxQueue.io.enq.ready ||
             !blockOffQueue.io.enq.ready

  val empty = !rwQueue.io.deq.valid ||
              !reqIdQueue.io.deq.valid ||
              !coreIdQueue.io.deq.valid ||
              !byteEnInverseQueue.io.deq.valid ||
              !wayQueue.io.deq.valid ||
              !tagQueue.io.deq.valid ||
              !idxQueue.io.deq.valid ||
              !blockOffQueue.io.deq.valid

  // Connections for pushing data
  rwQueue.io.enq.valid := io.push.push
  rwQueue.io.enq.bits := io.push.pushEntry.rw
  reqIdQueue.io.enq.valid := io.push.push
  reqIdQueue.io.enq.bits := io.push.pushEntry.reqId
  coreIdQueue.io.enq.valid := io.push.push
  coreIdQueue.io.enq.bits := io.push.pushEntry.coreId
  byteEnInverseQueue.io.enq.valid := io.push.push
  byteEnInverseQueue.io.enq.bits := io.push.pushEntry.byteEnInverse
  wayQueue.io.enq.valid := io.push.push
  wayQueue.io.enq.bits := io.push.pushEntry.replaceWay
  tagQueue.io.enq.valid := io.push.push
  tagQueue.io.enq.bits := io.push.pushEntry.tag
  idxQueue.io.enq.valid := io.push.push
  idxQueue.io.enq.bits := io.push.pushEntry.index
  blockOffQueue.io.enq.valid := io.push.push
  blockOffQueue.io.enq.bits := io.push.pushEntry.blockOffset

  // Connections for popping data
  rwQueue.io.deq.ready := io.pop.pop
  reqIdQueue.io.deq.ready := io.pop.pop
  coreIdQueue.io.deq.ready := io.pop.pop
  byteEnInverseQueue.io.deq.ready := io.pop.pop
  wayQueue.io.deq.ready := io.pop.pop
  tagQueue.io.deq.ready := io.pop.pop
  idxQueue.io.deq.ready := io.pop.pop
  blockOffQueue.io.deq.ready := io.pop.pop

  for (i <- 0 until nMSHRs) {
    io.push.currentTags(i) := tagQueue.io.regOut(i)
    io.push.currentIndexes(i) := idxQueue.io.regOut(i)
  }

  io.push.full := full
  io.push.validMSHRs := tagQueue.io.validRegs

  io.pop.empty := empty
  io.pop.popEntry.rw := rwQueue.io.deq.bits
  io.pop.popEntry.reqId := reqIdQueue.io.deq.bits
  io.pop.popEntry.coreId := coreIdQueue.io.deq.bits
  io.pop.popEntry.byteEnInverse := byteEnInverseQueue.io.deq.bits
  io.pop.popEntry.replaceWay := wayQueue.io.deq.bits
  io.pop.popEntry.tag := tagQueue.io.deq.bits
  io.pop.popEntry.index := idxQueue.io.deq.bits
  io.pop.popEntry.blockOffset := blockOffQueue.io.deq.bits
}