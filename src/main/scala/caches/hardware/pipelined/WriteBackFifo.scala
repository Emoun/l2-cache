package caches.hardware.pipelined

import chisel3._
import chisel.lib.fifo._

class WbFifoEntryIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle {
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val wbData = Input(UInt(blockWidth.W))
}

class WbFifoPushIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle() {
  val push = Input(Bool())
  val pushEntry = new WbFifoEntryIO(tagWidth, indexWidth, blockWidth)
  val full = Output(Bool())
}

class WbFifoPopIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle() {
  val pop = Input(Bool())
  val popEntry = Flipped(new WbFifoEntryIO(tagWidth, indexWidth, blockWidth))
  val empty = Output(Bool())
}

class WbFifoIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle {
  val push = new WbFifoPushIO(tagWidth, indexWidth, blockWidth)
  val pop = new WbFifoPopIO(tagWidth, indexWidth, blockWidth)
}

class WriteBackFifo(queueDepth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new WbFifoIO(tagWidth, indexWidth, blockWidth))

  // If need be, the data queue can be turned in to a mem based fifo
  val wbDataFifo = Module(new RegFifo(UInt(blockWidth.W), queueDepth))
  val tagFifo = Module(new RegFifo(UInt(tagWidth.W), queueDepth))
  val indexFifo = Module(new RegFifo(UInt(indexWidth.W), queueDepth))

  val queueFull = !wbDataFifo.io.enq.ready || !tagFifo.io.enq.ready || !indexFifo.io.enq.ready
  val queueEmpty = !wbDataFifo.io.deq.valid || !tagFifo.io.deq.valid || !indexFifo.io.deq.valid

  wbDataFifo.io.enq.valid := io.push.push
  tagFifo.io.enq.valid := io.push.push
  indexFifo.io.enq.valid := io.push.push
  tagFifo.io.enq.bits := io.push.pushEntry.tag
  indexFifo.io.enq.bits := io.push.pushEntry.index
  wbDataFifo.io.enq.bits := io.push.pushEntry.wbData

  wbDataFifo.io.deq.ready := io.pop.pop
  tagFifo.io.deq.ready := io.pop.pop
  indexFifo.io.deq.ready := io.pop.pop
  io.pop.popEntry.tag := tagFifo.io.deq.bits
  io.pop.popEntry.index := indexFifo.io.deq.bits
  io.pop.popEntry.wbData := wbDataFifo.io.deq.bits

  io.push.full := queueFull
  io.pop.empty := queueEmpty
}
