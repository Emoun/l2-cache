package caches.hardware.pipelined.cache

import chisel3._
import chisel.lib.fifo.{MemFifo, RegFifo}

class WbFifoEntryIO(tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Bundle {
  val tag = Input(UInt(tagWidth.W))
  val index = Input(UInt(indexWidth.W))
  val wbData = Input(UInt(blockWidth.W))
}

class WriteBackFifo(queueDepth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int) extends Module {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val pop = Input(Bool())
    val pushEntry = new WbFifoEntryIO(tagWidth, indexWidth, blockWidth)
    val popEntry = Flipped(new WbFifoEntryIO(tagWidth, indexWidth, blockWidth))
    val full = Output(Bool())
    val empty = Output(Bool())
  })

  val wbDataFifo = Module(new MemFifo(UInt(blockWidth.W), queueDepth)) // TODO: Synthesis cannot synthesize this to memory due to incorrect memory size
  val tagFifo = Module(new RegFifo(UInt(tagWidth.W), queueDepth))
  val indexFifo = Module(new RegFifo(UInt(indexWidth.W), queueDepth))

  // Need to delay the output of the register fifos to match the timing of the memory based fifo
  val queueFull = !wbDataFifo.io.enq.ready || !tagFifo.io.enq.ready || !indexFifo.io.enq.ready
  val queueEmpty = !wbDataFifo.io.deq.valid || !tagFifo.io.deq.valid || !indexFifo.io.deq.valid

  wbDataFifo.io.enq.valid := io.push
  tagFifo.io.enq.valid := io.push
  indexFifo.io.enq.valid := io.push
  tagFifo.io.enq.bits := io.pushEntry.tag
  indexFifo.io.enq.bits := io.pushEntry.index
  wbDataFifo.io.enq.bits := io.pushEntry.wbData

  wbDataFifo.io.deq.ready := io.pop
  tagFifo.io.deq.ready := io.pop
  indexFifo.io.deq.ready := io.pop
  io.popEntry.tag := RegNext(tagFifo.io.deq.bits, 0.U)
  io.popEntry.index := RegNext(indexFifo.io.deq.bits, 0.U)
  io.popEntry.wbData := wbDataFifo.io.deq.bits

  io.full := queueFull
  io.empty := queueEmpty
}
