package caches.hardware.util

import chisel3._
import chisel3.util.log2Ceil
import chisel3.util.experimental.BoringUtils
import chisel.lib.fifo.{FifoIO, RegFifo}

class RegFifoWithStatusIO[T <: Data](gen: T, depth: Int) extends FifoIO(gen) {
  val regOut = Output(Vec(depth, gen))
  val validRegs = Output(Vec(depth, Bool()))
  val wrPtr = Output(UInt(log2Ceil(depth).W))
  val rdPtr = Output(UInt(log2Ceil(depth).W))
}

/**
 * A register based fifo with outputs of the current register contents along with the valid status of each register.
 */
class RegFifoWithStatus[T <: Data](gen: T, depth: Int) extends Module {
  val io = IO(new RegFifoWithStatusIO(gen, depth))

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

  io.wrPtr := wrPtr
  io.rdPtr := rdPtr
  io.validRegs := validRegs
  io.regOut <> memReg
  io.deq <> fifo.io.deq
  io.enq <> fifo.io.enq
}
