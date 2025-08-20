package caches.hardware.util

import chisel3._
import chisel3.util._

class DecoupledMux(dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val dec1 = Flipped(Decoupled(UInt(dataWidth.W)))
    val dec2 = Flipped(Decoupled(UInt(dataWidth.W)))
    val sel = Input(Bool())
    val out = Decoupled(UInt(dataWidth.W))
  })

  io.out.bits := Mux(io.sel, io.dec2.bits, io.dec1.bits)
  io.out.valid := Mux(io.sel, io.dec2.valid, io.dec1.valid)
  io.dec1.ready := Mux(!io.sel, io.out.ready, false.B)
  io.dec2.ready := Mux(io.sel, io.out.ready, false.B)
}

object DecoupledMux {
  def apply(dec1: DecoupledIO[UInt], dec2: DecoupledIO[UInt], sel: Bool): DecoupledIO[UInt] = {
    val mux = Module(new DecoupledMux(dec1.bits.getWidth))
    mux.io.dec1 <> dec1
    mux.io.dec2 <> dec2
    mux.io.sel := sel
    mux.io.out
  }
}
