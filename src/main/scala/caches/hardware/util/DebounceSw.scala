package caches.hardware.util

import chisel3._
import chisel3.util._

class DebounceSw(dataSize: Int, freq: Int) extends Module {
  val io = IO(new Bundle {
    val sw = Input(UInt(dataSize.W))
    val swDb = Output(UInt(dataSize.W))
  })

  def syncIn(din: UInt): UInt = RegNext(RegNext(din))

  def tickGen(fac: Int): Bool = {
    val cntReg = RegInit(0.U(log2Up(fac).W))
    val tick = cntReg === (fac - 1).U
    cntReg := Mux(tick, 0.U, cntReg + 1.U)
    tick
  }

  // Synchronized inputs
  val syncData = syncIn(io.sw)

  // Debounced inputs
  val debDataReg = RegInit(0.U(dataSize.W))

  // Tick generation
  val tick = tickGen(freq / 100)

  // Debounce Inputs
  when(tick) {
    debDataReg := syncData
  }

  io.swDb := debDataReg
}
