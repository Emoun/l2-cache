package caches.hardware

import chisel3._
import chisel3.util.log2Up

class MemBlockIO(depth: Int, width: Int) extends Bundle {
  private val addrLen = log2Up(depth)

  val readAddr = Input(UInt(addrLen.W))
  val readData = Output(UInt(width.W))
  val writeAddr = Input(UInt(addrLen.W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
}

/**
 * General purpose synchronous memory block.
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 */
class MemBlock(depth: Int, width: Int) extends Module {
  val io = IO(new MemBlockIO(depth, width))

  val mem = SyncReadMem(depth, UInt(width.W))
  val readData = WireDefault(0.U(width.W))
  val writeDataReg = RegNext(io.writeData)
  val forwardSelReg = RegNext(io.writeAddr === io.readAddr && io.wrEn)

  readData := mem.read(io.readAddr)

  when(io.wrEn) {
    mem.write(io.writeAddr, io.writeData)
  }

  io.readData := Mux(forwardSelReg, writeDataReg, readData)
}
