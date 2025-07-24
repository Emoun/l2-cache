package caches.hardware.util

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

class MemBlockIO(depth: Int, width: Int) extends Bundle {
  val readAddr = Input(UInt(log2Up(depth).W))
  val writeAddr = Input(UInt(log2Up(depth).W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
  val readData = Output(UInt(width.W))
}

/**
 * General purpose synchronous memory block.
 *
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 */
class MemBlock(depth: Int, width: Int, dataFile: Option[String] = None) extends Module {
  val io = IO(new MemBlockIO(depth, width))

  val readData = WireDefault(0.U(width.W))
  val mem = SyncReadMem(depth, UInt(width.W), SyncReadMem.ReadFirst)

  // Initialize memory block from a file
  if (dataFile.isDefined) {
    val file = dataFile.get

    if (file.trim().nonEmpty) { // If not empty path
      loadMemoryFromFileInline(mem, file)
    } else {
      println(s"Warning: The provided initialization file path is incorrect: $file")
    }
  }

  // Write
  when(io.wrEn) {
    mem.write(io.writeAddr, io.writeData)
  }

  // Read
  readData := mem.read(io.readAddr)

  val writeDataReg = RegNext(io.writeData)
  val forwardSelReg = RegNext((io.writeAddr === io.readAddr) && io.wrEn)
  io.readData := Mux(forwardSelReg, writeDataReg, readData)
}