package caches.hardware

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import java.io.File

class MemBlockIO(depth: Int, width: Int) extends Bundle {
  private val addrLen = log2Up(depth)

  val readAddr = Input(UInt(addrLen.W))
  val writeAddr = Input(UInt(addrLen.W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
  val readData = Output(UInt(width.W))
}

/**
 * General purpose synchronous memory block.
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 */
class MemBlock(depth: Int, width: Int, dataFile: Option[String] = None) extends Module {
  val io = IO(new MemBlockIO(depth, width))

  val mem = SyncReadMem(depth, UInt(width.W))
  val readData = WireDefault(0.U(width.W))
  val writeDataReg = RegNext(io.writeData)
  val forwardSelReg = RegNext(io.writeAddr === io.readAddr && io.wrEn)
  // TODO: Check this for 8 ways since the synthesis tool seems to remove forwarding registers

  // Initialize memory block from a file
  if (dataFile.isDefined) {
    val file = dataFile.get

    if (file.trim().nonEmpty) { // If not empty path
      loadMemoryFromFileInline(mem, file)
    } else {
      println(s"Warning: The provided initialization file path is incorrect: $file")
    }
  }

  readData := mem.read(io.readAddr)

  when(io.wrEn) {
    mem.write(io.writeAddr, io.writeData)
  }

  io.readData := Mux(forwardSelReg, writeDataReg, readData)
}
