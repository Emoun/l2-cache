package caches.hardware

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

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
 *
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 */
class MemBlock(depth: Int, width: Int, dataFile: Option[String] = None) extends Module {
  val io = IO(new MemBlockIO(depth, width))

  // TODO: Think about if the forwarding registers are needed, i.e. if we ever read or write to the same address in the same cycle
  //  NOTE: There is no need for this as we are never requesting a read and write to the same address in the same cycle
  val mem = SyncReadMem(depth, UInt(width.W))
  val readData = WireDefault(0.U(width.W))
  //  val writeDataReg = RegNext(io.writeData)
  //  val forwardSelReg = RegNext((io.writeAddr === io.readAddr) && io.wrEn)

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

  io.readData := readData //Mux(forwardSelReg, writeDataReg, readData)
}
