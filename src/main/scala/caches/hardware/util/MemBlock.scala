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

class MaskedMemBlockIO(depth: Int, width: Int) extends Bundle {
  val readAddr = Input(UInt(log2Up(depth).W))
  val writeAddr = Input(UInt(log2Up(depth).W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
  val byteEn = Input(Vec(width / 8, Bool()))
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

/**
 * General purpose synchronous memory block with mask.
 *
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 */
class ByteMaskedMemBlock(depth: Int, width: Int, dataFile: Option[String] = None) extends Module {
  val io = IO(new MaskedMemBlockIO(depth, width))

  val readDataAsVec = VecInit(Seq.fill(width / 8)(0.U(8.W)))
  val mem = SyncReadMem(depth, Vec(width / 8, UInt(8.W)), SyncReadMem.ReadFirst)

  // Initialize memory block from a file
  if (dataFile.isDefined) {
    val file = dataFile.get

    if (file.trim().nonEmpty) { // If not empty path
      loadMemoryFromFileInline(mem, file)
    } else {
      println(s"Warning: The provided initialization file path is incorrect: $file")
    }
  }

  val writeDataAsVec = UIntToVec(io.writeData, 8)

  // Write
  when(io.wrEn) {
    mem.write(io.writeAddr, writeDataAsVec, io.byteEn)
  }

  // TODO: This module cannot be easily synthesized by quartus

  // Read
  readDataAsVec := mem.read(io.readAddr)

  val writeByteMaskReg = RegNext(io.byteEn)
  val writeDataReg = RegNext(writeDataAsVec)
  val forwardSelReg = RegNext((io.writeAddr === io.readAddr) && io.wrEn)

  // If forwarding then we need to combine the old data with the write data using the byte mask
  val forwardDataAsVec = VecInit(Seq.fill(width / 8)(0.U(8.W)))
  for (i <- 0 until forwardDataAsVec.length) {
    when(writeByteMaskReg(i)) {
      forwardDataAsVec(i) := writeDataReg(i)
    } .otherwise {
      forwardDataAsVec(i) := readDataAsVec(i)
    }
  }

  io.readData := Mux(forwardSelReg, writeDataAsVec.asUInt, readDataAsVec.asUInt)
}