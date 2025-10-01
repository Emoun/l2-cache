package caches.hardware.util

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

class MemBlockIO(depth: Int, width: Int) extends Bundle {
  val stall = Input(Bool())
  val readAddr = Input(UInt(log2Up(depth).W))
  val writeAddr = Input(UInt(log2Up(depth).W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
  val readData = Output(UInt(width.W))
}

class TwoReadMemBlockIO(depth: Int, width: Int) extends Bundle {
  val stall = Input(Bool())
  val readAddr1 = Input(UInt(log2Up(depth).W))
  val readAddr2 = Input(UInt(log2Up(depth).W))
  val writeAddr = Input(UInt(log2Up(depth).W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
  val readData1 = Output(UInt(width.W))
  val readData2 = Output(UInt(width.W))
}

class MaskedMemBlockIO(depth: Int, width: Int, maskElemWidth: Int) extends Bundle {
  val readAddr = Input(UInt(log2Up(depth).W))
  val writeAddr = Input(UInt(log2Up(depth).W))
  val writeData = Input(UInt(width.W))
  val wrEn = Input(Bool())
  val byteEn = Input(Vec(width / maskElemWidth, Bool()))
  val readData = Output(UInt(width.W))
}

/**
 * General purpose synchronous memory block.
 *
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 * @param forward Whether to include forwarding registers or not
 * @param dataFile Hex datafile for initializing the memory block
 */
class MemBlock(depth: Int, width: Int, forward: Boolean = true, stallReg: Boolean = true, dataFile: Option[String] = None) extends Module {
  val io = IO(new MemBlockIO(depth, width))

  val readData = WireDefault(0.U(width.W))
  val mem = SyncReadMem(depth, UInt(width.W), SyncReadMem.ReadFirst)

  val rAddr = WireDefault(0.U(log2Up(depth).W))
  if (stallReg) {
    // In case of pipeline stall, we need to hold the read address, otherwise a new read address will overwrite the read data
    val readAddr1StallReg = PipelineReg(io.readAddr, 0.U, !io.stall)
    rAddr := Mux(io.stall, readAddr1StallReg, io.readAddr)
  } else {
    rAddr := io.readAddr
  }

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
  readData := mem.read(rAddr)

  if (forward) {
    // Forwarding logic
    val writeDataReg = RegNext(io.writeData)
    val forwardSelReg = RegNext((io.writeAddr === rAddr) && io.wrEn)
    io.readData := Mux(forwardSelReg, writeDataReg, readData)
  } else {
    // No forwarding, just output the read data
    io.readData := readData
  }
}

/**
 * General purpose synchronous memory block with mask.
 *
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 * @param maskElemWidth Width of bits that a single mask bool value covers in the input data
 * @param forward Whether to include forwarding registers or not
 * @param dataFile Hex datafile for initializing the memory block
 */
class MaskedMemBlock(depth: Int, width: Int, maskElemWidth: Int, forward: Boolean = true, dataFile: Option[String] = None) extends Module {
  val io = IO(new MaskedMemBlockIO(depth, width, maskElemWidth))

  val mem = SyncReadMem(depth, Vec(width / maskElemWidth, UInt(maskElemWidth.W)), SyncReadMem.ReadFirst)

  // Initialize memory block from a file
  if (dataFile.isDefined) {
    val file = dataFile.get

    if (file.trim().nonEmpty) { // If not empty path
      loadMemoryFromFileInline(mem, file)
    } else {
      println(s"Warning: The provided initialization file path is incorrect: $file")
    }
  }

  val writeDataAsVec = UIntToVec(io.writeData, width, maskElemWidth)

  // Write
  when(io.wrEn) {
    mem.write(io.writeAddr, writeDataAsVec, io.byteEn)
  }

  // Read
  val readDataAsVec = mem.read(io.readAddr)

  if (forward) {
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
  } else {
    // No forwarding, just output the read data
    io.readData := readDataAsVec.asUInt
  }
}

/**
 * General purpose synchronous memory block with two read ports.
 *
 * @param depth Number of memory entries
 * @param width Memory entry width in bits
 * @param forward Whether to include forwarding registers or not
 * @param dataFile Hex datafile for initializing the memory block
 */
class TwoReadMemBlock(depth: Int, width: Int, forward: Boolean = true, dataFile: Option[String] = None, stallReg1: Boolean = true, stallReg2: Boolean = true) extends Module {
  val io = IO(new TwoReadMemBlockIO(depth, width))

  val readData1 = WireDefault(0.U(width.W))
  val readData2 = WireDefault(0.U(width.W))
  val mem = SyncReadMem(depth, UInt(width.W), SyncReadMem.ReadFirst)

  // In case of pipeline stall, we need to hold the read address, otherwise a new read address will overwrite the read data
  val rAddr1 = WireDefault(0.U(log2Up(depth).W))
  if (stallReg1) {
    val readAddr1StallReg = PipelineReg(io.readAddr1, 0.U, !io.stall)
    rAddr1 := Mux(io.stall, readAddr1StallReg, io.readAddr1)
  } else {
    rAddr1 := io.readAddr1
  }

  val rAddr2 = WireDefault(0.U(log2Up(depth).W))
  if (stallReg2) {
    val readAddr2StallReg = PipelineReg(io.readAddr2, 0.U, !io.stall)
    rAddr2 := Mux(io.stall, readAddr2StallReg, io.readAddr2)
  } else {
    rAddr2 := io.readAddr2
  }

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
  readData1 := mem.read(rAddr1)
  readData2 := mem.read(rAddr2)

  if (forward) {
    // Forwarding logic
    val writeDataReg = RegNext(io.writeData)
    val forwardSelReg1 = RegNext((io.writeAddr === rAddr1) && io.wrEn)
    val forwardSelReg2 = RegNext((io.writeAddr === rAddr2) && io.wrEn)

    io.readData1 := Mux(forwardSelReg1, writeDataReg, readData1)
    io.readData2 := Mux(forwardSelReg2, writeDataReg, readData2)
  } else {
    // No forwarding, just output the read data
    io.readData1 := readData1
    io.readData1 := readData1
  }
}