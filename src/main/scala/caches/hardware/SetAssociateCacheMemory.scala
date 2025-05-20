package caches.hardware

import caches.hardware.util.Constants.ADDRESS_WIDTH
import chisel3._
import chisel3.util._

/**
 * Interface to another level module/memory component
 *
 * @param addrWidth width of the address line in bits
 * @param dataWidth width of the data bus in bits
 */
class memIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = Input(UInt(addrWidth.W))
  val wData = Input(UInt(dataWidth.W))
  val wMask = Input(UInt((dataWidth / 8).W)) // Byte mask if needed
  val rData = Output(UInt(dataWidth.W))
}

/**
 * Interface to/from a cache controller
 *
 * @param ways number of ways in a single set
 * @param sets number of sets in a cache
 */
class controlMemIO(ways: Int, sets: Int) extends Bundle {
  val latchReq = Input(Bool())
  val validReq = Input(Bool())
  val updateLine = Input(Bool())
  val replaceLine = Input(Bool())
  val replaceWay = Input(UInt(log2Up(ways).W))
  val writeBack = Input(Bool())
  val hit = Output(Bool())
  val dirty = Output(UInt(ways.W))
  val set = Output(UInt(log2Up(sets).W))
  val hitWay = Output(UInt(log2Up(ways).W))
}

/**
 * Module that holds the cache data. Store cache lines and tags in RAM modules, and stores valid and dirty bits in
 * registers.
 *
 * @param size cache size in bytes, must be a power of 2
 * @param ways number of ways for the set associate cache, must be power of 2
 */
class SetAssociateCacheMemory(size: Int, ways: Int, sets: Int, bytesPerBlock: Int, bytesPerWord: Int) extends Module {
  require(isPow2(size), "Cache size must be a power of 2.")
  require(isPow2(ways), "Number of ways must be a power of 2.")

  private val wordsPerBlock = bytesPerBlock / bytesPerWord
  private val byteOffsetWidth = log2Up(bytesPerWord)
  private val blockOffsetWidth = log2Up(wordsPerBlock)
  private val indexWidth = log2Up(sets)
  private val tagWidth = ADDRESS_WIDTH - indexWidth - blockOffsetWidth - byteOffsetWidth

  val io = IO(new Bundle {
    val controller = new controlMemIO(ways, sets)
    val higher = new memIO(ADDRESS_WIDTH, bytesPerWord * 8)
    val lower = Flipped(new memIO(ADDRESS_WIDTH, bytesPerBlock * 8))
  })

  // Address and data holding registers in case of cache misses
  val addrReg = RegInit(WireDefault(0.U(ADDRESS_WIDTH.W)))
  val writeDataReg = RegInit(WireDefault(0.U((bytesPerWord * 8).W)))

  when(io.controller.latchReq) {
    addrReg := io.higher.addr
    writeDataReg := io.higher.wData
  }

  val addr = Mux(io.controller.validReq, addrReg, io.higher.addr)
  val writeData = Mux(io.controller.validReq, writeDataReg, io.higher.wData)

  // Decode the address fields
  val byteOffset = addr(byteOffsetWidth - 1, 0)
  val blockOffset = addr((blockOffsetWidth - 1) + byteOffsetWidth, byteOffsetWidth)
  val index = addr((indexWidth - 1) + blockOffsetWidth + byteOffsetWidth, blockOffsetWidth + byteOffsetWidth)
  val tag = addr(ADDRESS_WIDTH - 1, indexWidth + blockOffsetWidth + byteOffsetWidth)

  // Create state holding elements for cache memory
  val waysValidBits = Array.fill(ways)(RegInit(VecInit(Seq.fill(sets)(false.B))))
  val waysDirtyBits = Array.fill(ways)(RegInit(VecInit(Seq.fill(sets)(false.B))))
  val waysTagMem = Array.fill(ways)(Module(new MemBlock(sets, tagWidth)))
  val waysCacheLineMem = Array.fill(ways)(
    Array.fill(wordsPerBlock)(Module(new MemBlock(sets, bytesPerWord * 8)))
  )

  // Registers used to delay address fields due to SRAM behaviour
  val blockOffsetDelayReg = RegNext(blockOffset)
  val indexDelayReg = RegNext(index)
  val tagDelayReg = RegNext(tag)

  // Vectors for storing signals coming out of each way
  val hits = VecInit(Seq.fill(ways)(false.B))
  val dirty = VecInit(Seq.fill(ways)(false.B))
  val hitWay = WireDefault(0.U(log2Up(ways).W))
  val waysTags = VecInit(Seq.fill(ways)(0.U(tagWidth.W)))
  val waysData = VecInit(Seq.fill(ways)(
    VecInit(Seq.fill(wordsPerBlock)(0.U((bytesPerWord * 8).W)))
  ))

  for (wayIdx <- 0 until ways) {

    for (wordIdx <- 0 until wordsPerBlock) {
      waysCacheLineMem(wayIdx)(wordIdx).io.readAddr := index
      waysCacheLineMem(wayIdx)(wordIdx).io.writeData := Mux(io.controller.replaceLine, io.lower.rData((bytesPerWord * 8) - 1 + (wordIdx * bytesPerWord * 8), wordIdx * bytesPerWord * 8), writeData)
      waysCacheLineMem(wayIdx)(wordIdx).io.writeAddr := index
      waysCacheLineMem(wayIdx)(wordIdx).io.wrEn := (io.controller.replaceLine && io.controller.replaceWay === wayIdx.asUInt) || (io.controller.updateLine && blockOffset === wordIdx.asUInt && hitWay === wayIdx.asUInt)
      // waysCacheLines(wayIdx)(wordIdx).io.wrMask := higherIO.dinMask TODO: If adding support for byte masks

      waysData(wayIdx)(wordIdx) := waysCacheLineMem(wayIdx)(wordIdx).io.readData
    }

    waysTagMem(wayIdx).io.readAddr := index
    waysTagMem(wayIdx).io.writeData := tag
    waysTagMem(wayIdx).io.writeAddr := index
    waysTagMem(wayIdx).io.wrEn := io.controller.replaceLine && (io.controller.replaceWay === wayIdx.asUInt)

    // Set the valid bit to true when loading a new cache line in
    when(io.controller.replaceLine && io.controller.replaceWay === wayIdx.asUInt) { // TODO: Is there a need to set it to true on update too?
      waysValidBits(wayIdx)(index) := true.B
    }

    // Set the dirty bit to true on update and set it to false on evict
    when(io.controller.updateLine && hitWay === wayIdx.asUInt) {
      waysDirtyBits(wayIdx)(index) := true.B
    }.elsewhen(io.controller.replaceLine && io.controller.replaceWay === wayIdx.asUInt) {
      waysDirtyBits(wayIdx)(index) := false.B
    }

    // Check if the block is dirty or valid in a way
    hits(wayIdx) := waysValidBits(wayIdx)(indexDelayReg) && (tagDelayReg === waysTagMem(wayIdx).io.readData)
    dirty(wayIdx) := waysDirtyBits(wayIdx)(indexDelayReg)
    waysTags(wayIdx) := waysTagMem(wayIdx).io.readData
  }

  hitWay := PriorityEncoder(hits)
  val selectedBlock = waysData(Mux(io.controller.writeBack, io.controller.replaceWay,  hitWay)) // Select between a hit block or an evict block

  // Form the address of the dirty block by concatenating dirty block's tag and index
  val writeBackAddr = (waysTags(io.controller.replaceWay) ## indexDelayReg ## 0.U((blockOffsetWidth + byteOffsetWidth).W)).asUInt

  io.higher.rData := selectedBlock(blockOffsetDelayReg).asUInt // A word from the block to the higher level
  io.lower.wData := selectedBlock.asUInt
  io.lower.addr := Mux(io.controller.writeBack, writeBackAddr, addr) // Select between the dirty or missing cache line address
  io.lower.wMask := 0.U // TODO: Not using any of the write masks for now

  io.controller.hit := hits.reduce((x, y) => x || y)
  io.controller.dirty := dirty.asUInt
  io.controller.set := indexDelayReg
  io.controller.hitWay := hitWay
}