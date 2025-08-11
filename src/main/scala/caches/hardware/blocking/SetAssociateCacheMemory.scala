package caches.hardware.blocking

import caches.hardware.util.MemBlock
import chisel3._
import chisel3.util._

/**
 * Interface to another level module/memory component
 *
 * @param addrWidth width of the address line in bits
 * @param dataWidth width of the data bus in bits
 */
class MemIO(addrWidth: Int, dataWidth: Int) extends Bundle {
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
class ControllerMemIO(ways: Int, sets: Int) extends Bundle {
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
 * @param nWays number of ways for the set associate cache, must be power of 2
 */
class SetAssociateCacheMemory(nWays: Int, nSets: Int, bytesPerBlock: Int, bytesPerWord: Int, addressWidth: Int) extends Module {
  require(isPow2(nWays), "Number of ways must be a power of 2.")

  private val wordsPerBlock = bytesPerBlock / bytesPerWord
  private val byteOffsetWidth = log2Up(bytesPerWord)
  private val blockOffsetWidth = log2Up(wordsPerBlock)
  private val indexWidth = log2Up(nSets)
  private val tagWidth = addressWidth - indexWidth - blockOffsetWidth - byteOffsetWidth

//  private val tagMemSize = (nSets * tagWidth * nWays) / 8
//  private val cacheLineMemSize = nSets * wordsPerBlock * bytesPerWord * nWays
//  println(s"Tag memory size: $tagMemSize bytes")
//  println(s"Cache line memory size: $cacheLineMemSize bytes")

  val io = IO(new Bundle {
    val controller = new ControllerMemIO(nWays, nSets)
    val higher = new MemIO(addressWidth, bytesPerWord * 8)
    val lower = Flipped(new MemIO(addressWidth, bytesPerBlock * 8))
  })

  // Address and data holding registers in case of cache misses
  val addrReg = RegInit(WireDefault(0.U(addressWidth.W))) // We can assume that master has to keep data valid until controller acknowledges the request. Then for interfaces that doe not require the master to keep the data valid, we can create a wrapper instead.
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
  val tag = addr(addressWidth - 1, indexWidth + blockOffsetWidth + byteOffsetWidth)

  // Create state holding elements for cache memory
  val waysValidBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))
  val waysDirtyBits = Array.fill(nWays)(RegInit(VecInit(Seq.fill(nSets)(false.B))))
  val waysTagMem = Array.fill(nWays)(Module(new MemBlock(nSets, tagWidth)))
  val waysCacheLineMem = Array.fill(nWays)(
    Array.fill(wordsPerBlock)(Module(new MemBlock(nSets, bytesPerWord * 8)))
  )

  // Registers used to delay address fields due to SRAM behaviour
  val blockOffsetDelayReg = RegNext(blockOffset)
  val indexDelayReg = RegNext(index)
  val tagDelayReg = RegNext(tag)

  // Vectors for storing signals coming out of each way
  val hits = VecInit(Seq.fill(nWays)(false.B))
  val dirty = VecInit(Seq.fill(nWays)(false.B))
  val waysTags = VecInit(Seq.fill(nWays)(0.U(tagWidth.W)))
  val waysData = VecInit(Seq.fill(nWays)(
    VecInit(Seq.fill(wordsPerBlock)(0.U((bytesPerWord * 8).W)))
  ))

  val hitWay = WireDefault(0.U(log2Up(nWays).W))

  for (wayIdx <- 0 until nWays) {

    // Assign the signals for the cache line memories
    for (wordIdx <- 0 until wordsPerBlock) {

      val wordwData = Mux(io.controller.replaceLine, io.lower.rData((bytesPerWord * 8) - 1 + (wordIdx * bytesPerWord * 8), wordIdx * bytesPerWord * 8), writeData)
      val wordwrEn = (io.controller.replaceLine && io.controller.replaceWay === wayIdx.asUInt) || (io.controller.updateLine && blockOffset === wordIdx.asUInt && hitWay === wayIdx.asUInt)

      waysCacheLineMem(wayIdx)(wordIdx).io.readAddr := index
      waysCacheLineMem(wayIdx)(wordIdx).io.writeData := wordwData
      waysCacheLineMem(wayIdx)(wordIdx).io.writeAddr := index
      waysCacheLineMem(wayIdx)(wordIdx).io.wrEn := wordwrEn
      // waysCacheLines(wayIdx)(wordIdx).io.wrMask := higherIO.dinMask // If adding support for byte masks

      waysData(wayIdx)(wordIdx) := waysCacheLineMem(wayIdx)(wordIdx).io.readData
    }

    // Assign the signals for the tag memories
    waysTagMem(wayIdx).io.readAddr := index
    waysTagMem(wayIdx).io.writeData := tag
    waysTagMem(wayIdx).io.writeAddr := index
    waysTagMem(wayIdx).io.wrEn := io.controller.replaceLine && (io.controller.replaceWay === wayIdx.asUInt)

    val tagReadData = waysTagMem(wayIdx).io.readData
    waysTags(wayIdx) := tagReadData

    // Set the valid bit to true when loading a new cache line in
    when(io.controller.replaceLine && io.controller.replaceWay === wayIdx.asUInt) {
      waysValidBits(wayIdx)(index) := true.B
    }

    // Set the dirty bit to true on update and set it to false on evict
    when(io.controller.updateLine && hitWay === wayIdx.asUInt) {
      waysDirtyBits(wayIdx)(index) := true.B
    }.elsewhen(io.controller.replaceLine && io.controller.replaceWay === wayIdx.asUInt) {
      waysDirtyBits(wayIdx)(index) := false.B
    }

    // Check if the block is dirty or valid in a way
    hits(wayIdx) := waysValidBits(wayIdx)(indexDelayReg) && (tagDelayReg === tagReadData)
    dirty(wayIdx) := waysDirtyBits(wayIdx)(indexDelayReg)
  }

  hitWay := PriorityEncoder(hits)

  // Form the address of the dirty block by concatenating dirty block's tag and index
  val writeBackAddr = (waysTags(io.controller.replaceWay) ## indexDelayReg ## 0.U((blockOffsetWidth + byteOffsetWidth).W)).asUInt

  io.higher.rData := waysData(hitWay)(blockOffsetDelayReg).asUInt // A word from the block to the higher level
  io.lower.wData := waysData(io.controller.replaceWay).asUInt
  io.lower.addr := Mux(io.controller.writeBack, writeBackAddr, addr) // Select between the dirty or missing cache line address
  io.lower.wMask := 0.U // Not using any of the write masks for now

  io.controller.hit := hits.reduce((x, y) => x || y)
  io.controller.dirty := dirty.asUInt
  io.controller.set := indexDelayReg
  io.controller.hitWay := hitWay
}