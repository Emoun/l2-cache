package caches.hardware.pipelined.cache

import caches.hardware.util.{MemBlock, PipelineReg}
import chisel3._
import chisel3.util._

class CacheMemory(sizeInBytes: Int, nWays: Int, bytesPerBlock: Int, bytesPerSubBlock: Int) extends Module {
  private val nSets = sizeInBytes / (nWays * bytesPerBlock)
  private val nSubBlocks = bytesPerBlock / bytesPerSubBlock
  private val indexWidth = log2Up(nSets)

  val io = IO(new Bundle{
    val rIndex = Input(UInt(indexWidth.W))
    val rWayIdx = Input(UInt(log2Up(nWays).W))
    val wrIndex = Input(UInt(indexWidth.W))
    val wrWayIdx = Input(UInt(log2Up(nWays).W))
    val wrEn = Input(Bool())
    val stall = Input(Bool())
    val wrData = Input(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
    val rData = Output(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
  })

  val waySplitMem = Array.fill(nWays)(
    Array.fill(nSubBlocks)(Module(new MemBlock(nSets, bytesPerSubBlock * 8)))
  )

  val rData = VecInit(Seq.fill(nSubBlocks)(0.U((bytesPerSubBlock * 8).W)))
  val delayRWay = PipelineReg(io.rWayIdx, 0.U, !io.stall) // Need to delay this signal since it is only used to multiplex each sets data

  for (wayIdx <- 0 until nWays) {
    for (wordIdx <- 0 until nSubBlocks) {

      val wordWrData = io.wrData(wordIdx)
      val wordWrEn = io.wrEn && wayIdx.U === io.wrWayIdx

      waySplitMem(wayIdx)(wordIdx).io.readAddr := io.rIndex
      waySplitMem(wayIdx)(wordIdx).io.writeData := wordWrData
      waySplitMem(wayIdx)(wordIdx).io.writeAddr := io.wrIndex
      waySplitMem(wayIdx)(wordIdx).io.wrEn := wordWrEn
      // waysCacheLines(wayIdx)(wordIdx).io.wrMask := higherIO.dinMask

      when(wayIdx.U === delayRWay) {
        rData(wordIdx) := waySplitMem(wayIdx)(wordIdx).io.readData
      }
    }
  }

  io.rData := rData
}
