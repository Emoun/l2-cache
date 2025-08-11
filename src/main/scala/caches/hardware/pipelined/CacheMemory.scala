package caches.hardware.pipelined

import caches.hardware.util.{ByteMaskedMemBlock, PipelineReg}
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
    val byteMask = Input(UInt(bytesPerBlock.W))
    val rData = Output(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
  })

  val waySplitMem = Array.fill(nWays)(
    Array.fill(nSubBlocks)(Module(new ByteMaskedMemBlock(nSets, bytesPerSubBlock * 8)))
  )

  val rData = VecInit(Seq.fill(nSubBlocks)(0.U((bytesPerSubBlock * 8).W)))
  val delayRWay = PipelineReg(io.rWayIdx, 0.U, !io.stall) // Need to delay this signal since it is only used to multiplex each sets data

  for (wayIdx <- 0 until nWays) {
    for (subBlockIdx <- 0 until nSubBlocks) {

      val subBlockWrData = io.wrData(subBlockIdx)
      val subBlockByteEn = io.byteMask((bytesPerSubBlock - 1) + (subBlockIdx * bytesPerSubBlock), subBlockIdx * bytesPerSubBlock)
      val subBlockWrEn = io.wrEn && wayIdx.U === io.wrWayIdx

      waySplitMem(wayIdx)(subBlockIdx).io.readAddr := io.rIndex
      waySplitMem(wayIdx)(subBlockIdx).io.writeData := subBlockWrData
      waySplitMem(wayIdx)(subBlockIdx).io.writeAddr := io.wrIndex
      waySplitMem(wayIdx)(subBlockIdx).io.wrEn := subBlockWrEn
      waySplitMem(wayIdx)(subBlockIdx).io.byteEn := subBlockByteEn.asBools

      when(wayIdx.U === delayRWay) {
        rData(subBlockIdx) := waySplitMem(wayIdx)(subBlockIdx).io.readData
      }
    }
  }

  io.rData := rData
}
