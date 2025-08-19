package caches.hardware.pipelined

import caches.hardware.util._
import chisel3._
import chisel3.util._

// TODO: The cache memory doesn't support byte masks right now, but it supports word masks.
//  Currently it takes every 4 bits of the byte mask, and converts it into a word mask using an or operation.
//  This is an assumption that patmos mostly uses byte en to enable words and not bytes.
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
    val wrData = Input(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
    val byteMask = Input(UInt(bytesPerBlock.W))
    val rData = Output(Vec(nSubBlocks, UInt((bytesPerSubBlock * 8).W)))
  })

  val waySplitMem = Array.fill(nWays)(
    Array.fill(nSubBlocks)(
      Array.fill(bytesPerSubBlock / 4)(
        Module(new MemBlock(nSets, 32, forward = false)))
      )
  )

  val rData = VecInit(
    Seq.fill(nSubBlocks)(VecInit(
      Seq.fill(bytesPerSubBlock / 4)(0.U(32.W))
    ))
  )

  val wordsPerBlock = bytesPerBlock / 4
  val wordsPerSubBlock = bytesPerSubBlock / 4

  val delayRWay = RegNext(io.rWayIdx, 0.U) // Need to delay this signal since it is only used to multiplex each sets data
  val wordEnMask = VecInit(Seq.fill(wordsPerBlock)(false.B))

  for (wordIdx <- 0 until wordsPerBlock) {
    wordEnMask(wordIdx) := io.byteMask(3 + (4 * wordIdx), 4 * wordIdx).orR
  }

  for (wayIdx <- 0 until nWays) {
    val isUpdateWay = wayIdx.U === io.wrWayIdx
    val isReadWay = wayIdx.U === delayRWay

    for (subBlockIdx <- 0 until nSubBlocks) {
      for (wordIdx <- 0 until wordsPerSubBlock) {
        val subBlockWordWrData = io.wrData(subBlockIdx)(31 + wordIdx * 32, wordIdx * 32)
        val subBlockWordWrEn = io.wrEn && isUpdateWay && wordEnMask(wordIdx + (wordsPerSubBlock * subBlockIdx))

        waySplitMem(wayIdx)(subBlockIdx)(wordIdx).io.readAddr := io.rIndex
        waySplitMem(wayIdx)(subBlockIdx)(wordIdx).io.writeData := subBlockWordWrData
        waySplitMem(wayIdx)(subBlockIdx)(wordIdx).io.writeAddr := io.wrIndex
        waySplitMem(wayIdx)(subBlockIdx)(wordIdx).io.wrEn := subBlockWordWrEn
//        waySplitMem(wayIdx)(subBlockIdx)(wordIdx).io.byteEn := io.byteMask(3 + ((4 * wordIdx) + byteMaskStartIdx), (4 * wordIdx) + byteMaskStartIdx).asBools

        when(isReadWay) {
          rData(subBlockIdx)(wordIdx) := waySplitMem(wayIdx)(subBlockIdx)(wordIdx).io.readData
        }
      }

      io.rData(subBlockIdx) := rData(subBlockIdx).asUInt
    }
  }
}
