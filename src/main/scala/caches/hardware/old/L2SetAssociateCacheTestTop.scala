package caches.hardware.old

import caches.hardware.reppol.{SchedulerIO, SharedCacheReplacementPolicyType}
import caches.hardware.util.Constants.ADDRESS_WIDTH
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

/**
 *
 * @deprecated
 */
class debugCache(size: Int, ways: Int, bytesPerBlock: Int, bytesPerWord: Int) extends Bundle {
  private val indexWidth = log2Up((size / bytesPerBlock) / ways)
  private val blockOffsetWidth = log2Up(bytesPerBlock / bytesPerWord)
  private val tagWidth = ADDRESS_WIDTH - indexWidth - blockOffsetWidth - log2Up(bytesPerWord)

  val byteOffset = Output(UInt(log2Up(bytesPerWord).W))
  val blockOffset = Output(UInt(blockOffsetWidth.W))
  val index = Output(UInt(indexWidth.W))
  val tag = Output(UInt(tagWidth.W))
  val replaceWay = Output(UInt(log2Up(ways).W))
  val setTags = Output(Vec(ways, UInt(tagWidth.W)))
  val setData = Output(Vec(ways, Vec(bytesPerBlock / bytesPerWord, UInt((bytesPerBlock * 8).W))))
  val dirtyTags = Output(UInt(ways.W))
  val controllerState = Output(UInt(3.W))
}

class L2SetAssociateCacheTestTop(size: Int, ways: Int, bytesPerBlock: Int, bytesPerWord: Int, nCores: Int, addressWidth: Int, repPolicy: () => SharedCacheReplacementPolicyType) extends Module {
  val io = IO(new Bundle {
    val dbg = new debugCache(size, ways, bytesPerBlock, bytesPerWord)
    val scheduler = new SchedulerIO(nCores)
    val higher = new SharedCacheIO(ADDRESS_WIDTH, bytesPerWord * 8, nCores)
    val lower = Flipped(new SharedCacheIO(ADDRESS_WIDTH, bytesPerBlock * 8, nCores))
  })

  val l2cache = Module(new L2SetAssociateCache(
      size = size,
      ways = ways,
      bytesPerBlock = bytesPerBlock,
      bytesPerWord = bytesPerWord,
      nCores = nCores,
      addressWidth = addressWidth,
      repPolicy = repPolicy
    )
  )

  io.scheduler <> l2cache.io.scheduler
  io.higher <> l2cache.io.higher
  io.lower <> l2cache.io.lower

  io.dbg.byteOffset := DontCare
  io.dbg.blockOffset := DontCare
  io.dbg.index := DontCare
  io.dbg.tag := DontCare
  io.dbg.replaceWay := DontCare
  io.dbg.setTags := DontCare
  io.dbg.setData := DontCare
  io.dbg.dirtyTags := DontCare
  io.dbg.controllerState := DontCare
  BoringUtils.bore(l2cache.cacheMem.byteOffset, Seq(io.dbg.byteOffset))
  BoringUtils.bore(l2cache.cacheMem.blockOffset, Seq(io.dbg.blockOffset))
  BoringUtils.bore(l2cache.cacheMem.index, Seq(io.dbg.index))
  BoringUtils.bore(l2cache.cacheMem.tag, Seq(io.dbg.tag))
  BoringUtils.bore(l2cache.repPol.io.control.replaceWay, Seq(io.dbg.replaceWay))
  BoringUtils.bore(l2cache.cacheMem.waysTags, Seq(io.dbg.setTags))
  BoringUtils.bore(l2cache.cacheMem.waysData, Seq(io.dbg.setData))
  BoringUtils.bore(l2cache.cacheMem.io.controller.dirty, Seq(io.dbg.dirtyTags))
  BoringUtils.bore(l2cache.cacheController.stateReg, Seq(io.dbg.controllerState))
}
