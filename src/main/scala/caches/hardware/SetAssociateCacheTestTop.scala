package caches.hardware

import caches.hardware.reppol.ReplacementPolicyType
import caches.hardware.util.Constants.ADDRESS_WIDTH
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

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
  val controllerState = Output(UInt(3.W))
}

class SetAssociateCacheTestTop(size: Int, ways: Int, bytesPerBlock: Int, bytesPerWord: Int, repPolicy: () => ReplacementPolicyType) extends Module {
  val io = IO(new Bundle {
    val dbg = new debugCache(size, ways, bytesPerBlock, bytesPerWord)
    val higher = new CacheIO(ADDRESS_WIDTH, bytesPerWord * 8)
    val lower = Flipped(new CacheIO(ADDRESS_WIDTH, bytesPerBlock * 8))
  })

  val lruCache = Module(new SetAssociateCache(size, ways, bytesPerBlock, bytesPerWord, repPolicy))

  io.higher <> lruCache.io.higher
  io.lower <> lruCache.io.lower

  io.dbg.byteOffset := DontCare
  io.dbg.blockOffset := DontCare
  io.dbg.index := DontCare
  io.dbg.tag := DontCare
  io.dbg.replaceWay := DontCare
  io.dbg.setTags := DontCare
  io.dbg.setData := DontCare
  io.dbg.controllerState := DontCare
  BoringUtils.bore(lruCache.cacheMem.byteOffset, Seq(io.dbg.byteOffset))
  BoringUtils.bore(lruCache.cacheMem.blockOffset, Seq(io.dbg.blockOffset))
  BoringUtils.bore(lruCache.cacheMem.index, Seq(io.dbg.index))
  BoringUtils.bore(lruCache.cacheMem.tag, Seq(io.dbg.tag))
  BoringUtils.bore(lruCache.cacheControl.repPolicy.io.replaceWay, Seq(io.dbg.replaceWay))
  BoringUtils.bore(lruCache.cacheMem.waysTags, Seq(io.dbg.setTags))
  BoringUtils.bore(lruCache.cacheMem.waysData, Seq(io.dbg.setData))
  BoringUtils.bore(lruCache.cacheControl.stateReg, Seq(io.dbg.controllerState))
}
