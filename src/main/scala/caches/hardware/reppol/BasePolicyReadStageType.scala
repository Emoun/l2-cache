package caches.hardware.reppol

import chisel3._
import chisel3.util.log2Up

abstract class BasePolicyReadStageType(nWays: Int, nSets: Int, dataWidth: Int, repSetFormat: BaseReplacementSetFormat) extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())
    val rIdx = Input(UInt(log2Up(nSets).W))
    val wrEn = Input(Bool())
    val wIdx = Input(UInt(log2Up(nSets).W))
    val wData = Input(UInt(dataWidth.W))
    val fwd = Input(Bool())
    val readState = Output(UInt(dataWidth.W))
    val replaceWay = Output(UInt(log2Up(nWays).W))
    // If any other policy needs replacement set
    val replacementSet = repSetFormat match {
      case NumericalFormat() => Output(Vec(nWays, UInt(log2Up(nWays).W)))
      case MruFormat() => Output(Vec(nWays, UInt(1.W)))
      case _ => throw new IllegalArgumentException("Unrecognized replacement set format.")
    }
  })

  val stateWidth = dataWidth

  def getNumericalReplacementSet(state: UInt): Vec[UInt]

  def getMruReplacementSet(state: UInt): Vec[UInt]

  def getRepWay(state: UInt): UInt
}
