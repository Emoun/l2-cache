package caches.hardware.reppol

import chisel3._
import chisel3.util._

abstract class BasePolicyUpdateStageType(nWays: Int, dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val hitWay = Input(UInt(log2Up(nWays).W))
    val stateIn = Input(UInt(dataWidth.W))
    val stateOut = Output(UInt(dataWidth.W))
  })

  def update(hitWay: UInt, state: UInt): UInt
}
