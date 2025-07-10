package caches.hardware.reppol

import chisel3._
import chisel3.util.{Valid, isPow2, log2Up}

/**
 * Interface for replacement policy algorithms
 *
 * @param nWays number of ways in a set-associate cache
 */
class ReplacementAlgorithmType(nWays: Int) extends Module {
  require(isPow2(nWays), "Number of ways must be a power of 2.")
  val io = IO(new Bundle {
    val update = Input(Valid(UInt(log2Up(nWays).W)))
    val replaceWay = Output(UInt(log2Up(nWays).W))
    val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W))) // If a replacement policy needs an ordered set of ways, otherwise can be ignored
  })
  var wayIdxBits = log2Up(nWays)

  if (wayIdxBits % 2 != 0) {
    wayIdxBits += 1
  }
}
