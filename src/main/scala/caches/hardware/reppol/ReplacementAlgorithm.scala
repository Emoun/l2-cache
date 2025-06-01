package caches.hardware.reppol

import chisel3._
import chisel3.util.{Valid, isPow2, log2Up}

/**
 * Interface for replacement policy algorithms
 *
 * @param ways number of ways in a set-associate cache
 */
class ReplacementAlgorithm(ways: Int) extends Module {
  require(isPow2(ways), "Number of ways must be a power of 2.")
  val io = IO(new Bundle {
    val update = Input(Valid(UInt(log2Up(ways).W)))
    val replaceWay = Output(UInt(log2Up(ways).W))
    val replacementSet = Output(Vec(ways, UInt(log2Up(ways).W))) // If a replacement policy needs an ordered set of ways, otherwise can be ignored
  })
  var wayIdxBits = log2Up(ways)

  if (wayIdxBits % 2 != 0) {
    wayIdxBits += 1
  }
}
