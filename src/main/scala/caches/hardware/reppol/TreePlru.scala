package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * Tree based implementation of Pseudo LRU.
 *
 * @param ways number of ways in a single cache set
 */
class TreePlru(ways: Int) extends Module {
  require(isPow2(ways), "Number of ways must be a power of 2.")
  private val nBits = log2Up(ways)

  val io = IO(new Bundle {
    val update = Input(Valid(UInt(log2Up(ways).W)))
    val replaceWay = Output(UInt(log2Up(ways).W))
  })

  // An array of one-bit registers, each for a node in the tree
  val plruBits = RegInit(VecInit(Seq.fill(ways - 1)(false.B)))

  /**
   * Finds the index of the LRU way in the set
   *
   * @return the index of the LRU way in the set
   */
  def getLru: UInt = {
    val lru = VecInit(Seq.fill(nBits)(false.B))
    val treePath = VecInit(Seq.fill(nBits)(0.U(nBits.W)))

    for (i <- 0 until nBits) {
      val nodeState = plruBits(treePath(i))
      lru(nBits - 1 - i) := nodeState // Set the MSB bits first
      if (i != nBits - 1) {
        val pathOffset = (treePath(i) << 1).asUInt

        when(nodeState === true.B) {
          treePath(i + 1) := pathOffset + 2.U
        }.otherwise {
          treePath(i + 1) := pathOffset + 1.U
        }
      }
    }

    lru.asUInt
  }

  /**
   * Updates the LRU tree state based on the recently accessed way
   *
   * @param way the way that has been accessed
   */
  def updateLru(way: UInt): Unit = {
    val treePath = VecInit(Seq.fill(nBits)(0.U(nBits.W)))

    for (i <- 0 until nBits) {
      val accessBit = way(nBits - 1 - i)
      plruBits(treePath(i)) := ~accessBit

      if (i != nBits - 1) {
        val pathOffset = (treePath(i) << 1).asUInt

        when(accessBit === true.B) {
          treePath(i + 1) := pathOffset + 2.U
        }.otherwise {
          treePath(i + 1) := pathOffset + 1.U
        }
      }
    }
  }

  val replaceWay = getLru

  when(io.update.valid) {
    updateLru(io.update.bits)
  }

  io.replaceWay := replaceWay
}
