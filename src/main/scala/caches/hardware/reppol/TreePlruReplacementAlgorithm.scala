package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * Tree based implementation of Pseudo LRU.
 *
 * @param nWays number of ways in a set-associate cache
 */
class TreePlruReplacementAlgorithm(nWays: Int) extends Module() {
  val io = IO(new Bundle {
    val hitWay = Input(UInt(log2Up(nWays).W))
    val computeLruBits = Input(Vec(nWays - 1, Bool()))
    val updateLruBits = Input(Vec(nWays - 1, Bool()))
    val updatedLru = Output(Vec(nWays - 1, Bool()))
    val replaceWay = Output(UInt(log2Up(nWays).W))
    val replacementSet = Output(Vec(nWays, UInt(log2Up(nWays).W)))
  })

  var wayIdxBits = log2Up(nWays)

  /**
   * Finds the index of the LRU way in the set
   *
   * @return the index of the LRU way in the set
   */
  def getLru(mruBits: Vec[Bool]): UInt = {
    val lru = VecInit(Seq.fill(wayIdxBits)(false.B))
    val treePath = VecInit(Seq.fill(wayIdxBits)(0.U(wayIdxBits.W)))

    for (i <- 0 until wayIdxBits) {
      val nodeState = mruBits(treePath(i))
      lru(wayIdxBits - 1 - i) := nodeState // Set the MSB bits first
      if (i != wayIdxBits - 1) {
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
   * Generates logic for computing an ordered LRU set from a given MRU bits array
   *
   * @param mruBits set of mru bits
   * @return
   */
  def getLruOrderedSet(mruBits: Vec[Bool]): Vec[UInt] = {
    val lruOrderedSet = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))

    // TODO: Implement

    lruOrderedSet
  }

  /**
   * Updates the LRU tree state based on the recently accessed way
   *
   * @param way the way that has been accessed
   */
  def updateLru(way: UInt, lruBits: Vec[Bool]): Vec[Bool] = {
    val treePath = VecInit(Seq.fill(wayIdxBits)(0.U(wayIdxBits.W)))

    val newLru = VecInit(Seq.fill(nWays - 1)(false.B))
    for (j <- 0 until lruBits.length) { // Copy the lru bits
      newLru(j) := lruBits(j)
    }

    for (i <- 0 until wayIdxBits) {
      val accessBit = way(wayIdxBits - 1 - i)
      newLru(treePath(i)) := ~accessBit

      // Flip all the bits that are not the same as the way we hit
      if (i != wayIdxBits - 1) {
        val pathOffset = (treePath(i) << 1).asUInt

        when(accessBit === true.B) {
          treePath(i + 1) := pathOffset + 2.U
        }.otherwise {
          treePath(i + 1) := pathOffset + 1.U
        }
      }
    }

    newLru
  }

  val updatedLruBits = updateLru(io.hitWay, io.updateLruBits)

  // Replacement selection logic
  val replaceWay = getLru(io.computeLruBits)
  val replacementSet = getLruOrderedSet(io.computeLruBits)

  io.updatedLru := updatedLruBits
  io.replaceWay := replaceWay
  io.replacementSet := replacementSet
}
