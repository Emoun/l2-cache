package caches.hardware.reppol

import chisel3._
import chisel3.util._
import chisel3.util.PriorityEncoder

/**
 * Bit based implementation of Pseudo LRU
 *
 * @param nWays number of ways in a set-associate cache
 */
class BitPlruReplacementAlgorithm(nWays: Int) extends ReplacementAlgorithmType(nWays) {
  // An array of one-bit MRU registers, each for a way
  val mruBits = RegInit(VecInit(Seq.fill(nWays)(false.B)))

  /**
   * Finds the index of the LRU way in the set
   *
   * @return the index of the LRU way in the set
   */
  def getLru: UInt = {
    // Find the first 0 bit by inverting the bit registers and passing them to the priority encoder
    PriorityEncoder((~mruBits.asUInt).asUInt)
  }

  /**
   * Updates the selected MRU register and resets at capacity
   *
   * @param way the way that has been accessed
   */
  def updateLru(way: UInt): Unit = {
    mruBits(way) := true.B // Set the accessed way to 1

    // Check for capacity
    val capacity = ((~mruBits.asUInt).asUInt & ((~mruBits.asUInt).asUInt - 1.U)) === 0.U
    when(capacity) {
      for (i <- 0 until nWays) { // When at capacity, reset all bits except the accessed way bit
        when(i.U =/= way) {
          mruBits(i) := false.B // Set the accessed way to 1
        }
      }
    }
  }

  // Older function, requires more LUTs
//  def getLruOrderedSet: Vec[UInt] = {
//    val lruOrderedSet = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
//    val zeros = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
//    val ones = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
//
//    val onesCount = VecInit(Seq.fill(mruBits.length + 1)(0.U(log2Up(nWays).W)))
//    val zerosCount = VecInit(Seq.fill(mruBits.length + 1)(0.U(log2Up(nWays).W)))
//
//    for (i <- 0 until mruBits.length) {
//      val updateOnesCount = WireDefault(false.B)
//      val updateZerosCount = WireDefault(false.B)
//
//      when(mruBits(i) === false.B) {
//        zeros(zerosCount(i)) := i.U
//        updateZerosCount := true.B
//      } .otherwise {
//        ones(onesCount(i)) := i.U
//        updateOnesCount := true.B
//      }
//
//      zerosCount(i + 1) := Mux(updateZerosCount, zerosCount(i) + 1.U, zerosCount(i))
//      onesCount(i + 1) := Mux(updateOnesCount, onesCount(i) + 1.U, onesCount(i))
//    }
//
//    val shiftedOnes = ones.asUInt << (zerosCount(mruBits.length) << (log2Up(nWays) - 1).asUInt).asUInt
//    val orderedIdxs = zeros.asUInt + shiftedOnes.asUInt
//    for (i <- 0 until mruBits.length) {
//      val temp = orderedIdxs((i * wayIdxBits) + (wayIdxBits - 1), i * wayIdxBits)
//      lruOrderedSet(i) := temp
//    }
//    lruOrderedSet
//  }

  def getLruOrderedSet: Vec[UInt] = {
    val lruOrderedSet = VecInit(Seq.fill(nWays)(0.U(wayIdxBits.W)))
    val invertedMru = (~mruBits.asUInt).asUInt

    for(wayIdx <- 0 until nWays - 1) {
      lruOrderedSet(wayIdx) := PriorityEncoder(invertedMru(nWays - 1, wayIdx))
    }

    lruOrderedSet(nWays - 1) := (~lruOrderedSet(nWays - 2)).asUInt

    lruOrderedSet
  }

  when(io.update.valid) {
    updateLru(io.update.bits)
  }

  val lruOrderedSet = getLruOrderedSet
  val replaceWay = getLru

  io.replaceWay := replaceWay
  io.replacementSet := lruOrderedSet
}
