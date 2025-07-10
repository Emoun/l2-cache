package caches.hardware.reppol

import chisel3._
import chisel3.util._
import chisel3.util.PriorityEncoder

/**
 * Bit based implementation of Pseudo LRU
 *
 * @param ways number of ways in a set-associate cache
 */
class BitPlruReplacementAlgorithm(ways: Int) extends ReplacementAlgorithmType(ways) {
  // An array of one-bit MRU registers, each for a way
  val mruBits = RegInit(VecInit(Seq.fill(ways)(false.B)))

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
      for (i <- 0 until ways) { // When at capacity, reset all bits except the accessed way bit
        when(i.U =/= way) {
          mruBits(i) := false.B // Set the accessed way to 1
        }
      }
    }
  }

  def getLruOrderedSet: Vec[UInt] = {
    val lruOrderedSet = VecInit(Seq.fill(ways)(0.U(wayIdxBits.W)))
    val zeros = VecInit(Seq.fill(ways)(0.U(wayIdxBits.W)))
    val ones = VecInit(Seq.fill(ways)(0.U(wayIdxBits.W)))

    val onesCount = VecInit(Seq.fill(mruBits.length + 1)(0.U(log2Up(ways).W)))
    val zerosCount = VecInit(Seq.fill(mruBits.length + 1)(0.U(log2Up(ways).W)))

    for (i <- 0 until mruBits.length) {
      val updateOnesCount = WireDefault(false.B)
      val updateZerosCount = WireDefault(false.B)

      when(mruBits(i) === false.B) {
        zeros(zerosCount(i)) := i.U
        updateZerosCount := true.B
      } .otherwise {
        ones(onesCount(i)) := i.U
        updateOnesCount := true.B
      }

      zerosCount(i + 1) := Mux(updateZerosCount, zerosCount(i) + 1.U, zerosCount(i))
      onesCount(i + 1) := Mux(updateOnesCount, onesCount(i) + 1.U, onesCount(i))
    }

    val shiftedOnes = ones.asUInt << (zerosCount(mruBits.length) << (log2Up(ways) - 1).asUInt).asUInt
    val orderedIdxs = zeros.asUInt + shiftedOnes.asUInt
    for (i <- 0 until mruBits.length) {
      val temp = orderedIdxs((i * wayIdxBits) + (wayIdxBits - 1), i * wayIdxBits)
      lruOrderedSet(i) := temp
    }
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
