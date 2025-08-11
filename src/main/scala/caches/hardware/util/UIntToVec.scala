package caches.hardware.util

import chisel3._

object UIntToVec {
  /** Converts a UInt to a Vec of smaller UInts.
   *
   * @param value The UInt value to convert.
   * @param elemWidth The width of each element of the Vec.
   * @return A Vec of UInts representing the broken apart UInt into smaller UInts.
   */
  def apply(value: UInt, elemWidth: Int): Vec[UInt] = {
    val width = value.getWidth / elemWidth
    val vec = VecInit(Seq.fill(width)(0.U(elemWidth.W)))

    for (i <- 0 until width) {
      vec(i) := value(elemWidth - 1 + i * elemWidth, i * elemWidth)
    }

    vec
  }
}
