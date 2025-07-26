package caches.hardware.util

import chisel3._

object UpdateSingleVecElem {
  /** Updates a single element in a Vec.
   *
   * @example {{{
   * val updatedVec = UpdateSingleVecElem(Vec(0.U, 1.U, 2.U), 3.U, 1.U)
   * }}}
   */
  def apply[T <: Data](orig: Vec[T], newVal: T, idx: UInt): Vec[T] = {
    val newVec = Wire(Vec(orig.length, chiselTypeOf(newVal)))

    for (i <- 0 until orig.length) {
      when(i.U === idx) {
        newVec(i) := newVal
      } .otherwise{
        newVec(i) := orig(i)
      }
    }

    newVec
  }
}
