package caches.hardware.util

import chisel3._

object UpdateSingleVecElem {
  /** Returns a pipeline register, which can be enabled or disabled. The register accepts a default value too.
   *
   * @example {{{
   * val pipeReg = PipelineReg(next, init, en)
   * }}}
   */
  def apply[T <: Data](orig: Vec[T], newVal: T, valIdx: UInt): Vec[T] = {
    val newVec = Wire(Vec(orig.length, chiselTypeOf(newVal)))

    for (idx <- 0 until orig.length) {
      when(idx.U === valIdx) {
        newVec(idx) := newVal
      } .otherwise{
        newVec(idx) := orig(idx)
      }
    }

    newVec
  }
}
