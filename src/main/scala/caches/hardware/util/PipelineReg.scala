package caches.hardware.util

import chisel3._

object PipelineReg {
  /** Returns a pipeline register, which can be enabled or disabled. The register accepts a default value too.
   *
   * @example {{{
   * val pipeReg = PipelineReg(next, init, en)
   * }}}
   */
  def apply[T <: Data](next: T, init: T, en: Bool): T = {
    val pipelineReg = RegInit(init)
    when(en) {
      pipelineReg := next
    }
    pipelineReg
  }
}