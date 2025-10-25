package caches.hardware.reppol

import chisel3._
import chisel3.util._

class TreePlruUpdateStage(nWays: Int) extends BasePolicyUpdateStageType(nWays, nWays - 1) {
  val wayIdxBits = log2Up(nWays)

  override def update(hitWay: UInt, state: UInt): UInt = {
    val lruBits = VecInit(state.asBools)
    val treePath = VecInit(Seq.fill(wayIdxBits)(0.U(wayIdxBits.W)))

    val newLru = VecInit(Seq.fill(nWays - 1)(false.B))
    for (j <- 0 until lruBits.length) { // Copy the lru bits
      newLru(j) := lruBits(j)
    }

    for (i <- 0 until wayIdxBits) {
      val accessBit = hitWay(wayIdxBits - 1 - i)
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

    newLru.asUInt
  }

  io.stateOut := update(io.hitWay, io.stateIn)
}
