package caches.hardware.reppol

import chisel3._

class BitPlruUpdateStage(nWays: Int) extends BasePolicyUpdateStageType(nWays, nWays) {
  override def update(hitWay: UInt, state: UInt): UInt = {
    val newMruBits = VecInit(Seq.fill(nWays)(false.B))

    // TODO: Need to find a way to reset the mru bits if the remaining 0 bits are owned by critical ways

    // Check for capacity
    val capacity = ((~state.asUInt).asUInt & ((~state.asUInt).asUInt - 1.U)) === 0.U

    for (bitIdx <- 0 until nWays) {
      when(capacity && (bitIdx.U =/= hitWay).asBool) { // When at capacity, reset all bits except the accessed way bit
        newMruBits(bitIdx) := false.B
      }.otherwise {
        newMruBits(bitIdx) := state(bitIdx)
      }
    }

    newMruBits(hitWay) := true.B // Update the accessed way

    newMruBits.asUInt
  }

  io.stateOut := update(io.hitWay, io.stateIn)
}
