package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * @inheritdoc
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class BitPlruReplacementPolicy(ways: Int, sets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(ways, sets, nCores) {
  val setPlrus = Array.fill(sets)(Module(new BitPlruReplacementAlgorithm(ways)))

  // ---------------- Base policy stage ----------------

  for (set <- 0 until sets) {
    setPlrus(set).io.update.valid := io.control.update.valid && (io.control.setIdx === set.asUInt)
    setPlrus(set).io.update.bits := io.control.update.bits

    setReplaceWays(set) := setPlrus(set).io.replaceWay
  }

  // ---------------- Eviction stage ----------------

  val replaceWayPipeReg = pipelineReg(setReplaceWays(io.control.setIdx), 0.U, !io.control.stall)

  io.control.replaceWay := replaceWayPipeReg
  io.control.isValid := true.B
}
