package caches.hardware.reppol

import chisel3._

/**
 * @inheritdoc
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class BitPlruReplacementPolicy(ways: Int, sets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(ways, sets, nCores) {
  val setPlrus = Array.fill(sets)(Module(new BitPlruReplacementAlgorithm(ways)))

  for (set <- 0 until sets) {
    setPlrus(set).io.update.valid := io.control.update.valid && (io.control.setIdx === set.asUInt)
    setPlrus(set).io.update.bits := io.control.update.bits

    setReplaceWays(set) := setPlrus(set).io.replaceWay
  }

  io.control.replaceWay := setReplaceWays(io.control.setIdx)
  io.control.isValid := true.B
}
