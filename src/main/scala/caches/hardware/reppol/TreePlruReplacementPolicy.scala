package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * @inheritdoc
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class TreePlruReplacementPolicy(ways: Int, sets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(ways, sets, nCores) {
  val setPlrus = Array.fill(sets)(Module(new TreePlruReplacementAlgorithm(ways)))

  for (i <- 0 until sets) {
    setPlrus(i).io.update.valid := io.control.update.valid && (io.control.setIdx === i.asUInt)
    setPlrus(i).io.update.bits := io.control.update.bits

    setReplaceWays(i) := setPlrus(i).io.replaceWay
  }

  io.control.replaceWay := setReplaceWays(io.control.setIdx)
  io.control.isValid := true.B
}

