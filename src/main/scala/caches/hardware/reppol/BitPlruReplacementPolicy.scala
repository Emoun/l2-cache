package caches.hardware.reppol

import chisel3._

/**
 * @inheritdoc
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class BitPlruReplacementPolicy(ways: Int, sets: Int) extends ReplacementPolicyType(ways, sets) {
  val setPlrus = Array.fill(sets)(Module(new BitPlru(ways)))

  for (i <- 0 until sets) {
    setPlrus(i).io.update.valid := io.update.valid && (io.setIdx === i.asUInt)
    setPlrus(i).io.update.bits := io.update.bits

    setReplaceWays(i) := setPlrus(i).io.replaceWay
  }

  io.replaceWay := setReplaceWays(io.setIdx)
  io.isValid := true.B
}
