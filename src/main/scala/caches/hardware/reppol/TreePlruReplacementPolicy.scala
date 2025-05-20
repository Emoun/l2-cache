package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * @inheritdoc
 * @param ways number of ways in a single cache set
 * @param sets number of sets in the whole cache
 */
class TreePlruReplacementPolicy(ways: Int, sets: Int) extends ReplacementPolicyType(ways, sets) {
  val setPlrus = Array.fill(sets)(Module(new TreePlru(ways)))
  val setReplaceWays = VecInit(Seq.fill(sets)(0.U(log2Up(ways).W)))

  for (i <- 0 until sets) {
    setPlrus(i).io.update.valid := io.update.valid && (io.index === i.asUInt)
    setPlrus(i).io.update.bits := io.update.bits

    setReplaceWays(i) := setPlrus(i).io.replaceWay
  }

  io.replaceWay := setReplaceWays(io.index)
}

