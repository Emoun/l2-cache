package caches.hardware.reppol

import chisel3._
import chisel3.util._

/**
 * @inheritdoc
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class TreePlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores) {
  val setPlrus = Array.fill(nSets)(Module(new TreePlruReplacementAlgorithm(nWays)))

  for (i <- 0 until nSets) {
    setPlrus(i).io.update.valid := io.control.update.valid && (io.control.setIdx === i.asUInt)
    setPlrus(i).io.update.bits := io.control.update.bits

    setReplaceWays(i) := setPlrus(i).io.replaceWay
  }

  io.control.replacementSet := VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W)))
  io.control.replaceWay := setReplaceWays(io.control.setIdx)
  io.control.isValid := true.B
}

