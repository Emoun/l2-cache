package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * @inheritdoc
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class BitPlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores) {
  // ---------------- Base policy stage ----------------

  val setMruBits = Array.fill(nSets)(RegInit(VecInit(Seq.fill(nWays)(false.B))))

  val selMruBits = VecInit(Seq.fill(nWays)(false.B))

  // Multiplexer for selecting the sets mru bits
  for (setIdx <- 0 until nSets) {
    when(setIdx.U === io.control.setIdx){
      selMruBits := setMruBits(setIdx)
    }
  }

  val bitPlruAlgorithm = Module(new BitPlruReplacementAlgorithm(nWays))
  bitPlruAlgorithm.io.mruBits := selMruBits
  bitPlruAlgorithm.io.hitWay := io.control.update.bits

  val updatedMruBits = bitPlruAlgorithm.io.updatedMru
  val replaceWay = bitPlruAlgorithm.io.replaceWay
  val replaceSet = bitPlruAlgorithm.io.replacementSet

  // Demultiplexer for updating the mru bits
  for (setIdx <- 0 until nSets) {
    when(setIdx.U === io.control.setIdx && io.control.update.valid){
      setMruBits(setIdx) := updatedMruBits
    }
  }

  // ---------------- Eviction stage ----------------

  val replaceWayPipeReg = PipelineReg(replaceWay, 0.U, !io.control.stall)
  val replaceSetPipeReg = PipelineReg(replaceSet, VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W))), !io.control.stall)

  io.control.replaceWay := replaceWayPipeReg
  io.control.replacementSet := replaceSetPipeReg
  io.control.isValid := true.B
}
