package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * Binary tree based implementation of Pseudo LRU
 *
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 * @param nCores number of cores sharing the cache
 */
class TreePlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, repSetFormat: BaseReplacementSetFormat = new NumericalFormat) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, repSetFormat = repSetFormat) {
  override def printConfig(): Unit = println(s"Tree PLRU replacement policy configuration: ways: $nWays, sets: $nSets, cores: $nCores" + "\n")

  // ---------------- Read stage ----------------
  val updateStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val updatedStageWbMruBits = WireDefault(0.U((nWays - 1).W))
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)

  val treePlruRead = Module(new TreePlruReadStage(nWays, nSets, repSetFormat))
  treePlruRead.io.stall := io.control.stall
  treePlruRead.io.rIdx := io.control.setIdx
  treePlruRead.io.wrEn := io.control.update.valid
  treePlruRead.io.wIdx := updateStageSetIdx
  treePlruRead.io.wData := updatedStageWbMruBits
  treePlruRead.io.fwd := updateStageSetIdx === idxDelayReg && io.control.update.valid

  val replaceWayPipeReg = PipelineReg(treePlruRead.io.replaceWay, 0.U, !io.control.stall)
  val replaceSetPipeReg = PipelineReg(treePlruRead.io.replacementSet, getDefaultRepSet, !io.control.stall)
  val mruBitsPipeReg = PipelineReg(treePlruRead.io.readState, 0.U, !io.control.stall)
  val setIdxPipeReg = PipelineReg(idxDelayReg, 0.U, !io.control.stall)

  // ---------------- Update stage ----------------
  val treePlruUpdate = Module(new TreePlruUpdateStage(nWays))
  treePlruUpdate.io.hitWay := io.control.update.bits
  treePlruUpdate.io.stateIn := mruBitsPipeReg

  // Forwarding signals
  updatedStageWbMruBits := treePlruUpdate.io.stateOut // Updated LRU bits to writeback to memory
  updateStageSetIdx := setIdxPipeReg

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.info <> 0.U.asTypeOf(io.info)
  io.scheduler <> 0.U.asTypeOf(io.scheduler)

  // We assign only the signals that are relevant to this policy
  io.control.replaceWay := replaceWayPipeReg
  io.control.isValid := true.B

  // Debugging signal
  repSet := replaceSetPipeReg
}

