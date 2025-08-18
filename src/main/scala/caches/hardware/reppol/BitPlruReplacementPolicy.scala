package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class BitPlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, 1) {
  // ---------------- Base policy stage ----------------

  val readMruBits = VecInit(Seq.fill(nWays)(false.B))
  val updateStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val updatedMruBits = VecInit(Seq.fill(nWays)(false.B))

  val setMruBits = Array.fill(nSets)(RegInit(VecInit(Seq.fill(nWays)(false.B))))

  for (setIdx <- 0 until nSets) {
    // Multiplexer for selecting the mru bits
    when(setIdx.U === io.control.setIdx) {
      readMruBits := setMruBits(setIdx)
    }

    // Demultiplexer for updating the mru bits
    when(setIdx.U === updateStageSetIdx && io.control.update.valid) {
      setMruBits(setIdx) := updatedMruBits
    }
  }

  val doForward = updateStageSetIdx === io.control.setIdx && io.control.update.valid
  val computeMruBits = Mux(doForward, updatedMruBits, readMruBits)

  val bitPlruAlgorithm = Module(new BitPlruReplacementAlgorithm(nWays))
  bitPlruAlgorithm.io.computeMruBits := computeMruBits

  val replaceWay = bitPlruAlgorithm.io.replaceWay
  val replaceSet = bitPlruAlgorithm.io.replacementSet

  val mruBitsPipeReg = PipelineReg(computeMruBits, VecInit(Seq.fill(nWays)(false.B)), !io.control.stall)
  val setIdxPipeReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)
  val replaceWayPipeReg = PipelineReg(replaceWay, 0.U, !io.control.stall)
  val replaceSetPipeReg = PipelineReg(replaceSet, VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W))), !io.control.stall)

  // ---------------- Update stage ----------------
  bitPlruAlgorithm.io.hitWay := io.control.update.bits
  bitPlruAlgorithm.io.updateMruBits := mruBitsPipeReg
  updatedMruBits := bitPlruAlgorithm.io.updatedMru
  updateStageSetIdx := setIdxPipeReg

  io.control.replaceWay := replaceWayPipeReg
  io.control.replacementSet := replaceSetPipeReg
  io.control.isValid := true.B
  io.scheduler.rData := DontCare // Bit PLRU does not use scheduler control
}
