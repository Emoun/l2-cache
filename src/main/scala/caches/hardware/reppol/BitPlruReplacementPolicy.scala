package caches.hardware.reppol

import chisel3._
import chisel3.util._
import caches.hardware.util.{MemBlock, PipelineReg}

/**
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class BitPlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores) {
  // ---------------- Base policy stage ----------------
  def plruBits(rIdx: UInt, wrEn: Bool, wIdx: UInt, wData: Vec[Bool], stall: Bool): Vec[Bool] = {
    val mruBits = Module(new MemBlock(nSets, nWays))
    val rMruBits = Wire(Vec(nWays, Bool()))

    mruBits.io.readAddr := rIdx
    mruBits.io.writeAddr := wIdx
    mruBits.io.writeData := wData.asUInt
    mruBits.io.wrEn := wrEn
    mruBits.io.stall := stall

    rMruBits := mruBits.io.readData.asBools

    rMruBits
  }

  // ---------------- Pre-Read stage ----------------

  val updateStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val updatedStageWbMruBits = VecInit(Seq.fill(nWays)(false.B))

  val readMruBits = plruBits(rIdx = io.control.setIdx, wrEn = io.control.update.valid, wIdx = updateStageSetIdx, wData = updatedStageWbMruBits, stall = io.control.stall)
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)

  // ---------------- Read stage ----------------

  val doForward = updateStageSetIdx === idxDelayReg && io.control.update.valid
  val computeMruBits = Mux(doForward, updatedStageWbMruBits, readMruBits)

  val bitPlruAlgorithm = Module(new BitPlruReplacementAlgorithm(nWays))
  bitPlruAlgorithm.io.computeMruBits := computeMruBits

  val replaceWay = bitPlruAlgorithm.io.replaceWay
  val replaceSet = bitPlruAlgorithm.io.replacementSet

  val mruBitsPipeReg = PipelineReg(computeMruBits, VecInit(Seq.fill(nWays)(false.B)), !io.control.stall)
  val setIdxPipeReg = PipelineReg(idxDelayReg, 0.U, !io.control.stall)
  val replaceWayPipeReg = PipelineReg(replaceWay, 0.U, !io.control.stall)
  val replaceSetPipeReg = PipelineReg(replaceSet, VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W))), !io.control.stall)

  // ---------------- Update stage ----------------
  bitPlruAlgorithm.io.hitWay := io.control.update.bits
  bitPlruAlgorithm.io.updateMruBits := mruBitsPipeReg
  updatedStageWbMruBits := bitPlruAlgorithm.io.updatedMru // Updated MRU bits to writeback to memory
  updateStageSetIdx := setIdxPipeReg

  // Default output assignments
  io.control <> 0.U.asTypeOf(io.control)
  io.scheduler <> 0.U.asTypeOf(io.scheduler)

  // We assign only the signals that are relevant to this policy
  io.control.replaceWay := replaceWayPipeReg
  io.control.replacementSet := replaceSetPipeReg
  io.control.isValid := true.B
}
