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
  def plruBits(rIdx: UInt, wrEn: Bool, wIdx: UInt, wData: Vec[Bool]): Vec[Bool] = {
    val mruBits = Module(new MemBlock(nSets, nWays))
    val rMruBits = Wire(Vec(nWays, Bool()))

    mruBits.io.readAddr := rIdx
    mruBits.io.writeAddr := wIdx
    mruBits.io.writeData := wData.asUInt
    mruBits.io.wrEn := wrEn

    rMruBits := mruBits.io.readData.asBools

    rMruBits
  }

  val updateStageSetIdx = WireDefault(0.U(log2Up(nSets).W))
  val updatedStageMruBits = VecInit(Seq.fill(nWays)(false.B))

  val readMruBits = plruBits(io.control.setIdx, io.control.update.valid, updateStageSetIdx, updatedStageMruBits)
  val idxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall)

  // ---------------- Read stage ----------------

  val doForward = updateStageSetIdx === idxDelayReg && io.control.update.valid
  val computeMruBits = Mux(doForward, updatedStageMruBits, readMruBits)

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
  updatedStageMruBits := bitPlruAlgorithm.io.updatedMru
  updateStageSetIdx := setIdxPipeReg

  io.control.replaceWay := replaceWayPipeReg
  io.control.replacementSet := replaceSetPipeReg
  io.control.isValid := true.B
  io.scheduler.rData := DontCare // Bit PLRU does not use scheduler control
}
