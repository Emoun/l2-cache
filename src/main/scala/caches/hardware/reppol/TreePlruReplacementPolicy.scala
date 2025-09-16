package caches.hardware.reppol

import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * @param nWays number of ways in a single cache set
 * @param nSets number of sets in the whole cache
 */
class TreePlruReplacementPolicy(nWays: Int, nSets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores) {
  val setMruBits = Array.fill(nSets)(RegInit(VecInit(Seq.fill(nWays - 1)(false.B))))

  val selMruBits = VecInit(Seq.fill(nWays - 1)(false.B))

  // Multiplexer for selecting the sets mru bits
  for (setIdx <- 0 until nSets) {
    when(setIdx.U === io.control.setIdx) {
      selMruBits := setMruBits(setIdx)
    }
  }

  val treePlruAlgo = Module(new TreePlruReplacementAlgorithm(nWays))
  treePlruAlgo.io.mruBits := selMruBits
  treePlruAlgo.io.hitWay := io.control.update.bits

  val updatedMruBits = treePlruAlgo.io.updatedMru
  val replaceWay = treePlruAlgo.io.replaceWay
  val replaceSet = treePlruAlgo.io.replacementSet

  // Demultiplexer for updating the mru bits
  for (setIdx <- 0 until nSets) {
    when(setIdx.U === io.control.setIdx && io.control.update.valid) {
      setMruBits(setIdx) := updatedMruBits
    }
  }

  // ---------------- Eviction stage ----------------

  val replaceWayPipeReg = PipelineReg(replaceWay, 0.U, !io.control.stall)
  val replaceSetPipeReg = PipelineReg(replaceSet, VecInit(Seq.fill(nWays)(0.U(log2Up(nWays).W))), !io.control.stall)

  io.control.replaceWay := replaceWay
  io.control.replaceWay := replaceWayPipeReg
  io.control.replacementSet := replaceSetPipeReg
  io.control.isValid := true.B
  io.scheduler.rData := DontCare
  io.control.popRejQueue.valid := DontCare
  io.control.popRejQueue.bits := DontCare
  io.control.pushReqToCritQueue := DontCare
}

