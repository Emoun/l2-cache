package caches.hardware.reppol

import caches.hardware.util.Constants.TIMEOUT_LIMIT_WIDTH
import caches.hardware.util.PipelineReg
import chisel3._
import chisel3.util._

/**
 * ...
 *
 * @param nWays      number of ways in a cache set
 * @param nSets      number of sets in a cache
 * @param nCores     number of cores sharing the cache
 * @param basePolicy the base replacement policy module generating function
 */
class TimeoutReplacementPolicy(nWays: Int, nSets: Int, nCores: Int, basePolicy: () => SharedCacheReplacementPolicyType) extends SharedCacheReplacementPolicyType(nWays, nSets, nCores, TIMEOUT_LIMIT_WIDTH) {
  //--------------- Base Policy ---------------------
  val basePolicyInst = Module(basePolicy())

  // Update base policy
  basePolicyInst.io.control.setIdx := io.control.setIdx
  basePolicyInst.io.control.update.valid := io.control.update.valid
  basePolicyInst.io.control.update.bits := io.control.update.bits
  basePolicyInst.io.control.stall := io.control.stall
  // The rest of the signals are only relevant to timeout and contention policies, thus DontCare
  basePolicyInst.io.control.evict := DontCare
  basePolicyInst.io.control.updateCoreId := DontCare
  basePolicyInst.io.scheduler.cmd := DontCare
  basePolicyInst.io.scheduler.addr := DontCare
  basePolicyInst.io.scheduler.wData := DontCare
  basePolicyInst.io.control.isHit := DontCare
  basePolicyInst.io.control.missQueueEmpty := DontCare
  basePolicyInst.io.control.missQueueCores := DontCare
  basePolicyInst.io.control.missQueueValidCores := DontCare

  // Need to delay this signal by two CCs because PLRU has 2 stages
  val setIdxDelayReg = PipelineReg(io.control.setIdx, 0.U, !io.control.stall) // Delay once for accessing base policy mem
  val setIdxPipeReg = PipelineReg(setIdxDelayReg, 0.U, !io.control.stall) // Delay twice for computing replacement set

  //-------------------------------------

  val timeoutAlgo = Module(new TimeoutReplacementAlgorithm(nWays = nWays, nSets = nSets, nCores = nCores))
  timeoutAlgo.io.setIdx := setIdxPipeReg
  timeoutAlgo.io.evict := io.control.evict
  timeoutAlgo.io.update := io.control.update
  timeoutAlgo.io.updateCoreId := io.control.updateCoreId
  timeoutAlgo.io.baseCandidates := basePolicyInst.io.control.replacementSet
  timeoutAlgo.io.scheduler <> io.scheduler

  io.control.isValid := timeoutAlgo.io.isRepValid
  io.control.replaceWay := timeoutAlgo.io.replaceWay
  io.control.replacementSet := DontCare

  io.control.popRejQueue.valid := timeoutAlgo.io.freeRejQueue
  io.control.popRejQueue.bits := nCores.U // Free the entire rejection queue
  io.control.pushReqToCritQueue := false.B // TODO: set this appropriately
}
