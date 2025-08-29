package caches.hardware.reppol

import chisel3._

class TimeoutReplacementPolicy (ways: Int, sets: Int, nCores: Int) extends SharedCacheReplacementPolicyType(ways, sets, nCores, 0) {
  // TODO: Implement
  io.scheduler.rData := DontCare
  io.control.popRejQueue.valid := DontCare
  io.control.popRejQueue.bits := DontCare
}
