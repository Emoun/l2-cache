package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ContentionReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  def setCoreAsCritical(dut: ContentionReplacementPolicy, coreID: Int, contentionLimit: Int): Unit = {
    dut.io.setCritical.valid.poke(true.B)
    dut.io.setCritical.bits.poke(coreID.U)
    dut.io.contentionLimit.poke(contentionLimit.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.setCritical.valid.poke(false.B)
    dut.io.setCritical.bits.poke(0.U)
    dut.io.contentionLimit.poke(0.U)
  }

  def unsetCoreAsCritical(dut: ContentionReplacementPolicy, coreID: Int): Unit = {
    dut.io.unsetCritical.valid.poke(true.B)
    dut.io.unsetCritical.bits.poke(coreID.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.unsetCritical.valid.poke(false.B)
    dut.io.unsetCritical.bits.poke(0.U)
  }

  def performEvictionRequest(dut: ContentionReplacementPolicy, coreID: Int, expectedEvictionCandidate: Int, evictionSetNotEmpty: Boolean): Unit = {
    // Make a request on behalf of the requesting core
    dut.io.evict.poke(true.B)
    dut.io.reqID.poke(coreID.U)

    dut.io.replaceWay.expect(expectedEvictionCandidate.U)
    dut.io.isValid.expect(evictionSetNotEmpty.B)

    dut.clock.step(1)

    // Update the base policy as if we had a hit
    dut.io.update.valid.poke(evictionSetNotEmpty.B) // Only update the base policy if we are allowed to evict
    dut.io.update.bits.poke(expectedEvictionCandidate.U) // Need to update base policy with a hit to an eviction candidate
    dut.io.evict.poke(false.B)

    dut.clock.step(1)

    dut.io.update.valid.poke(false.B)
    dut.io.update.bits.poke(0.U)
    dut.io.reqID.poke(0.U)
  }

  "ContentionReplacementPolicy" should "reach contention limit for two cores and two ways" in {
    val (ways, sets, nCores, missLatency) = (2, 1, 2, 3)
    test(new ContentionReplacementPolicy(ways, sets, nCores, missLatency, () => new BitPlru(ways))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.update.valid.poke(false.B)
      dut.io.update.bits.poke(0.U)
      dut.io.setIdx.poke(0.U)
      dut.io.reqID.poke(0.U)
      dut.io.setCritical.valid.poke(false.B)
      dut.io.setCritical.bits.poke(0.U)
      dut.io.unsetCritical.valid.poke(false.B)
      dut.io.unsetCritical.bits.poke(0.U)
      dut.io.contentionLimit.poke(0.U)

      dut.clock.step(1)

      // Expect the policy to output the first way of the base policy as the LRU way
      dut.io.replaceWay.expect(0.U)
      dut.io.isValid.expect(true.B)

      // Set the first core as critical with a contention limit of 4, given that miss latency is 3 a single
      // contention event will for the core to reach the contention limit
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 4)

      dut.clock.step(1)

      // Expect the policy to still output the first way of the base policy as the LRU way
      // This will force the first way to be assigned to a critical core
      performEvictionRequest(dut, coreID = 1, expectedEvictionCandidate = 0, evictionSetNotEmpty = true)

      dut.clock.step(1)

      // Expect the policy to point to the second way now, since second way is not assigned to any core
      // This will force the second way to be assigned to a critical core too
      performEvictionRequest(dut, coreID = 1, expectedEvictionCandidate = 1, evictionSetNotEmpty = true)

      dut.clock.step(1)

      // Expect the policy to point to the first way again, since the critical core hasn't reached the contention limit
      // Eviction by a low priority core will force the policy to evict the critical core line, and reach the maximum contention limit
      performEvictionRequest(dut, coreID = 0, expectedEvictionCandidate = 0, evictionSetNotEmpty = true)

      dut.clock.step(1)

      // Expect the policy to point to the first way again, since second way belongs to a critical core
      // that has reached the contention limit
      performEvictionRequest(dut, coreID = 0, expectedEvictionCandidate = 0, evictionSetNotEmpty = true)

      dut.clock.step(1)

      // Expect the policy to point to the first way again, since it is not assigned to a critical core
      // since it is preferable to evict a low priority core line instead of a critical core line
      // This will force the first line to be assigned to a critical core, i.e. core 1
      performEvictionRequest(dut, coreID = 1, expectedEvictionCandidate = 0, evictionSetNotEmpty = true)

      dut.clock.step(1)

      // Expect the policy to reject our request, since both ways are assigned to a critical core
      // whose contention limit has been reached
      performEvictionRequest(dut, coreID = 0, expectedEvictionCandidate = 0, evictionSetNotEmpty = false)

      dut.clock.step(1)

      unsetCoreAsCritical(dut, coreID = 1)

      dut.clock.step(1)

      // Expect the that the policy will allow us to evict a way previously assigned to a critical core
      // Expect the LRU way this time to be the second way
      performEvictionRequest(dut, coreID = 0, expectedEvictionCandidate = 1, evictionSetNotEmpty = true)

      dut.clock.step(1)

      // Expect the that the policy will allow us to evict the first way too
      performEvictionRequest(dut, coreID = 0, expectedEvictionCandidate = 0, evictionSetNotEmpty = true)

      dut.clock.step(1)
    }
  }
}
