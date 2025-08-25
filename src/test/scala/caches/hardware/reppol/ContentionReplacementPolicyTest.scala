package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ContentionReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  def setCoreAsCritical(dut: ContentionReplacementPolicy, coreID: Int, contentionLimit: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.WR)
    dut.io.scheduler.addr.poke(coreID.U)
    dut.io.scheduler.wData.poke(contentionLimit.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }

  def unsetCoreAsCritical(dut: ContentionReplacementPolicy, coreID: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.scheduler.addr.poke(coreID.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
  }

  def performEvictionRequest(dut: ContentionReplacementPolicy, coreId: Int, setIdx: Int, expectedEvictionCandidate: Option[Int] = None): Unit = {
    val expectedCandidate = expectedEvictionCandidate match {
      case Some(data) => data.U
      case None => 0.U
    }

    val evictionSetEmpty = expectedEvictionCandidate match {
      case Some(_) => true.B
      case None => false.B
    }

    // Make a request on behalf of the requesting core
    dut.io.control.setIdx.poke(setIdx.U)

    dut.clock.step(1) // Need one clock cycle delay here since the base policy is separated from contention logic by a pipeline stage

    dut.io.control.evict.poke(true.B)
    dut.io.control.coreId.poke(coreId.U)

    dut.io.control.replaceWay.expect(expectedCandidate)
    dut.io.control.isValid.expect(evictionSetEmpty)

    dut.clock.step(1)

    dut.io.control.coreId.poke(0.U)
    dut.io.control.evict.poke(false.B)
    dut.io.control.setIdx.poke(0.U)
  }

  def performUpdateRequest(dut: ContentionReplacementPolicy, coreId: Int, setIdx: Int, hitWay: Int): Unit = {
    dut.io.control.setIdx.poke(setIdx.U)
    dut.io.control.update.valid.poke(true.B)
    dut.io.control.update.bits.poke(hitWay.U)
    dut.io.control.coreId.poke(coreId.U)

    dut.clock.step(1)

    dut.io.control.coreId.poke(0.U)
    dut.io.control.update.valid.poke(false.B)
    dut.io.control.update.bits.poke(0.U)
    dut.io.control.setIdx.poke(0.U)
  }

  def defaultAssignments(dut: ContentionReplacementPolicy): Unit = {
    dut.io.control.update.valid.poke(false.B)
    dut.io.control.update.bits.poke(0.U)
    dut.io.control.setIdx.poke(0.U)
    dut.io.control.coreId.poke(0.U)
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }

  "ContentionReplacementPolicy" should "reach contention limit for BitPlru" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)
      dut.clock.step(1)

      // Expect the policy to output the first way of the base policy as the LRU way
      dut.io.control.replaceWay.expect(0.U)
      dut.io.control.isValid.expect(true.B)

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 2)

      dut.clock.step(1)

      // Assign the three ways to first core first
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 2)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 3)

      dut.clock.step(1)

      // Evict critical core lines
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      // Evict critical core lines
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 1)

      dut.clock.step(1)

      // Evict a non-critical line since we now cannot evict the LRU way since it's core has reached a limit, so we evict
      // the second LRU way
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 3)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      // Expect that all cores are now assigned to a critical core that has reached capacity
      performEvictionRequest(dut, coreId = 3, setIdx = 0, expectedEvictionCandidate = None)
      dut.clock.step(1)

      // Unset the core as critical
      unsetCoreAsCritical(dut, coreID = 1)

      dut.clock.step(1)

      // Expect that a non-critical core can evict a previously critical core line
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      dut.clock.step(1)
    }
  }

  "ContentionReplacementPolicy" should "Critical evicted by non-critical" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      dut.clock.step()

      setCoreAsCritical(dut, coreID = 0, contentionLimit = 1)

      // Assign all ways to critical core
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 2)

      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 3)

      // Perform eviction using non-critical core, which should trigger an event
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)
      dut.clock.step()

      // Now try to again to evict.
      // Because the most recently used is the only non-critical (and non-limited) it should be evicted
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
    }
  }

  "ContentionReplacementPolicy" should "Critical evicted by critical" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      dut.clock.step()

      setCoreAsCritical(dut, coreID = 0, contentionLimit = 0)
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 10)

      // Assign LRU to limited critical and rest to non-critical
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 3)

      // Perform eviction using other critical core. Because LRU is limited, should not be evicted
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
    }
  }

  "ContentionReplacementPolicy" should "Miss-in-Miss" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      dut.clock.step()

      // Set the first core as critical with a contention limit of 1
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 1)

      // Assign all ways to critical core except least recent
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 0)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 2)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 3)

      // Perform eviction using critical core during a miss, which should trigger an event
      dut.io.control.missActive.poke(true.B)
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)
      dut.clock.step()

      // Now try to evict the critical core using non-critical
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = None)
    }
  }

  "ContentionReplacementPolicy" should "Miss-in-Miss No Unlimited" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      dut.clock.step()

      // Set the first core as critical with a contention limit of 1
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 1)
      // Set the second core as critical with a contention limit of 0
      setCoreAsCritical(dut, coreID = 2, contentionLimit = 0)

      // Assign all ways to limited critical
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 3)

      // Perform eviction using unlimited critical core during a miss, which should trigger an event
      dut.io.control.missActive.poke(true.B)
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)
      dut.clock.step()

      // Now try to evict the critical core using non-critical
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = None)
    }
  }

  "ContentionReplacementPolicy" should "Miss-in-Miss with eviction event" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      dut.clock.step()

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 0, contentionLimit = 2)

      // Assign one way to critical core and rest to uncritical
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 3)

      // Perform eviction using unlimited critical core during a miss, which should trigger 2 events:
      // One eviction event and one miss-in-miss
      dut.io.control.missActive.poke(true.B)
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 0)
      dut.clock.step()

      // Now use all the non-critical lines to reset the critical core to least recently used
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 1)
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 2)
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 3)

      // Now try to evict the critical core using non-critical, should evict non-critical instead of limited LRU
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(1))
    }
  }
}
