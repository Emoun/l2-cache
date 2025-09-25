package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import caches.hardware.reppol.ReplacementPolicyTest._

class ContentionReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {

  "ContentionReplacementPolicy" should "Reach contention limit for BitPlru" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)
      val workingSet = 1
      dut.clock.step(1)

      // Expect the policy to output the first way of the base policy as the LRU way
      dut.io.control.replaceWay.expect(0.U)
      dut.io.control.isValid.expect(true.B)

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 1, wData = 2)

      dut.clock.step(1)

      // Assign the three ways to first core first
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      dut.clock.step(1)

      // Evict critical core lines
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      dut.clock.step(1)

      // Evict critical core lines
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      dut.clock.step(1)

      // Evict a non-critical line since we now cannot evict the LRU way since it's core has reached a limit, so we evict
      // the second LRU way
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 3)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      dut.clock.step(1)

      // Expect that all cores are now assigned to a critical core that has reached capacity
      performEvictionRequest(dut, coreId = 3, setIdx = workingSet, expectedEvictionCandidate = None)
      dut.clock.step(1)

      // Unset the core as critical
      unsetCoreAsCritical(dut, coreID = 1)

      dut.clock.step(1)

      // Expect that a non-critical core can evict a previously critical core line
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      dut.clock.step(1)
    }
  }

  "ContentionReplacementPolicy" should "Critical evicted by non-critical" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      setCoreAsCritical(dut, coreID = 2, wData = 1)

      // Assign all ways to critical core
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Perform eviction using non-critical core, which should trigger an event
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)
      dut.clock.step()

      // Now try again to evict.
      // Because the most recently used is the only non-critical (and non-limited) it should be evicted
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
    }
  }

  "ContentionReplacementPolicy" should "Critical evicted by critical" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      setCoreAsCritical(dut, coreID = 0, wData = 0)
      setCoreAsCritical(dut, coreID = 1, wData = 10)

      // Assign LRU to limited critical and rest to non-critical
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Perform eviction using other critical core. Because LRU is limited, should not be evicted
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
    }
  }

  "ContentionReplacementPolicy" should "Miss-in-Miss" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), enableMissInMiss = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      // Set the first core as critical with a contention limit of 1
      setCoreAsCritical(dut, coreID = 1, wData = 1)

      // Assign all ways to critical core except least recent
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 3)

      // Perform eviction using critical core during a miss, which should trigger an event
      dut.io.control.missQueueEmpty.poke(false.B)
      dut.io.control.missQueueCores(0).poke(2.U)
      dut.io.control.missQueueValidCores(0).poke(true.B)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)
      dut.clock.step()

      dut.io.control.missQueueEmpty.poke(true.B)
      dut.io.control.missQueueCores(0).poke(0.U)
      dut.io.control.missQueueValidCores(0).poke(false.B)

      // Now try to evict the critical core using non-critical
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = None)
    }
  }

  "ContentionReplacementPolicy" should "Miss-in-Miss No Unlimited" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), enableMissInMiss = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      // Set the first core as critical with a contention limit of 1
      setCoreAsCritical(dut, coreID = 1, wData = 1)
      // Set the second core as critical with a contention limit of 0
      setCoreAsCritical(dut, coreID = 2, wData = 0)

      // Assign all ways to limited critical
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Perform eviction using unlimited critical core during a miss, which should trigger an event
      dut.io.control.missQueueEmpty.poke(false.B)
      dut.io.control.missQueueCores(0).poke(0.U)
      dut.io.control.missQueueValidCores(0).poke(true.B)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)
      dut.clock.step()

      dut.io.control.missQueueEmpty.poke(true.B)
      dut.io.control.missQueueCores(0).poke(0.U)
      dut.io.control.missQueueValidCores(0).poke(false.B)

      // Now try to evict the critical core using non-critical
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = None)
    }
  }

  "ContentionReplacementPolicy" should "Miss-in-Miss with eviction event" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), enableMissInMiss = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 0, wData = 2)

      // Assign one way to critical core and rest to uncritical
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Perform eviction using unlimited critical core during a miss, which should trigger 2 events:
      // One eviction event and one miss-in-miss
      dut.io.control.missQueueEmpty.poke(false.B)
      dut.io.control.missQueueCores(0).poke(2.U)
      dut.io.control.missQueueValidCores(0).poke(true.B)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)
      dut.clock.step()

      dut.io.control.missQueueEmpty.poke(true.B)
      dut.io.control.missQueueCores(0).poke(0.U)
      dut.io.control.missQueueValidCores(0).poke(false.B)

      // Now use all the non-critical lines to reset the critical core to least recently used
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Now try to evict the critical core using non-critical, should evict non-critical instead of limited LRU
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
    }
  }

  "ContentionReplacementPolicy" should "Precedent events" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), enablePrecedentEvents = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 0, wData = 2)

      // Assign some lines to the critical core
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Bring the contention limit up by accessing a line that was brought in by a non-critical core
      dut.io.control.isHit.poke(true.B)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)
      dut.clock.step()

      // Now use non-critical cores to evict the critical core's lines, thus reach the contention limit
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 2)

      // Bring in a new line by a critical core that should not be evicted
      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      // Get the plru to point to the critical core's line
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Now try to evict the critical line using non-critical core, should evict non-critical instead
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))

      dut.clock.step()
    }
  }

  "ContentionReplacementPolicy" should "Miss-queue events" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), missQueueDepth = 4, enableMissInMiss = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 2, wData = 2)

      // Assign some lines to cores
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      // Make plru point to the first non-critical line
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      // Bring the contention limit down by indicating that there is currently two outstanding misses in the queue
      dut.io.control.missQueueEmpty.poke(false.B)
      dut.io.control.missQueueCores(0).poke(1.U)
      dut.io.control.missQueueCores(1).poke(0.U)
      dut.io.control.missQueueValidCores(0).poke(true.B)
      dut.io.control.missQueueValidCores(1).poke(true.B)

      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      dut.io.control.missQueueEmpty.poke(true.B)
      dut.io.control.missQueueCores(0).poke(0.U)
      dut.io.control.missQueueCores(1).poke(0.U)
      dut.io.control.missQueueValidCores(0).poke(false.B)
      dut.io.control.missQueueValidCores(1).poke(false.B)

      // Get the plru to point to the critical core's line
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 3)

      // Now try to evict the critical line using non-critical core, should evict non-critical line instead
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))

      dut.clock.step()
    }
  }

  "ContentionReplacementPolicy" should "WB events" in {
    val (nWays, nSets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(nWays, nSets, nCores, () => new BitPlruReplacementPolicy(nWays, nSets, nCores), missQueueDepth = 4, enableMissInMiss = true, enableWbEvents = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut)

      val workingSet = 1
      dut.clock.step()

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 2, wData = 2)

      // Assign some lines to cores
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 0)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      performEvictionRequest(dut, coreId = 0, setIdx = workingSet, expectedEvictionCandidate = Some(3))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      // Simulate a WB, the wb entry is critical, thus a wb event should not be triggered
      dut.io.control.nonCritWbPop.poke(true.B)
      dut.io.control.nonCritWbEntryIsCrit.poke(true.B)
      // Simulate a critical core being in the miss-q
      dut.io.control.missQueueCores(1).poke(2.U)
      dut.io.control.missQueueValidCores(1).poke(true.B)
      // Add a non-critical core in the miss-q too
      dut.io.control.missQueueCores(0).poke(1.U)
      dut.io.control.missQueueValidCores(0).poke(true.B)

      dut.clock.step(1)

      dut.io.control.nonCritWbPop.poke(false.B)
      dut.io.control.nonCritWbEntryIsCrit.poke(false.B)

      // Trigger a replacement event
      performEvictionRequest(dut, coreId = 1, setIdx = workingSet, expectedEvictionCandidate = Some(0))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      // Simulate a WB again, the wb entry is non-critical, thus a wb event should be triggered
      dut.io.control.nonCritWbPop.poke(true.B)
      dut.io.control.nonCritWbEntryIsCrit.poke(false.B)

      dut.clock.step(1)

      dut.io.control.nonCritWbPop.poke(false.B)
      dut.io.control.nonCritWbEntryIsCrit.poke(false.B)

      // Make a critical core evict a new line
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(1))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 1)

      // Make a critical core evict a new line
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(2))
      dut.clock.step()
      performUpdateRequest(dut, coreId = 2, setIdx = workingSet, hitWay = 2)

      // Make plru point to the first critical line
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 3)
      performUpdateRequest(dut, coreId = 1, setIdx = workingSet, hitWay = 0)

      // Now try to evict the critical line using a non-critical core, should evict non-critical line instead
      performEvictionRequest(dut, coreId = 2, setIdx = workingSet, expectedEvictionCandidate = Some(0))

      dut.clock.step()
    }
  }
}
