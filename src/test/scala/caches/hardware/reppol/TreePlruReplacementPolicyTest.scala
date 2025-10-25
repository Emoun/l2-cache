package caches.hardware.reppol

import caches.hardware.reppol.ReplacementPolicyTest.performUpdateRequest
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TreePlruReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  "TreePlruReplacementPolicy" should "keep track of LRU way for 2 ways" in {
    val (nWays, nSets, nCores) = (2, 2, 1)
    test(new PolicyTestWrapper(() => new TreePlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 0
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      // Expect to point to the second way
      dut.io.policy.control.replaceWay.expect(1.U)

      // Update LRU state by accessing the second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)

      // Update LRU state by accessing second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      // Expect to point to the second way
      dut.io.policy.control.replaceWay.expect(1.U)

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      // Expect to point to the second way
      dut.io.policy.control.replaceWay.expect(1.U)

      dut.clock.step()
    }
  }

  "TreePlruReplacementPolicy" should "keep track of LRU way for 4 ways" in {
    val (nWays, nSets, nCores) = (4, 2, 1)
    test(new PolicyTestWrapper(() => new TreePlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)

      // Update LRU on cache hit to way one
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      // Expect to point to the third way
      dut.io.policy.control.replaceWay.expect(2.U)

      // Update LRU on cache hit to way three
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      // Expect to point to the second way
      dut.io.policy.control.replaceWay.expect(1.U)

      // Update LRU on cache hit to way two
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      // Expect to point to the fourth way
      dut.io.policy.control.replaceWay.expect(3.U)

      // Update LRU on cache hit to way four
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)

      // Update LRU on cache hit to way two
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      // Expect to point to the first third way
      dut.io.policy.control.replaceWay.expect(2.U)

      // Update LRU on cache hit to way two
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      // Expect to point to the third way again
      dut.io.policy.control.replaceWay.expect(2.U)

      // Update LRU on cache hit to way three
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      // Expect to point to the first way now, since we had accessed way three recently
      dut.io.policy.control.replaceWay.expect(0.U)

      dut.clock.step()
    }
  }

  "TreePlruReplacementPolicy" should "keep track of LRU way for 8 ways" in {
    val (nWays, nSets, nCores) = (8, 2, 1)
    test(new PolicyTestWrapper(() => new TreePlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var workingSet = 0
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      dut.io.policy.control.replaceWay.expect(4.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 4)

      dut.io.policy.control.replaceWay.expect(2.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      dut.io.policy.control.replaceWay.expect(6.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 6)

      dut.io.policy.control.replaceWay.expect(1.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      dut.io.policy.control.replaceWay.expect(5.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 5)

      dut.io.policy.control.replaceWay.expect(3.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      dut.io.policy.control.replaceWay.expect(7.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 7)

      dut.io.policy.control.replaceWay.expect(0.U)

      workingSet = 1
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step(5)

      dut.io.policy.control.replaceWay.expect(0.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      dut.io.policy.control.replaceWay.expect(4.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 4)

      workingSet = 0
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step(5)

      dut.io.policy.control.replaceWay.expect(0.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      dut.io.policy.control.replaceWay.expect(4.U)
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 4)

      dut.clock.step()
    }
  }
}
