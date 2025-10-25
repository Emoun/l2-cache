package caches.hardware.reppol

import caches.hardware.reppol.ReplacementPolicyTest._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BitPlruReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  "BitPlruReplacementPolicy" should "keep track LRU way for 2 ways" in {
    val (nWays, nSets, nCores) = (2, 2, 1)
    test(new PolicyTestWrapper(() => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

  "BitPlruReplacementPolicy" should "keep track of LRU way for 4 ways" in {
    val (nWays, nSets, nCores) = (4, 2, 1)
    test(new PolicyTestWrapper(() => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val workingSet = 1
      dut.io.policy.control.setIdx.poke(workingSet.U)

      dut.clock.step()

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3))

      // Update LRU state by accessing first way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 0)

      // Expect to point to the second way
      dut.io.policy.control.replaceWay.expect(1.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 0))

      // Update LRU state by accessing third way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 2)

      // Expect to point to the second way
      dut.io.policy.control.replaceWay.expect(1.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(1, 3, 0, 2))

      // Update LRU state by accessing second way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 1)

      // Expect to point to the fourth way
      dut.io.policy.control.replaceWay.expect(3.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(3, 0, 1, 2))

      // Update LRU state by accessing fourth way
      performUpdateRequest(dut, coreId = 0, setIdx = workingSet, hitWay = 3)

      // Expect to point to the first way
      dut.io.policy.control.replaceWay.expect(0.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3))

      dut.clock.step(1)
    }
  }

  "BitPlruReplacementPolicy" should "keep track of LRU way for 8 ways" in {
    val (nWays, nSets, nCores) = (8, 2, 1)
    test(new PolicyTestWrapper(() => new BitPlruReplacementPolicy(nWays, nSets, nCores))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.policy.control.setIdx.poke(0.U)

      dut.clock.step()

      dut.io.policy.control.replaceWay.expect(0.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 6, 7))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(0.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(1.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 6, 7, 0))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(2.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(1.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(1, 3, 4, 5, 6, 7, 0, 2))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(1.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(3.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(3, 4, 5, 6, 7, 0, 1, 2))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(3.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(4.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(4, 5, 6, 7, 0, 1, 2, 3))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(5.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(4.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(4, 6, 7, 0, 1, 2, 3, 5))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(5.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(4.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(4, 6, 7, 0, 1, 2, 3, 5))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(7.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(4.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(4, 6, 0, 1, 2, 3, 5, 7))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(4.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(6.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(6, 0, 1, 2, 3, 4, 5, 7))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(6.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(0.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 7, 6))

      dut.io.policy.control.update.valid.poke(false.B)
      dut.io.policy.control.update.bits.poke(0.U)

      dut.io.policy.control.setIdx.poke(1.U)
      //-------------
      dut.clock.step(1)

      dut.io.policy.control.setIdx.poke(0.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(0.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 6, 7))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(0.U)

      dut.io.policy.control.setIdx.poke(1.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(0.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 7, 6))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(0.U)

      dut.io.policy.control.setIdx.poke(0.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(1.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 6, 7, 0))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(1.U)

      dut.clock.step(1)

      dut.io.policy.control.replaceWay.expect(1.U)
      assertNumericalReplacementSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 7, 0, 6))

      dut.io.policy.control.update.valid.poke(true.B)
      dut.io.policy.control.update.bits.poke(0.U)

      dut.clock.step(1)
    }
  }
}
