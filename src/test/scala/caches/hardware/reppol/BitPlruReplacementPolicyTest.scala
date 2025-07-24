package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// TODO: Finish tests
class BitPlruReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  def assertPlruSet(dut: BitPlruReplacementPolicy, expectedSet: Array[Int], printActual: Boolean = false): Unit = {
//    if (printActual) println("")
//
//    val idxs = dut.io.replacementSet
//    for (i <- 0 until idxs.length) {
//      if (printActual) println(s"Way $i: ${idxs(i).peekInt()}")
//
//      val actualWay = idxs(i).peekInt()
//      val expectedWay = expectedSet(i)
//      assert(actualWay === expectedWay)
//    }
//
//    if (printActual) println("")
  }

  "BitPlru" should "keep track of LRU way for 4 ways" in {
    val (nWays, nSets, nCores) = (4, 2, 1)
    test(new BitPlruReplacementPolicy(nWays = nWays, nSets = nSets, nCores = nCores)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      cancel("Need to update the test")
      // Default assignments
//      dut.io.update.valid.poke(false.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the first way
//      dut.io.replaceWay.expect(0.U)
//      assertPlruSet(dut, expectedSet = Array(0, 1, 2, 3))
//
//      // Update LRU state by accessing first way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the second way
//      dut.io.replaceWay.expect(1.U)
//      assertPlruSet(dut, expectedSet = Array(1, 2, 3, 0))
//
//      // Update LRU state by accessing third way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(2.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the second way
//      dut.io.replaceWay.expect(1.U)
//      assertPlruSet(dut, expectedSet = Array(1, 3, 0, 2))
//
//      // Update LRU state by accessing second way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(1.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the fourth way
//      dut.io.replaceWay.expect(3.U)
//      assertPlruSet(dut, expectedSet = Array(3, 0, 1, 2))
//
//      // Update LRU state by accessing fourth way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(3.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the first way
//      dut.io.replaceWay.expect(0.U)
//      assertPlruSet(dut, expectedSet = Array(0, 1, 2, 3))
//
//      dut.clock.step(1)
    }
  }

  "BitPlru" should "keep track LRU way for 2 ways only" in {
    val (nWays, nSets, nCores) = (2, 2, 1)
    test(new BitPlruReplacementPolicy(nWays = nWays, nSets = nSets, nCores = nCores)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      cancel("Need to update the test")
      // Default assignments
//      dut.io.update.valid.poke(false.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the first way
//      dut.io.replaceWay.expect(0.U)
//
//      // Update LRU state by accessing first way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the second way
//      dut.io.replaceWay.expect(1.U)
//
//      // Update LRU state by accessing the second way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(1.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the first way
//      dut.io.replaceWay.expect(0.U)
//
//      // Update LRU state by accessing second way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(1.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the first way
//      dut.io.replaceWay.expect(0.U)
//
//      // Update LRU state by accessing first way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the second way
//      dut.io.replaceWay.expect(1.U)
//
//      // Update LRU state by accessing first way
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      // Expect to point to the second way
//      dut.io.replaceWay.expect(1.U)
//
//      dut.clock.step(1)
    }
  }

  "BitPlru" should "keep track of LRU way for 8 ways" in {
    val (nWays, nSets, nCores) = (8, 2, 1)
    test(new BitPlruReplacementPolicy(nWays = nWays, nSets = nSets, nCores = nCores)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      cancel("Need to update the test")
      // Default assignments
//      dut.io.update.valid.poke(false.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(0.U)
//      assertPlruSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 6, 7))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(0.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(1.U)
//      assertPlruSet(dut, expectedSet = Array(1, 2, 3, 4, 5, 6, 7, 0))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(2.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(1.U)
//      assertPlruSet(dut, expectedSet = Array(1, 3, 4, 5, 6, 7, 0, 2))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(1.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(3.U)
//      assertPlruSet(dut, expectedSet = Array(3, 4, 5, 6, 7, 0, 1, 2))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(3.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(4.U)
//      assertPlruSet(dut, expectedSet = Array(4, 5, 6, 7, 0, 1, 2, 3))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(5.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(4.U)
//      assertPlruSet(dut, expectedSet = Array(4, 6, 7, 0, 1, 2, 3, 5))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(5.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(4.U)
//      assertPlruSet(dut, expectedSet = Array(4, 6, 7, 0, 1, 2, 3, 5))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(7.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(4.U)
//      assertPlruSet(dut, expectedSet = Array(4, 6, 0, 1, 2, 3, 5, 7))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(4.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(6.U)
//      assertPlruSet(dut, expectedSet = Array(6, 0, 1, 2, 3, 4, 5, 7))
//
//      dut.io.update.valid.poke(true.B)
//      dut.io.update.bits.poke(6.U)
//
//      dut.clock.step(1)
//
//      dut.io.replaceWay.expect(0.U)
//      assertPlruSet(dut, expectedSet = Array(0, 1, 2, 3, 4, 5, 7, 6))
//
//      dut.io.update.valid.poke(false.B)
//
//      dut.clock.step(1)
    }
  }
}
