package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BitPlruTest extends AnyFlatSpec with ChiselScalatestTester {
  def assertPlruSet(dut: BitPlru, expectedSet: Array[Int], printActual: Boolean = false): Unit = {
    if (printActual) println("")

    val idxs = dut.io.replacementSet
    for (i <- 0 until idxs.length) {
      val actualWay = idxs(i).peekInt()
      val expectedWay = expectedSet(i)
      assert(actualWay === expectedWay)

      if (printActual) println(s"Way $i: ${idxs(i).peekInt()}")
    }

    if (printActual) println("")
  }

  "BitPlru" should "keep track of LRU way" in {
    test(new BitPlru(4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.update.valid.poke(false.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)
      assertPlruSet(dut, expectedSet = Array(0, 1, 2, 3))

      // Update LRU state by accessing first way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.replaceWay.expect(1.U)
      assertPlruSet(dut, expectedSet = Array(1, 2, 3, 0))

      // Update LRU state by accessing third way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(2.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.replaceWay.expect(1.U)
      assertPlruSet(dut, expectedSet = Array(1, 3, 0, 2))

      // Update LRU state by accessing second way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(1.U)

      dut.clock.step(1)

      // Expect to point to the fourth way
      dut.io.replaceWay.expect(3.U)
      assertPlruSet(dut, expectedSet = Array(3, 0, 1, 2))

      // Update LRU state by accessing fourth way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(3.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)
      assertPlruSet(dut, expectedSet = Array(0, 1, 2, 3))

      dut.clock.step(1)
    }
  }

  "BitPlru" should "keep track LRU way for two ways only" in {
    test(new BitPlru(2)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.update.valid.poke(false.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)

      // Update LRU state by accessing first way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.replaceWay.expect(1.U)

      // Update LRU state by accessing the second way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(1.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)

      // Update LRU state by accessing second way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(1.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)

      // Update LRU state by accessing first way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.replaceWay.expect(1.U)

      // Update LRU state by accessing first way
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.replaceWay.expect(1.U)

      dut.clock.step(1)
    }
  }
}
