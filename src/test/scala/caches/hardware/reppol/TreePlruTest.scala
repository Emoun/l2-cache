package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TreePlruTest extends AnyFlatSpec with ChiselScalatestTester {
  "TreePlru" should "keep track of LRU way" in {
    test(new TreePlru(4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.update.valid.poke(false.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)

      // Update LRU on cache hit to way one
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the third way
      dut.io.replaceWay.expect(2.U)

      // Update LRU on cache hit to way three
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(2.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.replaceWay.expect(1.U)

      // Update LRU on cache hit to way two
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(1.U)

      dut.clock.step(1)

      // Expect to point to the fourth way
      dut.io.replaceWay.expect(3.U)

      // Update LRU on cache hit to way four
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(3.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.replaceWay.expect(0.U)

      // Update LRU on cache hit to way one
      dut.io.update.valid.poke(true.B)
      dut.io.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the third way
      dut.io.replaceWay.expect(2.U)
    }
  }
}
