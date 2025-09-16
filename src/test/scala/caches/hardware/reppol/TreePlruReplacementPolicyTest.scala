package caches.hardware.reppol

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import caches.hardware.reppol.ReplacementPolicyTest._

class TreePlruReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  "TreePlru" should "keep track of LRU way for 4 ways with pipelined updates" in {
    val (nWays, nSets, nCores) = (4, 2, 1)
    test(new TreePlruReplacementPolicy(nWays = nWays, nSets = nSets, nCores = nCores)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      defaultAssignments(dut)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.control.replaceWay.expect(0.U)

      // Update LRU on cache hit to way one
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.control.replaceWay.expect(0.U)

      // Update LRU on cache hit to way three
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(2.U)

      dut.clock.step(1)

      // Expect to point to the third way
      dut.io.control.replaceWay.expect(2.U)

      // Update LRU on cache hit to way two
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(1.U)

      dut.clock.step(1)

      // Expect to point to the second way
      dut.io.control.replaceWay.expect(1.U)

      // Update LRU on cache hit to way four
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(3.U)

      dut.clock.step(1)

      // Expect to point to the fourth way
      dut.io.control.replaceWay.expect(3.U)

      // Update LRU on cache hit to way one
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(0.U)

      dut.clock.step(1)

      // Expect to point to the first way
      dut.io.control.replaceWay.expect(0.U)

      // Update LRU on cache hit to way two
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(1.U)

      dut.clock.step(1)

      // Expect to point to the third way still
      dut.io.control.replaceWay.expect(2.U)

      // Update LRU on cache hit to way three
      dut.io.control.update.valid.poke(true.B)
      dut.io.control.update.bits.poke(2.U)

      dut.clock.step(1)

      // Expect to point to the first way now, since we had accessed way two recently
      dut.io.control.replaceWay.expect(2.U)

      dut.clock.step(1)
    }
  }
}
