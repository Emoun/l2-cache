package caches.hardware.util

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TwoReadMemBlockTest extends AnyFlatSpec with ChiselScalatestTester {
  "TwoReadMemBlock" should "work" in {
    test(new TwoReadMemBlock(depth = 128, width = 16, forward = true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.stall.poke(false.B)
      dut.io.readAddr1.poke(0.U)
      dut.io.readAddr2.poke(0.U)
      dut.io.writeData.poke(0.U)
      dut.io.wrEn.poke(false.B)

      dut.clock.step(1)

      dut.io.wrEn.poke(true.B)
      dut.io.writeAddr.poke(12.U)
      dut.io.writeData.poke(0xbeef.U)

      dut.clock.step(1)

      dut.io.wrEn.poke(true.B)
      dut.io.writeAddr.poke(54.U)
      dut.io.writeData.poke(0xdead.U)

      dut.clock.step(1)

      dut.io.wrEn.poke(false.B)
      dut.io.writeAddr.poke(0.U)
      dut.io.writeData.poke(0.U)

      // Test reading two different addresses
      dut.io.readAddr1.poke(12.U)
      dut.io.readAddr2.poke(54.U)

      dut.clock.step(1)

      var rdData1 = dut.io.readData1.peekInt()
      var rdData2 = dut.io.readData2.peekInt()

      assert(rdData1 === 0xbeef)
      assert(rdData2 === 0xdead)

      // Switch the read addresses
      dut.io.readAddr1.poke(54.U)
      dut.io.readAddr2.poke(12.U)

      dut.clock.step(1)

      rdData1 = dut.io.readData1.peekInt()
      rdData2 = dut.io.readData2.peekInt()

      assert(rdData1 === 0xdead)
      assert(rdData2 === 0xbeef)

      // Test same address
      dut.io.readAddr1.poke(12.U)
      dut.io.readAddr2.poke(12.U)

      dut.clock.step(1)

      rdData1 = dut.io.readData1.peekInt()
      rdData2 = dut.io.readData2.peekInt()

      assert(rdData1 === 0xbeef)
      assert(rdData2 === 0xbeef)

      // Test forwarding
      dut.io.readAddr1.poke(57.U)
      dut.io.readAddr2.poke(12.U)

      dut.io.wrEn.poke(true.B)
      dut.io.writeAddr.poke(57.U)
      dut.io.writeData.poke(0xcafe.U)

      dut.clock.step(1)

      rdData1 = dut.io.readData1.peekInt()
      rdData2 = dut.io.readData2.peekInt()

      assert(rdData1 === 0xcafe)
      assert(rdData2 === 0xbeef)

      // Test forwarding of second address
      dut.io.readAddr1.poke(12.U)
      dut.io.readAddr2.poke(64.U)

      dut.io.wrEn.poke(true.B)
      dut.io.writeAddr.poke(64.U)
      dut.io.writeData.poke(0xbabe.U)

      dut.clock.step(1)

      rdData1 = dut.io.readData1.peekInt()
      rdData2 = dut.io.readData2.peekInt()

      assert(rdData1 === 0xbeef)
      assert(rdData2 === 0xbabe)

      // Test forwarding of both addresses
      dut.io.readAddr1.poke(72.U)
      dut.io.readAddr2.poke(72.U)

      dut.io.wrEn.poke(true.B)
      dut.io.writeAddr.poke(72.U)
      dut.io.writeData.poke(0xf00d.U)

      dut.clock.step(1)

      rdData1 = dut.io.readData1.peekInt()
      rdData2 = dut.io.readData2.peekInt()

      assert(rdData1 === 0xf00d)
      assert(rdData2 === 0xf00d)

      dut.clock.step(1)
    }
  }
}
