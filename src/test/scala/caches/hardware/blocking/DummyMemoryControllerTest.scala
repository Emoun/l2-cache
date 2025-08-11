package caches.hardware.blocking

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DummyMemoryControllerTest extends AnyFlatSpec with ChiselScalatestTester {
  "DummyMemoryController" should "handle 16-byte cache requests with 4-byte memory bursts (read and write)" in {
    test(new DummyMemoryController(addressWidth = 8, blockSize = 16, burstSize = 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.cache.req.poke(false.B)
      dut.io.cache.reqId.poke(0.U)
      dut.io.cache.rw.poke(false.B)
      dut.io.cache.wData.poke(0.U)
      dut.io.cache.wMask.poke(0.U)
      dut.io.mem.ack.poke(false.B)
      dut.io.mem.respStatus.poke(0.U)
      dut.io.mem.rData.poke(0.U)
      dut.clock.step(1)

      // --- Test Write ---
      val writeData = BigInt("cafebabedeadbeef1122334455667788", 16)
      dut.io.cache.req.poke(true.B)
      dut.io.cache.addr.poke(0x2A.U)
      dut.io.cache.rw.poke(true.B)
      dut.io.cache.wData.poke(writeData.U)
      dut.clock.step(1)
      dut.io.cache.req.poke(false.B)
      dut.io.cache.rw.poke(false.B)

      // Simulate memory accepting 4 bursts
      for (i <- 0 until 4) {
        dut.clock.step(1)
      }

      dut.io.mem.ack.poke(true.B)

      dut.clock.step(1)

      dut.io.mem.ack.poke(false.B)

      // Controller should go to done and ack cache
      dut.io.cache.ack.expect(true.B)
      dut.io.cache.responseStatus.expect(0.U)

      dut.clock.step(1)

      // --- Test Read ---
      dut.io.cache.req.poke(true.B)
      dut.io.cache.addr.poke(0x2A.U)
      dut.io.cache.rw.poke(false.B)

      dut.clock.step(1)

      dut.io.cache.req.poke(false.B)

      dut.io.mem.req.expect(true.B)
      dut.io.mem.rw.expect(false.B)

      // Simulate memory returning 4 bursts (simulate 4 bytes each)
      val burstVals = Seq(
        BigInt("55667788", 16),
        BigInt("11223344", 16),
        BigInt("deadbeef", 16),
        BigInt("cafebabe", 16)
      )

      for (i <- 0 until 4) {
        dut.clock.step(1)

        dut.io.mem.rData.poke(burstVals(i).U)
      }

      dut.io.mem.ack.poke(true.B)

      // Controller should go to done and ack cache
      dut.clock.step(1)

      dut.io.cache.ack.expect(true.B)

      // The output should be the concatenation of the bursts
      val expectedRead = (burstVals.reverse.foldLeft(BigInt(0))((acc, v) => (acc << 32) | v))
      dut.io.cache.rData.expect(expectedRead.U)
      dut.io.cache.responseStatus.expect(0.U)
    }
  }
}
