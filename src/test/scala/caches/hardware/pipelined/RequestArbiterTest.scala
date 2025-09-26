package caches.hardware.pipelined

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RequestArbiterTest extends AnyFlatSpec with ChiselScalatestTester {
  "RequestArbiter" should "arbitrate between multiple requests" in {
    test(new RoundRobinRequestArbiter(nReqs = 4, addrWidth = 16, dataWidth = 32, reqIdWidth = 8)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
      // Initialize inputs
      for (i <- 0 until 4) {
        dut.io.ports(i).reqId.valid.poke(false.B)
        dut.io.ports(i).reqId.bits.poke(0.U)
        dut.io.ports(i).addr.poke(0.U)
        dut.io.ports(i).rw.poke(true.B)
        dut.io.ports(i).wData.poke(0.U)
        dut.io.out.reqId.ready.poke(true.B)
      }

      // Send requests separately
      for (i <- 0 until 4) {
        dut.io.ports(i).reqId.valid.poke(true.B)
        dut.io.ports(i).reqId.bits.poke((i * 4).U)
        dut.io.ports(i).addr.poke((i * 6).U)
        dut.io.ports(i).rw.poke((i % 2 == 0).B)
        dut.io.ports(i).wData.poke(i.U)

        dut.io.out.reqId.valid.expect(true.B)
        dut.io.out.reqId.bits.expect((i * 4).U)
        dut.io.out.addr.expect((i * 6).U)
        dut.io.out.rw.expect((i % 2 == 0).B)
        dut.io.out.wData.expect(i.U)
        dut.io.chosen.expect(i.U)

        dut.clock.step(1)

        dut.io.ports(i).reqId.valid.poke(false.B)
        dut.io.ports(i).reqId.bits.poke(0.U)
        dut.io.ports(i).rw.poke(false.B)
        dut.io.ports(i).wData.poke(0.U)

        dut.clock.step(1)
      }

      dut.clock.step(1)

      // Test if arbiter does not consume a request if consumer is not ready

      dut.io.out.reqId.ready.poke(false.B)

      // Place a request from the first core
      dut.io.ports(0).reqId.valid.poke(true.B)
      dut.io.ports(0).reqId.bits.poke(0.U)
      dut.io.ports(0).addr.poke(17.U)
      dut.io.ports(0).rw.poke(false.B)
      dut.io.ports(0).wData.poke(10.U)

      // Expect the port to not receive a ready signal from the arbiter
      dut.io.ports(0).reqId.ready.expect(false.B)

      dut.clock.step(1)

      dut.io.out.reqId.ready.poke(true.B)

      // Place another requests at the same time, this time from core 2
      dut.io.ports(1).reqId.valid.poke(true.B)
      dut.io.ports(1).reqId.bits.poke(1.U)
      dut.io.ports(1).addr.poke(24.U)
      dut.io.ports(1).rw.poke(true.B)
      dut.io.ports(1).wData.poke(5.U)

      // Expect the arbiter to grant access to port 1 first
      dut.io.out.reqId.valid.expect(true.B)
      dut.io.out.reqId.bits.expect(0.U)
      dut.io.out.addr.expect(17.U)
      dut.io.out.rw.expect(false.B)
      dut.io.out.wData.expect(10.U)

      dut.clock.step(1)

      dut.io.out.reqId.ready.poke(true.B)

      // Place another request from the first core
      dut.io.ports(0).reqId.valid.poke(true.B)
      dut.io.ports(0).reqId.bits.poke(2.U)
      dut.io.ports(0).addr.poke(27.U)
      dut.io.ports(0).rw.poke(true.B)
      dut.io.ports(0).wData.poke(15.U)

      // Expect the arbiter to grant access to port 2 now
      dut.io.out.reqId.valid.expect(true.B)
      dut.io.out.reqId.bits.expect(1.U)
      dut.io.out.addr.expect(24.U)
      dut.io.out.rw.expect(true.B)
      dut.io.out.wData.expect(5.U)

      dut.clock.step(1)
    }
  }
}
