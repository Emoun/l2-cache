package caches.hardware.pipelined.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WriteBackFifoTest extends AnyFlatSpec with ChiselScalatestTester {
  "WriteBackFifo" should "push and pop entries correctly" in {
    test(new WriteBackFifo(queueDepth = 2, tagWidth = 16, indexWidth = 8, blockWidth = 32)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      dut.io.push.poke(false.B)
      dut.io.pop.poke(false.B)
      dut.io.pushEntry.tag.poke(0.U)
      dut.io.pushEntry.index.poke(0.U)
      dut.io.pushEntry.wbData.poke(0.U)

      // Push an entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.tag.poke("h1234".U)
      dut.io.pushEntry.index.poke("h12".U)
      dut.io.pushEntry.wbData.poke("hdeadbeef".U)

      dut.io.full.expect(false.B)
      dut.io.empty.expect(true.B)

      dut.clock.step(1)

      dut.io.push.poke(false.B)

      // Pop an entry
      dut.io.pop.poke(true.B)
      dut.io.popEntry.tag.expect("h1234".U)
      dut.io.popEntry.index.expect("h12".U)
      dut.io.popEntry.wbData.expect("hdeadbeef".U)

      dut.clock.step(1)

      dut.io.pop.poke(false.B)
      dut.io.empty.expect(true.B)

      // Push an entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.tag.poke("h4321".U)
      dut.io.pushEntry.index.poke("h17".U)
      dut.io.pushEntry.wbData.poke("hcafebabe".U)

      dut.clock.step(1)

      // Push another entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.tag.poke("h5678".U)
      dut.io.pushEntry.index.poke("h21".U)
      dut.io.pushEntry.wbData.poke("hbabecafe".U)

      dut.clock.step(1)

      dut.io.push.poke(false.B)
      dut.io.full.expect(true.B)

      dut.io.pop.poke(true.B)
      dut.io.popEntry.tag.expect("h4321".U)
      dut.io.popEntry.index.expect("h17".U)
      dut.io.popEntry.wbData.expect("hcafebabe".U)

      dut.clock.step(1)

      dut.io.pop.poke(true.B)
      dut.io.popEntry.tag.expect("h5678".U)
      dut.io.popEntry.index.expect("h21".U)
      dut.io.popEntry.wbData.expect("hbabecafe".U)

      dut.clock.step(1)

      dut.io.pop.poke(false.B)
      dut.io.empty.expect(true.B)

      dut.clock.step(1)
    }
  }
}
