package caches.hardware.pipelined.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MissFifoTest extends AnyFlatSpec with ChiselScalatestTester {
  def assertMshrStatus(dut: MissFifo, expectedWays: Array[String], expectedIndexes: Array[String], expectedValidMSHRs: Array[Boolean]): Unit = {
    val currentWays = dut.io.currentWays
    val currentIndexes = dut.io.currentIndexes
    val validMSHRs = dut.io.validMSHRs

    for (i <- expectedWays.indices) {
        currentWays(i).expect(expectedWays(i).U)
        currentIndexes(i).expect(expectedIndexes(i).U)
        validMSHRs(i).expect(expectedValidMSHRs(i).B)
    }
  }

  "MissFifo" should "push and pop entries correctly" in {
    test(new MissFifo(
      nMSHRs = 4,
      nWays = 16,
      reqIdWidth = 2,
      tagWidth = 8,
      indexWidth = 4,
      blockOffsetWidth = 4,
      subBlockWidth = 8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      dut.io.push.poke(false.B)
      dut.io.pop.poke(false.B)
      dut.io.pushEntry.rw.poke(0.U)
      dut.io.pushEntry.reqId.poke(0.U)
      dut.io.pushEntry.wData.poke(0.U)
      dut.io.pushEntry.replaceWay.poke(0.U)
      dut.io.pushEntry.tag.poke(0.U)
      dut.io.pushEntry.index.poke(0.U)
      dut.io.pushEntry.blockOffset.poke(0.U)

      // Push an entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.rw.poke(0.U)
      dut.io.pushEntry.reqId.poke(1.U)
      dut.io.pushEntry.wData.poke("hff".U)
      dut.io.pushEntry.replaceWay.poke(2.U)
      dut.io.pushEntry.tag.poke("h12".U)
      dut.io.pushEntry.index.poke("h2".U)
      dut.io.pushEntry.blockOffset.poke("h3".U)

      dut.io.full.expect(false.B)
      dut.io.empty.expect(true.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.rw.poke(1.U)
      dut.io.pushEntry.reqId.poke(2.U)
      dut.io.pushEntry.wData.poke("hf6".U)
      dut.io.pushEntry.replaceWay.poke(3.U)
      dut.io.pushEntry.tag.poke("h44".U)
      dut.io.pushEntry.index.poke("h1".U)
      dut.io.pushEntry.blockOffset.poke("h6".U)

      dut.io.full.expect(false.B)
      dut.io.empty.expect(false.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.rw.poke(0.U)
      dut.io.pushEntry.reqId.poke(3.U)
      dut.io.pushEntry.wData.poke("h15".U)
      dut.io.pushEntry.replaceWay.poke(1.U)
      dut.io.pushEntry.tag.poke("haa".U)
      dut.io.pushEntry.index.poke("hc".U)
      dut.io.pushEntry.blockOffset.poke("he".U)

      dut.io.full.expect(false.B)
      dut.io.empty.expect(false.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.poke(true.B)
      dut.io.pushEntry.rw.poke(0.U)
      dut.io.pushEntry.reqId.poke(0.U)
      dut.io.pushEntry.wData.poke("h09".U)
      dut.io.pushEntry.replaceWay.poke(1.U)
      dut.io.pushEntry.tag.poke("hbb".U)
      dut.io.pushEntry.index.poke("he".U)
      dut.io.pushEntry.blockOffset.poke("h1".U)

      dut.io.full.expect(false.B)
      dut.io.empty.expect(false.B)

      dut.clock.step(1)

      // Pop an entry
      dut.io.pop.poke(true.B)
      dut.io.push.poke(false.B)
      dut.io.popEntry.rw.expect(0.U)
      dut.io.popEntry.reqId.expect(1.U)
      dut.io.popEntry.wData.expect("hff".U)
      dut.io.popEntry.replaceWay.expect(2.U)
      dut.io.popEntry.tag.expect("h12".U)
      dut.io.popEntry.index.expect("h2".U)
      dut.io.popEntry.blockOffset.expect("h3".U)

      dut.io.full.expect(true.B)
      dut.io.empty.expect(false.B)

      assertMshrStatus(
        dut,
        expectedWays = Array("h2", "h3", "h1", "h1"),
        expectedIndexes = Array("h2", "h1", "hc", "he"),
        expectedValidMSHRs = Array(true, true, true, true)
      )

      dut.clock.step(1)

      // Pop an entry
      dut.io.pop.poke(true.B)
      dut.io.popEntry.rw.expect(1.U)
      dut.io.popEntry.reqId.expect(2.U)
      dut.io.popEntry.wData.expect("hf6".U)
      dut.io.popEntry.replaceWay.expect(3.U)
      dut.io.popEntry.tag.expect("h44".U)
      dut.io.popEntry.index.expect("h1".U)
      dut.io.popEntry.blockOffset.expect("h6".U)

      dut.io.full.expect(false.B)
      dut.io.empty.expect(false.B)

      assertMshrStatus(
        dut,
        expectedWays = Array("h2", "h3", "h1", "h1"),
        expectedIndexes = Array("h2", "h1", "hc", "he"),
        expectedValidMSHRs = Array(false, true, true, true)
      )

      dut.clock.step(1)
    }
  }
}
