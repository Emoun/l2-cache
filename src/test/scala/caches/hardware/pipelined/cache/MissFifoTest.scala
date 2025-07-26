package caches.hardware.pipelined.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MissFifoTest extends AnyFlatSpec with ChiselScalatestTester {
  def assertMshrStatus(dut: MissFifo, expectedTags: Array[String], expectedIndexes: Array[String], expectedValidMSHRs: Array[Boolean]): Unit = {
    val currentWays = dut.io.push.currentTags
    val currentIndexes = dut.io.push.currentIndexes
    val validMSHRs = dut.io.push.validMSHRs

    for (i <- expectedTags.indices) {
        currentWays(i).expect(expectedTags(i).U)
        currentIndexes(i).expect(expectedIndexes(i).U)
        validMSHRs(i).expect(expectedValidMSHRs(i).B)
    }
  }

  "MissFifo" should "push and pop entries correctly" in {
    test(new MissFifo(
      nCores = 4,
      nMSHRs = 4,
      nWays = 16,
      reqIdWidth = 2,
      tagWidth = 8,
      indexWidth = 4,
      blockOffsetWidth = 4,
      subBlockWidth = 8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      dut.io.push.push.poke(false.B)
      dut.io.pop.pop.poke(false.B)
      dut.io.push.pushEntry.rw.poke(0.U)
      dut.io.push.pushEntry.reqId.poke(0.U)
      dut.io.push.pushEntry.wData.poke(0.U)
      dut.io.push.pushEntry.replaceWay.poke(0.U)
      dut.io.push.pushEntry.tag.poke(0.U)
      dut.io.push.pushEntry.index.poke(0.U)
      dut.io.push.pushEntry.blockOffset.poke(0.U)

      // Push an entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.rw.poke(0.U)
      dut.io.push.pushEntry.reqId.poke(1.U)
      dut.io.push.pushEntry.wData.poke("hff".U)
      dut.io.push.pushEntry.replaceWay.poke(2.U)
      dut.io.push.pushEntry.tag.poke("h12".U)
      dut.io.push.pushEntry.index.poke("h2".U)
      dut.io.push.pushEntry.blockOffset.poke("h3".U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(true.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.rw.poke(1.U)
      dut.io.push.pushEntry.reqId.poke(2.U)
      dut.io.push.pushEntry.wData.poke("hf6".U)
      dut.io.push.pushEntry.replaceWay.poke(3.U)
      dut.io.push.pushEntry.tag.poke("h44".U)
      dut.io.push.pushEntry.index.poke("h1".U)
      dut.io.push.pushEntry.blockOffset.poke("h6".U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.rw.poke(0.U)
      dut.io.push.pushEntry.reqId.poke(3.U)
      dut.io.push.pushEntry.wData.poke("h15".U)
      dut.io.push.pushEntry.replaceWay.poke(1.U)
      dut.io.push.pushEntry.tag.poke("haa".U)
      dut.io.push.pushEntry.index.poke("hc".U)
      dut.io.push.pushEntry.blockOffset.poke("he".U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.push.poke(true.B)
      dut.io.push.pushEntry.rw.poke(0.U)
      dut.io.push.pushEntry.reqId.poke(0.U)
      dut.io.push.pushEntry.wData.poke("h09".U)
      dut.io.push.pushEntry.replaceWay.poke(1.U)
      dut.io.push.pushEntry.tag.poke("hbb".U)
      dut.io.push.pushEntry.index.poke("he".U)
      dut.io.push.pushEntry.blockOffset.poke("h1".U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      dut.clock.step(1)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.push.push.poke(false.B)
      dut.io.pop.popEntry.rw.expect(0.U)
      dut.io.pop.popEntry.reqId.expect(1.U)
      dut.io.pop.popEntry.wData.expect("hff".U)
      dut.io.pop.popEntry.replaceWay.expect(2.U)
      dut.io.pop.popEntry.tag.expect("h12".U)
      dut.io.pop.popEntry.index.expect("h2".U)
      dut.io.pop.popEntry.blockOffset.expect("h3".U)

      dut.io.push.full.expect(true.B)
      dut.io.pop.empty.expect(false.B)

      assertMshrStatus(
        dut,
        expectedTags = Array("h12", "h44", "haa", "hbb"),
        expectedIndexes = Array("h2", "h1", "hc", "he"),
        expectedValidMSHRs = Array(true, true, true, true)
      )

      dut.clock.step(1)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.rw.expect(1.U)
      dut.io.pop.popEntry.reqId.expect(2.U)
      dut.io.pop.popEntry.wData.expect("hf6".U)
      dut.io.pop.popEntry.replaceWay.expect(3.U)
      dut.io.pop.popEntry.tag.expect("h44".U)
      dut.io.pop.popEntry.index.expect("h1".U)
      dut.io.pop.popEntry.blockOffset.expect("h6".U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      assertMshrStatus(
        dut,
        expectedTags = Array("h12", "h44", "haa", "hbb"),
        expectedIndexes = Array("h2", "h1", "hc", "he"),
        expectedValidMSHRs = Array(false, true, true, true)
      )

      dut.clock.step(1)
    }
  }
}
