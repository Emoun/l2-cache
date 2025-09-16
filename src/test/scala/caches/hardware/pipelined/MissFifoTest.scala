package caches.hardware.pipelined

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

case class ExpectedCmd(reqId: Int, coreId: Int, blockOffset: Int)
case class ExpectedMshrStatus(tags: Array[String], indexes: Array[String], validMSHRs: Array[Boolean], fullSignals: Array[Boolean])

class MissFifoTest extends AnyFlatSpec with ChiselScalatestTester {
  def assertMshrStatusInNonCritQueue(dut: MissFifo, expectedStatus: ExpectedMshrStatus): Unit = {
    val currentTags = dut.io.nonCritInfo.currentTags
    val currentIndexes = dut.io.nonCritInfo.currentIndexes
    val validMSHRs = dut.io.nonCritInfo.validMSHRs
    val fullSignals = dut.io.nonCritInfo.fullCmds

    for (i <- expectedStatus.indexes.indices) {
        currentTags(i).expect(expectedStatus.tags(i).U)
        currentIndexes(i).expect(expectedStatus.indexes(i).U)
        validMSHRs(i).expect(expectedStatus.validMSHRs(i).B)
        fullSignals(i).expect(expectedStatus.fullSignals(i).B)
    }
  }

  def assertCmds(dut: MissFifo, expectedCmds: Array[Option[ExpectedCmd]], print: Boolean = false): Unit = {
    println("Commands in MissFifo:")
    val cmdSignals = dut.io.pop.cmds

    for (i <- 0 until cmdSignals.length) {
      val expectedCmd = expectedCmds(i)

      if (expectedCmd.isDefined) {
        val expectedCmdVal = expectedCmd.get

        cmdSignals(i).reqId.expect(expectedCmdVal.reqId.U)
        cmdSignals(i).coreId.expect(expectedCmdVal.coreId.U)
        cmdSignals(i).blockOffset.expect(expectedCmdVal.blockOffset.U)
      } else {
        cmdSignals(i).reqId.expect(0.U)
        cmdSignals(i).coreId.expect(0.U)
        cmdSignals(i).blockOffset.expect(0.U)
      }

      println(s"Cmd $i: " +
        s"reqId=${cmdSignals(i).reqId.peekInt()}, " +
        s"coreId=${cmdSignals(i).coreId.peekInt()}, " +
        s"blockOffset=${cmdSignals(i).blockOffset.peekInt()}."
      )
    }

    println("")
  }

  "MissFifo" should "push and pop entries correctly for non critical queue" in {
    val blockWidth = 64
    val subBlockWidth = 16

    test(new MissFifo(
      nCores = 4,
      nCmds = 4,
      nMshrs = 4,
      nWays = 16,
      reqIdWidth = 16,
      tagWidth = 8,
      indexWidth = 4,
      blockOffsetWidth = 2,
      subBlockWidth = subBlockWidth,
      blockWidth = blockWidth
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Initialize inputs
      dut.io.push.pushReq.poke(false.B)
      dut.io.push.withCmd.poke(false.B)
      dut.io.push.pushReqEntry.tag.poke(0.U)
      dut.io.push.pushReqEntry.index.poke(0.U)
      dut.io.push.pushReqEntry.byteEn.poke(0.U) // All bytes disabled
      dut.io.push.pushReqEntry.replaceWay.poke(0.U)
      dut.io.push.pushCmd.poke(false.B)
      dut.io.push.mshrIdx.poke(0.U)
      dut.io.push.pushCmdEntry.reqId.poke(0.U)
      dut.io.push.pushCmdEntry.coreId.poke(0.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)
      dut.io.push.updateByteEn.poke(0.U)
      dut.io.push.updateByteEnRow.poke(0.U)
      dut.io.push.updateByteEnCol.poke(0.U)
      dut.io.push.updateByteEnVal.poke(0.U)
      dut.io.pop.pop.poke(false.B)

      // Push an entry
      dut.io.push.pushReq.poke(true.B)
      dut.io.push.withCmd.poke(true.B)
      dut.io.push.pushReqEntry.tag.poke("h12".U)
      dut.io.push.pushReqEntry.index.poke("h2".U)
      dut.io.push.pushReqEntry.byteEn.poke(3.U)
      dut.io.push.pushReqEntry.replaceWay.poke(2.U)

      dut.io.push.pushCmdEntry.reqId.poke(1.U)
      dut.io.push.pushCmdEntry.coreId.poke(2.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(true.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.pushReq.poke(true.B)
      dut.io.push.withCmd.poke(true.B)
      dut.io.push.pushReqEntry.tag.poke("h44".U)
      dut.io.push.pushReqEntry.index.poke("h1".U)
      dut.io.push.pushReqEntry.byteEn.poke(1.U)
      dut.io.push.pushReqEntry.replaceWay.poke(3.U)

      dut.io.push.pushCmdEntry.reqId.poke(2.U)
      dut.io.push.pushCmdEntry.coreId.poke(2.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.pushReq.poke(true.B)
      dut.io.push.withCmd.poke(false.B)
      dut.io.push.pushReqEntry.tag.poke("haa".U)
      dut.io.push.pushReqEntry.index.poke("hc".U)
      dut.io.push.pushReqEntry.byteEn.poke(2.U)
      dut.io.push.pushReqEntry.replaceWay.poke(2.U)

      dut.io.push.pushCmdEntry.reqId.poke(3.U)
      dut.io.push.pushCmdEntry.coreId.poke(1.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(2.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      dut.clock.step(1)

      // Push an entry
      dut.io.push.pushReq.poke(true.B)
      dut.io.push.withCmd.poke(true.B)
      dut.io.push.pushReqEntry.tag.poke("hbb".U)
      dut.io.push.pushReqEntry.index.poke("he".U)
      dut.io.push.pushReqEntry.byteEn.poke(0.U)
      dut.io.push.pushReqEntry.replaceWay.poke(1.U)

      dut.io.push.pushCmdEntry.reqId.poke(4.U)
      dut.io.push.pushCmdEntry.coreId.poke(3.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(3.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)

      dut.clock.step(1)

      dut.io.push.pushReq.poke(false.B)
      dut.io.push.withCmd.poke(false.B)
      dut.io.push.pushReqEntry.tag.poke(0.U)
      dut.io.push.pushReqEntry.index.poke(0.U)
      dut.io.push.pushReqEntry.byteEn.poke(0.U)
      dut.io.push.pushReqEntry.replaceWay.poke(0.U)

      // Add more commands to the first fifo entry
      dut.io.push.pushCmd.poke(true.B)
      dut.io.push.pushCmdEntry.reqId.poke(11.U)
      dut.io.push.pushCmdEntry.coreId.poke(3.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(3.U)

      dut.clock.step(1)

      dut.io.push.pushCmd.poke(true.B)
      dut.io.push.pushCmdEntry.reqId.poke(13.U)
      dut.io.push.pushCmdEntry.coreId.poke(1.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      dut.clock.step(1)

      dut.io.push.pushCmd.poke(true.B)
      dut.io.push.pushCmdEntry.reqId.poke(5.U)
      dut.io.push.pushCmdEntry.coreId.poke(2.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(2.U)

      dut.clock.step(1)

      dut.io.push.pushCmd.poke(false.B)
      dut.io.push.pushCmdEntry.reqId.poke(0.U)
      dut.io.push.pushCmdEntry.coreId.poke(0.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("h12".U)
      dut.io.pop.popEntry.index.expect("h2".U)
      dut.io.pop.popEntry.replaceWay.expect(2.U)
      dut.io.pop.popEntry.byteEn.expect(3.U)
      dut.io.pop.cmdCnt.expect(4.U)

      dut.io.push.full.expect(true.B)
      dut.io.pop.empty.expect(false.B)
      dut.io.nonCritInfo.fullCmds(0).expect(true.B)

      assertCmds(dut, Array(Some(ExpectedCmd(1, 2, 0)), Some(ExpectedCmd(11, 3, 3)), Some(ExpectedCmd(13, 1, 0)), Some(ExpectedCmd(5, 2, 2))), print = true)

      assertMshrStatusInNonCritQueue(
        dut,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(true, true, true, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step(1)

      dut.io.pop.pop.poke(false.B)

      // Update the byte enable for the second entry
      dut.io.push.updateByteEn.poke(true.B)
      dut.io.push.updateByteEnRow.poke(1.U)
      dut.io.push.updateByteEnCol.poke(1.U)
      dut.io.push.updateByteEnVal.poke(2.U)

      dut.clock.step(1)

      // Update the byte enable for the second entry again
      dut.io.push.updateByteEn.poke(true.B)
      dut.io.push.updateByteEnRow.poke(1.U)
      dut.io.push.updateByteEnCol.poke(2.U)
      dut.io.push.updateByteEnVal.poke(1.U)

      dut.clock.step(1)

      dut.io.push.updateByteEn.poke(false.B)
      dut.io.push.updateByteEnRow.poke(0.U)
      dut.io.push.updateByteEnCol.poke(0.U)
      dut.io.push.updateByteEnVal.poke(0.U)

      // Add more commands to the second fifo entry
      dut.io.push.pushCmd.poke(true.B)
      dut.io.push.mshrIdx.poke(1.U)
      dut.io.push.pushCmdEntry.reqId.poke(7.U)
      dut.io.push.pushCmdEntry.coreId.poke(2.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(1.U)

      dut.clock.step(1)

      dut.io.push.pushCmd.poke(false.B)
      dut.io.push.mshrIdx.poke(0.U)
      dut.io.push.pushCmdEntry.reqId.poke(0.U)
      dut.io.push.pushCmdEntry.coreId.poke(0.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("h44".U)
      dut.io.pop.popEntry.index.expect("h1".U)
      dut.io.pop.popEntry.replaceWay.expect(3.U)
      dut.io.pop.popEntry.byteEn.expect("b00011001".U)
      dut.io.pop.cmdCnt.expect(2.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)
      dut.io.nonCritInfo.fullCmds(1).expect(false.B)

      assertCmds(dut, Array(Some(ExpectedCmd(2, 2, 0)), Some(ExpectedCmd(7, 2, 1)), None, None), print = true)

      assertMshrStatusInNonCritQueue(
        dut,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, true, true, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step(1)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("haa".U)
      dut.io.pop.popEntry.index.expect("hc".U)
      dut.io.pop.popEntry.replaceWay.expect(2.U)
      dut.io.pop.popEntry.byteEn.expect("b00100000".U)
      dut.io.pop.cmdCnt.expect(0.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)
      dut.io.nonCritInfo.fullCmds(2).expect(false.B)

      assertCmds(dut, Array(None, None, None, None), print = true)

      assertMshrStatusInNonCritQueue(
        dut,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, false, true, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step(1)

      // Pop an entry
      dut.io.pop.pop.poke(true.B)
      dut.io.pop.popEntry.tag.expect("hbb".U)
      dut.io.pop.popEntry.index.expect("he".U)
      dut.io.pop.popEntry.replaceWay.expect(1.U)
      dut.io.pop.popEntry.byteEn.expect("b0000000".U)
      dut.io.pop.cmdCnt.expect(1.U)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(false.B)
      dut.io.nonCritInfo.fullCmds(3).expect(false.B)

      assertCmds(dut, Array(Option(ExpectedCmd(4, 3, 3)), None, None, None), print = true)

      assertMshrStatusInNonCritQueue(
        dut,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, false, false, true),
          fullSignals = Array(true, false, false, false)
        )
      )

      dut.clock.step(1)

      dut.io.push.full.expect(false.B)
      dut.io.pop.empty.expect(true.B)

      assertMshrStatusInNonCritQueue(
        dut,
        ExpectedMshrStatus(
          tags = Array("h12", "h44", "haa", "hbb"),
          indexes = Array("h2", "h1", "hc", "he"),
          validMSHRs = Array(false, false, false, false),
          fullSignals = Array(true, false, false, false)
        )
      )

      // Push an entry
      dut.io.push.pushReq.poke(true.B)
      dut.io.push.withCmd.poke(true.B)
      dut.io.push.pushReqEntry.tag.poke("ha".U)
      dut.io.push.pushReqEntry.index.poke("hb".U)
      dut.io.push.pushReqEntry.byteEn.poke(3.U)
      dut.io.push.pushReqEntry.replaceWay.poke(2.U)

      dut.io.push.pushCmdEntry.reqId.poke(12.U)
      dut.io.push.pushCmdEntry.coreId.poke(2.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      dut.clock.step(1)

      dut.io.push.pushReq.poke(false.B)
      dut.io.push.withCmd.poke(false.B)
      dut.io.push.pushReqEntry.tag.poke(0.U)
      dut.io.push.pushReqEntry.index.poke(0.U)
      dut.io.push.pushReqEntry.byteEn.poke(0.U)
      dut.io.push.pushReqEntry.replaceWay.poke(0.U)

      dut.io.push.pushCmdEntry.reqId.poke(0.U)
      dut.io.push.pushCmdEntry.coreId.poke(0.U)
      dut.io.push.pushCmdEntry.blockOffset.poke(0.U)

      assertMshrStatusInNonCritQueue(
        dut,
        ExpectedMshrStatus(
          tags = Array("ha", "h44", "haa", "hbb"),
          indexes = Array("hb", "h1", "hc", "he"),
          validMSHRs = Array(true, false, false, false),
          fullSignals = Array(false, false, false, false)
        )
      )
    }
  }
}
