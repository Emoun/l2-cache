package caches.hardware.pipelined

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MemoryInterfaceTest extends AnyFlatSpec with ChiselScalatestTester {
  "MemoryInterface" should "work" in {
    test(new MemoryInterface(
      nCores = 4,
      nWays = 8,
      reqIdWidth = 16,
      tagWidth = 19,
      indexWidth = 8,
      blockOffsetWidth = 2,
      blockWidth = 32 * 8,
      subBlockWidth = 8 * 8,
      beatSize = 4,
      burstLen = 4
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(false.B)
      dut.io.memController.rChannel.rData.bits.poke(0.U)
      dut.io.memController.wChannel.wAddr.ready.poke(false.B)
      dut.io.memController.wChannel.wData.ready.poke(false.B)
      dut.io.wbFifo.empty.poke(true.B)
      dut.io.wbFifo.popEntry.tag.poke(0.U)
      dut.io.wbFifo.popEntry.index.poke(0.U)
      dut.io.wbFifo.popEntry.wbData.poke(0.U)
      dut.io.missFifo.empty.poke(true.B)
      dut.io.missFifo.popEntry.rw.poke(false.B)
      dut.io.missFifo.popEntry.reqId.poke(0.U)
      dut.io.missFifo.popEntry.coreId.poke(0.U)
      dut.io.missFifo.popEntry.replaceWay.poke(0.U)
      dut.io.missFifo.popEntry.tag.poke(0.U)
      dut.io.missFifo.popEntry.index.poke(0.U)
      dut.io.missFifo.popEntry.blockOffset.poke(0.U)

      dut.clock.step(2)

      // Simulate a write-back operation
      dut.io.wbFifo.empty.poke(false.B)
      dut.io.wbFifo.popEntry.tag.poke(1.U)
      dut.io.wbFifo.popEntry.index.poke(2.U)
      dut.io.wbFifo.popEntry.wbData.poke("h7e3fa2c4d9b118fe0a27d6e54c3b98a1f4cd239a5e7b06d1acfe0b8dc4e73190".U)

      dut.clock.step(1)

      // Check that write address is valid
      dut.io.memController.wChannel.wAddr.valid.expect(true.B)
      dut.io.memController.wChannel.wAddr.bits.expect("b010000001000000".U)
      dut.io.memController.wChannel.wData.valid.expect(false.B)
      dut.io.memController.wChannel.wData.bits.expect(0.U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect(0.U)

      dut.io.memController.wChannel.wAddr.ready.poke(true.B)

      // Assert the first command for transferring a single block
      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("hc4e73190".U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wAddr.ready.poke(false.B)
      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("hacfe0b8d".U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("h5e7b06d1".U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("hf4cd239a".U)
      dut.io.memController.wChannel.wLast.expect(true.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that write address is valid for the second command _________
      dut.io.memController.wChannel.wAddr.valid.expect(true.B)
      dut.io.memController.wChannel.wAddr.bits.expect("b010000001010000".U)
      dut.io.memController.wChannel.wData.valid.expect(false.B)
      dut.io.memController.wChannel.wData.bits.expect(0.U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect(0.U)

      dut.io.memController.wChannel.wAddr.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("h4c3b98a1".U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wAddr.ready.poke(false.B)
      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("h0a27d6e5".U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("hd9b118fe".U)
      dut.io.memController.wChannel.wLast.expect(false.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      // Check that the write data is valid
      dut.io.memController.wChannel.wAddr.valid.expect(false.B)
      dut.io.memController.wChannel.wData.valid.expect(true.B)
      dut.io.memController.wChannel.wData.bits.expect("h7e3fa2c4".U)
      dut.io.memController.wChannel.wLast.expect(true.B)
      dut.io.memController.wChannel.wStrb.expect("b1111".U)

      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(1)

      dut.io.memController.wChannel.wData.ready.poke(false.B)

      dut.clock.step(1)

      // Expect the update logic to not receive any data and the wb entry to be popped
      dut.io.updateLogic.valid.expect(false.B)
      dut.io.wbFifo.pop.expect(true.B)

      dut.clock.step(1)

      // Simulate a fill operation
      dut.io.wbFifo.empty.poke(true.B)
      dut.io.missFifo.empty.poke(false.B)
      dut.io.missFifo.popEntry.tag.poke(5.U)
      dut.io.missFifo.popEntry.index.poke(9.U)

      dut.clock.step(1)

      // Check that the read address is valid
      dut.io.memController.rChannel.rAddr.valid.expect(true.B)
      dut.io.memController.rChannel.rAddr.bits.expect("b01010000100100000".U)
      dut.io.memController.rChannel.rData.ready.expect(false.B)

      dut.io.memController.rChannel.rAddr.ready.poke(true.B)

      dut.clock.step(1)

      // Give data for the first read command
      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("h7e3fa2c4".U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Give data for the first read command
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("hd9b118fe".U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Give data for the first read command
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("h0a27d6e5".U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Give data for the first read command
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("h4c3b98a1".U)
      dut.io.memController.rChannel.rLast.poke(true.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rAddr.bits.expect(0.U)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Check that the read address is valid for the second command
      dut.io.memController.rChannel.rAddr.valid.expect(true.B)
      dut.io.memController.rChannel.rAddr.bits.expect("b01010000100110000".U)
      dut.io.memController.rChannel.rData.ready.expect(false.B)

      dut.io.memController.rChannel.rAddr.ready.poke(true.B)

      dut.clock.step(1)

      // Give data for the second read command
      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("hf4cd239a".U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Give data for the second read command
      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("h5e7b06d1".U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Give data for the second read command
      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("hacfe0b8d".U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      // Give data for the second read command
      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke("hc4e73190".U)
      dut.io.memController.rChannel.rLast.poke(true.B)

      dut.io.memController.rChannel.rAddr.valid.expect(false.B)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      dut.clock.step(1)

      dut.io.memController.rChannel.rAddr.ready.poke(false.B)
      dut.io.memController.rChannel.rData.valid.poke(false.B)
      dut.io.memController.rChannel.rData.bits.poke(0.U)
      dut.io.memController.rChannel.rLast.poke(false.B)

      dut.clock.step(1)

      // Expect the update logic to receive the read data
      dut.io.missFifo.pop.expect(true.B)
      dut.io.updateLogic.valid.expect(true.B)
      dut.io.updateLogic.memReadData(0).expect("hd9b118fe7e3fa2c4".U)
      dut.io.updateLogic.memReadData(1).expect("h4c3b98a10a27d6e5".U)
      dut.io.updateLogic.memReadData(2).expect("h5e7b06d1f4cd239a".U)
      dut.io.updateLogic.memReadData(3).expect("hc4e73190acfe0b8d".U)

      dut.clock.step(1)

      dut.io.missFifo.empty.poke(true.B)

      dut.clock.step(1)
    }
  }
}
