package caches.hardware.old

import caches.hardware.util.Constants.ADDRESS_WIDTH
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SetAssociateCacheMemoryTest extends AnyFlatSpec with ChiselScalatestTester {
  "SetAssociateCacheMemory" should "fetch correct word from a cache line" in {
    val size = 256
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways

    test(new SetAssociateCacheMemory(ways, nSets, bytesPerBlock, bytesPerWord, addressWidth)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.controller.latchReq.poke(false.B)
      dut.io.controller.validReq.poke(false.B)
      dut.io.controller.replaceLine.poke(false.B)
      dut.io.controller.updateLine.poke(false.B)
      dut.io.controller.replaceWay.poke("b00".U)

      dut.clock.step(1)

      dut.io.controller.latchReq.poke(true.B)
      dut.io.higher.addr.poke("b01010101000".U)

      dut.clock.step(1)

      dut.io.controller.latchReq.poke(false.B)
      dut.io.controller.validReq.poke(true.B)
      dut.io.higher.addr.poke(0.U)

      dut.clock.step(1)

      // Expect a miss since there is nothing loaded in the memory just yet
      dut.io.controller.hit.expect(false.B)
      dut.io.controller.dirty.expect(false.B)
      dut.io.lower.addr.expect("b01010101000".U)

      dut.clock.step(5) // Latency from lower level memory

      // Instruct the cache to load a cache line from the lower level memory
      dut.io.lower.rData.poke("hbeefdeaddeadbeef".U)
      dut.io.controller.replaceLine.poke(true.B)
      dut.io.controller.replaceWay.poke(0.U)

      dut.clock.step(1)

      // Expect there to be a hit now and the data to be correct
      dut.io.controller.replaceLine.poke(false.B)
      dut.io.controller.hit.expect(true.B)
      dut.io.higher.rData.expect("hdeadbeef".U)

      dut.clock.step(1)

      // Initiate a write request from the lower level to the same block
      dut.io.controller.latchReq.poke(true.B)
      dut.io.controller.validReq.poke(false.B)
      dut.io.higher.addr.poke("b01010101100".U)
      dut.io.higher.wData.poke("hcafebabe".U)

      dut.clock.step(1)

      dut.io.controller.latchReq.poke(false.B)
      dut.io.controller.validReq.poke(true.B)
      dut.io.higher.addr.poke(0.U)

      dut.clock.step(1)

      // Expect there to be a hit now and the data to be correct
      dut.io.controller.updateLine.poke(true.B)
      dut.io.controller.hit.expect(true.B)
      dut.io.higher.rData.expect("hbeefdead".U)

      dut.clock.step(1)

      // Expect the cache line to be updated now
      dut.io.controller.updateLine.poke(false.B)
      dut.io.controller.hit.expect(true.B)
      dut.io.lower.wData.expect("hcafebabedeadbeef".U)
    }
  }
}
