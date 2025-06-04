package caches.hardware.pipelined.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PipelinedCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  def performCacheRequest(dut: PipelinedCache, coreId: Int, addr: String, rw: Boolean, wData: Option[String] = None): Unit = {
    // Expect the cache to be ready to accept a request
    dut.io.coreReqs(coreId).reqId.ready.expect(true.B)

    // Make request on behalf of the second core
    dut.io.coreReqs(coreId).reqId.valid.poke(true.B)
    dut.io.coreReqs(coreId).reqId.bits.poke(coreId.U)
    dut.io.coreReqs(coreId).addr.poke(addr.U)
    dut.io.coreReqs(coreId).rw.poke(rw.B)

    val wDataValue = wData match {
      case Some(data) => data.U
      case None => 0.U
    }

    dut.io.coreReqs(coreId).wData.poke(wDataValue)

    // TODO: Extend this function to handle the case when the arbiter is not ready to accept the request
    dut.clock.step(1)

    dut.io.coreReqs(coreId).reqId.valid.poke(false.B)
    dut.io.coreReqs(coreId).reqId.bits.poke(0.U)
    dut.io.coreReqs(coreId).addr.poke(0.U)
    dut.io.coreReqs(coreId).rw.poke(false.B)
    dut.io.coreReqs(coreId).wData.poke(0.U)
  }

  def performMemRead(dut: PipelinedCache, expectedAddr: String, nBurst: Int, readData: Array[String]): Unit = {
    dut.io.memController.rChannel.rAddr.ready.poke(true.B)
    dut.io.memController.rChannel.rData.valid.poke(false.B)
    dut.io.memController.rChannel.rData.bits.poke(0.U)

    dut.io.memController.rChannel.rAddr.valid.expect(true.B)
    dut.io.memController.rChannel.rAddr.bits.expect(expectedAddr.U)

    dut.clock.step(1)

    dut.io.memController.rChannel.rAddr.ready.poke(false.B)
    dut.io.memController.rChannel.rLast.poke(false.B)

    for (i <- 0 until nBurst) {
      dut.io.memController.rChannel.rData.valid.poke(true.B)
      dut.io.memController.rChannel.rData.bits.poke(readData(i).U)
      dut.io.memController.rChannel.rData.ready.expect(true.B)

      if (i === nBurst - 1) {
        dut.io.memController.rChannel.rLast.poke(true.B)
      }

      dut.clock.step(1)
    }

    dut.io.memController.rChannel.rAddr.ready.poke(true.B)
    dut.io.memController.rChannel.rData.valid.poke(false.B)
    dut.io.memController.rChannel.rData.bits.poke(0.U)
    dut.io.memController.rChannel.rLast.poke(false.B)
  }

  "PipelinedCache" should "work" in  {
    val nCores = 4
    test(new PipelinedCache(
      sizeInBytes = 512, nWays = 8, nCores = nCores, addressWidth = 16, bytesPerBlock = 16, bytesPerSubBlock = 8, bytesPerBurst = 8
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default inputs

      for(i <- 0 until nCores) {
        dut.io.coreReqs(i).reqId.valid.poke(false.B)
        dut.io.coreReqs(i).reqId.bits.poke(0.U)
        dut.io.coreReqs(i).addr.poke(0.U)
        dut.io.coreReqs(i).rw.poke(false.B)
        dut.io.coreReqs(i).wData.poke(0.U)
      }

      dut.io.repPol.replaceWay.poke(0.U)
      dut.io.repPol.isValid.poke(true.B)

      dut.io.memController.rChannel.rAddr.ready.poke(true.B)
      dut.io.memController.rChannel.rData.valid.poke(false.B)
      dut.io.memController.rChannel.rData.bits.poke(0.U)
      dut.io.memController.rChannel.rLast.poke(false.B)
      dut.io.memController.wChannel.wAddr.ready.poke(true.B)
      dut.io.memController.wChannel.wData.ready.poke(true.B)

      dut.clock.step(5)

      // Access data that is not yet in the cache
      performCacheRequest(dut, 1, "b101101000", false)

      dut.clock.step(3)

      performMemRead(dut, "b101101000", 2, Array("hbeefdeaddeadbeef", "hbabecafecafebabe"))

      dut.io.coreResps(1).reqId.ready.poke(true.B)
      dut.io.coreResps(1).reqId.valid.expect(true.B)
      dut.io.coreResps(1).reqId.bits.expect(1.U)
      dut.io.coreResps(1).rData.expect("hbabecafecafebabe".U)
      dut.io.coreResps(1).responseStatus.expect(0.U)

      dut.clock.step(1)

      dut.io.coreResps(1).reqId.ready.poke(false.B)

      // Access data that is already in the cache
      performCacheRequest(dut, 1, "b101100000", false)

      dut.clock.step(2)

      dut.io.coreResps(1).reqId.ready.poke(true.B)
      dut.io.coreResps(1).reqId.valid.expect(true.B)
      dut.io.coreResps(1).reqId.bits.expect(1.U)
      dut.io.coreResps(1).rData.expect("hbeefdeaddeadbeef".U)
      dut.io.coreResps(1).responseStatus.expect(0.U)

      dut.clock.step(1)

      dut.io.coreResps(1).reqId.ready.poke(false.B)

      // Write over the data that is in the cache (making the line dirty)
      performCacheRequest(dut, 2, "b101100000", true, Some("hd00dfeedfeedd00d"))

      dut.clock.step(2)

      dut.io.coreResps(2).reqId.ready.poke(true.B)
      dut.io.coreResps(2).reqId.valid.expect(true.B)
      dut.io.coreResps(2).reqId.bits.expect(2.U)
      dut.io.coreResps(1).rData.expect("hbeefdeaddeadbeef".U)
      dut.io.coreResps(2).responseStatus.expect(0.U)

      dut.clock.step(1)

      dut.io.coreResps(1).reqId.ready.poke(false.B)

      // Evict the dirty line
      performCacheRequest(dut, 3, "b111100000", false)

      dut.clock.step(3)

      performMemRead(dut, "b111100000", 2, Array("hbeefdeaddeadbeef", "hd00dfeedfeedd00d"))

      dut.clock.step(2)

      // TODO: Perform a write request
    }
  }
}
