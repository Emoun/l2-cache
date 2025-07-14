package caches.hardware.pipelined.cache

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CacheRequestControllerTest extends AnyFlatSpec with ChiselScalatestTester {
  /**
   * Assuming one start one stop bit
   * @param dut
   * @param byte
   * @param cyclesPerBit
   */
  def sendByte(dut: CacheRequestController, byte: Int, cyclesPerBit: Int = 10): Unit = {
    // Send UART frame: 1 start bit (0), 8 data bits (LSB first), 1 stop bit (1)
    val byteBits = 0 +: (0 until 8).map(i => (byte >> i) & 1) :+ 1

    for (bit <- byteBits) {
      dut.io.rxd.poke(bit.U)
      dut.clock.step(cyclesPerBit)
    }

    // Wait for byte to arrive at the buffer
    dut.clock.step(1)

    // Check that the byte was received
    val valid = dut.io.dbg.rxValid.peek().litToBoolean
    assert(valid, "Expected UART to signal valid after byte received")

    val received = dut.io.dbg.rxBits.peek().litValue.toInt
    assert(received == byte, s"Received $received but expected $byte")
  }

  /**
   * Assuming one start one stop bit
   * @param dut
   * @param expectedByte
   * @param cyclesPerBit
   */
  def receiveByte(dut: CacheRequestController, expectedByte: Int, cyclesPerBit: Int = 10): Unit = {
    // Capture and print the UART line (txd) for each bit
    val bits = scala.collection.mutable.ArrayBuffer[Int]()

    for (_ <- 0 until (10 * cyclesPerBit)) {
      val txd = dut.io.txd.peek().litValue.toInt
      bits.append(txd)
      dut.clock.step()
    }

    // Optionally decode bits and check structure
    val sampledBits = bits.grouped(cyclesPerBit).map(_.head).toVector

    assert(sampledBits.head == 0, "Expected start bit (0)")
    val dataBits = sampledBits.slice(1, 9)
    val stopBit = sampledBits(9)

    val sentByte = dataBits.zipWithIndex.map { case (bit, i) => bit << i }.sum

    assert(sentByte == expectedByte, s"Transmitted bits don't match: got $sentByte expected $expectedByte")
    assert(stopBit == 1, "Expected stop bit (1)")
  }

  def expectAndConsumeRequest(dut: CacheRequestController, coreId: Int, reqId: Int, addr: String, rw: Boolean, wData: String): Unit = {
    dut.io.cache.coreReqs(coreId).reqId.valid.expect(true.B)
    dut.io.cache.coreReqs(coreId).reqId.bits.expect(reqId.U)
    dut.io.cache.coreReqs(coreId).addr.expect(addr.U)
    dut.io.cache.coreReqs(coreId).rw.expect(rw.B)
    dut.io.cache.coreReqs(coreId).wData.expect(wData.U)

    dut.io.cache.coreReqs(coreId).reqId.ready.poke(true.B)

    dut.clock.step(1)

    dut.io.cache.coreReqs(coreId).reqId.ready.poke(false.B)
  }

  def respondToRequest(dut: CacheRequestController, coreId: Int, reqId: Int, rData: String, responseStatus: Int): Unit = {
    dut.io.cache.coreResps(coreId).reqId.ready.expect(true.B)

    dut.io.cache.coreResps(coreId).reqId.valid.poke(true.B)
    dut.io.cache.coreResps(coreId).reqId.bits.poke(reqId.U)
    dut.io.cache.coreResps(coreId).rData.poke(rData.U)
    dut.io.cache.coreResps(coreId).responseStatus.poke(responseStatus.U)

    dut.clock.step(1)

    dut.io.cache.coreResps(coreId).reqId.valid.poke(false.B)
    dut.io.cache.coreResps(coreId).reqId.bits.poke(0.U)
    dut.io.cache.coreResps(coreId).rData.poke(0.U)
    dut.io.cache.coreResps(coreId).responseStatus.poke(0.U)
  }

  "CacheRequestControllerTest" should "work" in {
    val nCores = 4
    val addrWidth = 16
    val reqIdWidth = 2
    val bytesPerSubBlock = 4
    val frequency = 100000
    val baudRate = 50000
    val ccsPerUartBit = frequency / baudRate
    test(new CacheRequestController(nCores, addrWidth, reqIdWidth, bytesPerSubBlock, frequency, baudRate))
      .withAnnotations(Seq(WriteVcdAnnotation, WriteFstAnnotation)) { dut =>
        // Default signal assignments
        dut.io.rxd.poke(1.U)
        for (coreIdx <- 0 until nCores) {
          dut.io.cache.coreReqs(coreIdx).reqId.ready.poke(false.B)

          dut.io.cache.coreResps(coreIdx).reqId.valid.poke(false.B)
          dut.io.cache.coreResps(coreIdx).reqId.bits.poke(0.U)
          dut.io.cache.coreResps(coreIdx).rData.poke(0.U)
          dut.io.cache.coreResps(coreIdx).responseStatus.poke(0.U)
        }

        dut.clock.step(5)

        // Send a command to the cache request controller byte by byte
        sendByte(dut, byte = 0xEF, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(1)

        sendByte(dut, byte = 0xBE, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(1)

        sendByte(dut, byte = 0xAD, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(1)

        sendByte(dut, byte = 0xDE, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(1)

        sendByte(dut, byte = 0xCC, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(1)

        sendByte(dut, byte = 0x00, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(1)

        sendByte(dut, byte = 0x1E, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(5)

        expectAndConsumeRequest(dut, coreId = 2, reqId = 3, addr = "h00cc", rw = true, wData = "hdeadbeef")

        dut.clock.step(1)

        respondToRequest(dut, coreId = 2, reqId = 3, rData = "hbd42f9c3", responseStatus = 1)

        dut.clock.step(3) // Need to step for 3 cc since, it will take us some time to put the value in the buffer

        receiveByte(dut, expectedByte = 0xC3, cyclesPerBit = ccsPerUartBit)

        dut.clock.step(4)

        receiveByte(dut, expectedByte = 0xf9, cyclesPerBit = 2)

        dut.clock.step(4)

        receiveByte(dut, expectedByte = 0x42, cyclesPerBit = 2)

        dut.clock.step(4)

        receiveByte(dut, expectedByte = 0xbd, cyclesPerBit = 2)

        dut.clock.step(4)

        receiveByte(dut, expectedByte = 0x07, cyclesPerBit = 2)

        dut.clock.step(4)
      }
  }
}
