package caches.hardware.pipelined.cache

import chisel3._
import chisel.lib.uart._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class BufferedRx(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle{
    val rxd = Input(UInt(1.W))
    val channel = new UartIO()
  })
  val rx = Module(new Rx(frequency, baudRate))
  val buf = Module(new Buffer())

  buf.io.in <> rx.io.channel
  buf.io.out <> io.channel
  rx.io.rxd <> io.rxd
}

class Uart(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle{
    val rxd = Input(UInt(1.W))
    val txd = Output(UInt(1.W))
    val rxChannel = new UartIO()
    val txChannel = Flipped(new UartIO())
  })

  val rx = Module(new BufferedRx(frequency, baudRate))
  val tx = Module(new BufferedTx(frequency, baudRate))

  io.rxChannel <> rx.io.channel
  rx.io.rxd <> io.rxd
  tx.io.channel <> io.txChannel
  io.txd <> tx.io.txd
}

class CacheRequestControllerDebug extends Bundle {
  val rxValid = Output(Bool())
  val rxBits = Output(UInt(8.W))
}

class CacheRequestController(nCores: Int, addrWidth: Int, reqIdWidth: Int, bytesPerSubBlock: Int, frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle{
    val rxd = Input(UInt(1.W))
    val txd = Output(UInt(1.W))
    val cache = Flipped(new CacheIO(nCores, reqIdWidth, addrWidth, bytesPerSubBlock * 8))
    val dbg = new CacheRequestControllerDebug()
  })

  val nCmdBytes = math.ceil((1 + reqIdWidth + log2Up(nCores) + addrWidth + (bytesPerSubBlock * 8)) / 8.0).toInt
  val nReadDataBytes = math.ceil(bytesPerSubBlock).toInt
  val nResponseHeaderBytes = math.ceil(1 + reqIdWidth / 8.0).toInt
  val nSendDataBytes = nReadDataBytes + nResponseHeaderBytes
  val sReceive :: sReq :: sWait :: sSend :: Nil = Enum(4)

  val uart = Module(new Uart(frequency, baudRate))

  // Registers
  val stateReg = RegInit(sReceive)
  val cmdReg = RegInit(VecInit(Seq.fill(nCmdBytes)(0.U(8.W))))
  val sendDataReg = RegInit(VecInit(Seq.fill(nSendDataBytes)(0.U(8.W))))
  val cmdRcvCntReg = RegInit(0.U(log2Up(nCmdBytes).W))
  val sendDataCntReg = RegInit(0.U(log2Up(nSendDataBytes).W))

  val txValid = WireDefault(false.B)
  val txBits = WireDefault(0.U(8.W))
  val rxReady = WireDefault(false.B)
  val reqIdValid = WireDefault(false.B)
  val respIdReady = WireDefault(false.B)

  // Decode command string
  val cmdAsUint = cmdReg.asUInt
  val rw = cmdAsUint(1 + reqIdWidth + log2Up(nCores) + addrWidth + (bytesPerSubBlock * 8), reqIdWidth + log2Up(nCores) + addrWidth + (bytesPerSubBlock * 8))
  val reqId = cmdAsUint((reqIdWidth - 1) + log2Up(nCores) + addrWidth + (bytesPerSubBlock * 8), log2Up(nCores) + addrWidth + (bytesPerSubBlock * 8))
  val coreId = cmdAsUint((log2Up(nCores) - 1) + addrWidth + (bytesPerSubBlock * 8), addrWidth + (bytesPerSubBlock * 8))
  val addr = cmdAsUint((addrWidth - 1) + (bytesPerSubBlock * 8), bytesPerSubBlock * 8)
  val wData = cmdAsUint((bytesPerSubBlock * 8) - 1, 0)

  switch(stateReg) {
    is(sReceive) { // Wait for a new command from uart
      rxReady := true.B
      when(uart.io.rxChannel.valid) {
        cmdRcvCntReg := cmdRcvCntReg + 1.U
        cmdReg(cmdRcvCntReg) := uart.io.rxChannel.bits
      }

      when(cmdRcvCntReg === nCmdBytes.U) {
        stateReg := sReq
        cmdRcvCntReg := 0.U
      }
    }
    is(sReq) { // Issue the received command
      reqIdValid := true.B
      when(io.cache.coreReqs(coreId).reqId.ready) {
        stateReg := sWait
      }
    }
    is(sWait) { // Wait for response from the L2 cache
      respIdReady := true.B
      when(io.cache.coreResps(coreId).reqId.valid) {
        for (sendByteIdx <- 0 until nReadDataBytes) {
          sendDataReg(sendByteIdx) := io.cache.coreResps(coreId).rData(7 + (sendByteIdx * 8), sendByteIdx * 8)
        }

        // Put the response status and reqId in the response too
        val responseHead = WireDefault(0.U((nResponseHeaderBytes * 8).W))
        responseHead := Cat(io.cache.coreResps(coreId).responseStatus, io.cache.coreResps(coreId).reqId.bits)
        for (sendByteIdx <- 0 until nResponseHeaderBytes) {
          sendDataReg(nReadDataBytes + sendByteIdx) := responseHead(7 + (sendByteIdx * 8), sendByteIdx * 8)
        }

        stateReg := sSend
      }
    }
    is(sSend) { // Send the received data back through UART
      txValid := true.B
      // Wait for ready signal before sending the next character
      when(uart.io.txChannel.ready) {
        sendDataCntReg := sendDataCntReg + 1.U
      }

      when(sendDataCntReg === nSendDataBytes.U) {
        stateReg := sReceive
        sendDataCntReg := 0.U
      }
    }
  }

  for (coreIdx <- 0 until nCores) {
    io.cache.coreReqs(coreIdx).reqId.valid := Mux(coreId === coreIdx.U, reqIdValid, false.B)
    io.cache.coreReqs(coreIdx).reqId.bits := reqId
    io.cache.coreReqs(coreIdx).rw := rw
    io.cache.coreReqs(coreIdx).addr := addr
    io.cache.coreReqs(coreIdx).wData := wData

    io.cache.coreResps(coreIdx).reqId.ready := Mux(coreId === coreIdx.U, respIdReady, false.B)
  }

  txBits := sendDataReg(sendDataCntReg)

  uart.io.txChannel.bits := txBits
  uart.io.txChannel.valid := txValid
  uart.io.rxChannel.ready := rxReady

  io.txd := uart.io.txd
  uart.io.rxd := io.rxd

  io.dbg.rxValid := DontCare
  io.dbg.rxBits := DontCare
  BoringUtils.bore(uart.io.rxChannel.valid, Seq(io.dbg.rxValid))
  BoringUtils.bore(uart.io.rxChannel.bits, Seq(io.dbg.rxBits))
}
