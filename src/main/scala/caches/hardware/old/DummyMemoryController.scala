package caches.hardware.old

import chisel3._
import chisel3.util._

/**
 *
 * @deprecated
 */
class MemoryControllerIO(addrWidth: Int, burstWidth: Int) extends Bundle {
  val ack = Input(Bool())
  val respStatus = Input(UInt(1.W))
  val rData = Input(UInt(burstWidth.W))
  val req = Output(Bool())
  val rw = Output(Bool())
  val addr = Output(UInt(addrWidth.W))
  val wData = Output(UInt(burstWidth.W))
}

class DummyMemoryController(addressWidth: Int, blockSize: Int, burstSize: Int) extends Module {
  val io = IO(new Bundle {
    val cache = new SharedCacheIO(addressWidth, blockSize * 8, 0)
    val mem = new MemoryControllerIO(addressWidth, burstSize * 8)
  })

  val nBursts = blockSize / burstSize
  val sIdle :: sRead :: sWrite :: sReadBurst :: sWriteBurst :: sDone :: Nil = Enum(6)

  val stateReg = RegInit(sIdle)
  val rwDataReg = RegInit(VecInit(Seq.fill(nBursts)(0.U((burstSize * 8).W))))
  val addrReg = RegInit(0.U(addressWidth.W))
  val burstCounter = RegInit(0.U(log2Up(nBursts).W))

  // Default signal assignments
  val memReq = WireDefault(false.B)
  val memRw = WireDefault(false.B)
  val memWData = WireDefault(0.U((burstSize * 8).W))

  val cacheAck = WireDefault(false.B)
  val cacheRData = WireDefault(0.U((blockSize * 8).W))
  val cacheRespStatus = WireDefault(0.U(1.W))

  switch(stateReg) {
    is(sIdle) {
      when(io.cache.req) {
        addrReg := io.cache.addr

        when(io.cache.rw) {
          for (i <- 0 until nBursts) {
            rwDataReg(i) := io.cache.wData((i * burstSize * 8) + (burstSize * 8 - 1), i * burstSize * 8)
          }

          stateReg := sWrite
        }.otherwise {
          stateReg := sRead
        }
      }
    }

    is(sRead) {
      memReq := true.B
      stateReg := sReadBurst
    }

    is(sWrite) {
      memReq := true.B
      memRw := true.B
      stateReg := sWriteBurst
    }

    is(sReadBurst) {
      burstCounter := burstCounter + 1.U

      rwDataReg(burstCounter) := io.mem.rData

      when (io.mem.ack) {
        when(burstCounter === (nBursts - 1).U && io.mem.ack) {
          burstCounter := 0.U
        }
        stateReg := sDone
      }
    }

    is(sWriteBurst) {
      burstCounter := burstCounter + 1.U

      memWData := rwDataReg(burstCounter)

      when (io.mem.ack) {
        when(burstCounter === (nBursts - 1).U) {
          burstCounter := 0.U
        }

        stateReg := sDone
      }
    }

    is(sDone) {
      cacheRespStatus := io.mem.respStatus
      cacheRData := rwDataReg.asUInt
      cacheAck := true.B
      stateReg := sIdle
//      rwDataReg := 0.U
    }
  }

  io.mem.req := memReq
  io.mem.rw := memRw
  io.mem.wData := memWData
  io.mem.addr := addrReg

  io.cache.ack := cacheAck
  io.cache.rData := cacheRData
  io.cache.responseStatus := cacheRespStatus
}
