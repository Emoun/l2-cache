package caches.hardware.blocking

import caches.hardware.reppol.ReplacementPolicyIO
import chisel3._
import chisel3.util._

class ControllerReqIO(nCores: Int) extends Bundle {
  val rw = Input(Bool())
  val reqId = Input(UInt(log2Up(nCores).W))
  val req = Input(Bool())
  val ack = Output(Bool())
  val status = Output(UInt(1.W)) // 0: OK, 1: REJECT
}

/**
 *
 * @deprecated
 */
class CacheController(ways: Int, sets: Int, nCores: Int) extends Module {
  val io = IO(new Bundle {
    val mem = Flipped(new ControllerMemIO(ways, sets))
    val repPol = Flipped(new ReplacementPolicyIO(ways, sets, nCores))
    val higher = new ControllerReqIO(nCores)
    val lower = Flipped(new ControllerReqIO(nCores))
  })

  val sIdle :: sCompareTag :: sWriteBack :: sMemFetch :: sWriteWait :: sFetchWait :: Nil = Enum(6)
  val stateReg = RegInit(sIdle)

  // Registers
  val opReg = RegInit(WireDefault(0.U(1.W)))
  val reqIdReg = RegInit(WireDefault(0.U(log2Up(nCores).W)))

  // Default signal assignments
  val latchReq = WireDefault(false.B)
  val updateLine = WireDefault(false.B)
  val replaceLine = WireDefault(false.B)
  val writeBack = WireDefault(false.B)
  val higherAck = WireDefault(false.B)
  val higherStatus = WireDefault(0.U(1.W))
  val lowerRw = WireDefault(false.B)
  val lowerReq = WireDefault(false.B)
  val policyUpdate = WireDefault(false.B)
  val evict = WireDefault(false.B)

  switch(stateReg) {
    is(sIdle) {
      when(io.higher.req) {
        latchReq := true.B
        opReg := io.higher.rw
        reqIdReg := io.higher.reqId
        stateReg := sCompareTag
      }
    }
    is(sCompareTag) {
      when(io.mem.hit) { // Hit case
        // Update the replacement policy on a hit
        policyUpdate := true.B

        when(opReg === 1.U) {
          updateLine := true.B
        }

        higherAck := true.B
        higherStatus := 0.U // OK response
        stateReg := sIdle
      }.otherwise { // Miss case
        // Check if there is a way to evict, if not we reject the request and return to the idle state
        // The core is expected to retry at a later time
        when(io.repPol.isValid) {
          when(io.mem.dirty(io.repPol.replaceWay)) {
            stateReg := sWriteBack
          }.otherwise {
            stateReg := sMemFetch
          }
        } .otherwise {
          higherAck := true.B
          higherStatus := 1.U // REJECT response
          stateReg := sIdle
        }
      }
    }
    is(sWriteBack) {
      lowerRw := true.B
      lowerReq := true.B
      writeBack := true.B
      stateReg := sWriteWait
    }
    is(sWriteWait) {
      when(io.lower.ack) {
        stateReg := sMemFetch
      }
    }
    is(sMemFetch) {
      lowerRw := false.B
      lowerReq := true.B
      stateReg := sFetchWait
    }
    is(sFetchWait) {
      when(io.lower.ack) {
        evict := true.B
        replaceLine := true.B
        stateReg := sCompareTag
      }
    }
  }

  io.repPol.update.valid := policyUpdate
  io.repPol.update.bits := io.mem.hitWay
  io.repPol.updateCoreId := reqIdReg
  io.repPol.setIdx := io.mem.set
  io.repPol.evict := evict
  io.repPol.stall := false.B

  io.mem.latchReq := latchReq
  io.mem.validReq := stateReg =/= sIdle
  io.mem.updateLine := updateLine
  io.mem.replaceLine := replaceLine
  io.mem.replaceWay := io.repPol.replaceWay
  io.mem.writeBack := writeBack

  io.higher.ack := higherAck
  io.higher.status := higherStatus

  io.lower.rw := lowerRw
  io.lower.req := lowerReq
  io.lower.reqId := reqIdReg
}
