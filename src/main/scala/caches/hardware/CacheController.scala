package caches.hardware

import caches.hardware.reppol.ReplacementPolicyType
import chisel3._
import chisel3.util._

class controlReqIO extends Bundle {
  val rw = Input(Bool())
  val req = Input(Bool())
  val ack = Output(Bool())
}

class CacheController(ways: Int, sets: Int, policyGen: () => ReplacementPolicyType) extends Module {
  val io = IO(new Bundle {
    val mem = Flipped(new controlMemIO(ways, sets))
    val higher = new controlReqIO
    val lower = Flipped(new controlReqIO)
  })

  val sIdle :: sCompareTag :: sWriteBack :: sMemFetch :: sWriteWait :: sFetchWait :: Nil = Enum(6)
  val stateReg = RegInit(sIdle)

  // Default signal assignments
  val latchReq = WireDefault(false.B)
  val updateLine = WireDefault(false.B)
  val replaceLine = WireDefault(false.B)
  val writeBack = WireDefault(false.B)
  val higherAck = WireDefault(false.B)
  val lowerRw = WireDefault(false.B)
  val lowerReq = WireDefault(false.B)
  val policyUpdate = WireDefault(false.B)

  val opReg = RegInit(WireDefault(0.U(1.W)))
  val repPolicy = Module(policyGen())

  switch(stateReg) {
    is(sIdle) {
      when(io.higher.req) {
        latchReq := true.B
        opReg := io.higher.rw
        stateReg := sCompareTag
      }
    }
    is(sCompareTag) {
      when(io.mem.hit) {
        // Update the replacement policy on a hit
        policyUpdate := true.B

        when(opReg === 1.U) {
          updateLine := true.B
        }

        higherAck := true.B
        stateReg := sIdle
      }.otherwise {
        // We need to return a list of possible eviction candidates and then take the first one to evict
        when(io.mem.dirty(repPolicy.io.replaceWay)) {
          stateReg := sWriteBack
        }.otherwise {
          stateReg := sMemFetch
        }
      }
    }
    is(sWriteBack) {
      lowerRw := true.B
      lowerReq := true.B
      writeBack := true.B
      stateReg := sWriteWait
    }
    is(sMemFetch) {
      lowerRw := false.B
      lowerReq := true.B
      stateReg := sFetchWait
    }
    is(sFetchWait) {
      when(io.lower.ack) {
        replaceLine := true.B
        stateReg := sCompareTag
      }
    }
    is(sWriteWait) {
      when(io.lower.ack) {
        stateReg := sMemFetch
      }
    }
  }

  repPolicy.io.index := io.mem.set
  repPolicy.io.update.valid := policyUpdate
  repPolicy.io.update.bits := io.mem.hitWay

  io.mem.latchReq := latchReq
  io.mem.validReq := stateReg =/= sIdle
  io.mem.updateLine := updateLine
  io.mem.replaceLine := replaceLine
  io.mem.replaceWay := repPolicy.io.replaceWay
  io.mem.writeBack := writeBack

  io.higher.ack := higherAck
  io.lower.rw := lowerRw
  io.lower.req := lowerReq
}
