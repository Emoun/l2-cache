package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._

class RequestArbiter(nReqs: Int, addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Module {
  val io = IO(new Bundle {
    val ports = Vec(nReqs, new CacheRequestIO(addrWidth, dataWidth, reqIdWidth))
    val out = Flipped(new CacheRequestIO(addrWidth, dataWidth, reqIdWidth))
    val chosen = Output(UInt(log2Up(nReqs).W))
  })

  val arbiter = Module(new RRArbiter(UInt(reqIdWidth.W), nReqs))

  for (coreId <- 0 until nReqs) {
    arbiter.io.in(coreId) <> io.ports(coreId).reqId
  }

  val chosen = arbiter.io.chosen

  io.out.reqId <> arbiter.io.out
  io.out.rw := io.ports(chosen).rw
  io.out.wData := io.ports(chosen).wData
  io.out.addr := io.ports(chosen).addr
  io.chosen := chosen
}
