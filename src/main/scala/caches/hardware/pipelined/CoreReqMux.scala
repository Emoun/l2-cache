package caches.hardware.pipelined

import chisel3._
import chisel3.util._
import caches.hardware.util.DecoupledMux

class CoreReqMux(nCores: Int, addrWidth: Int, dataWidth: Int, reqIdWidth: Int) extends Module {
  val io = IO(new Bundle {
    val req1 = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
    val req1CoreID = Input(UInt(log2Up(nCores).W))
    val req2 = new CacheRequestIO(addrWidth, dataWidth, reqIdWidth)
    val req2CoreID = Input(UInt(log2Up(nCores).W))
    val out = Flipped(new CacheRequestIO(addrWidth, dataWidth, reqIdWidth))
    val outCoreID = Output(UInt(log2Up(nCores).W))
  })

  val sel = io.req2.reqId.valid
  val mux = DecoupledMux(io.req1.reqId, io.req2.reqId, sel = sel)

  io.out.reqId <> mux
  io.out.addr := Mux(sel, io.req2.addr, io.req1.addr)
  io.out.rw := Mux(sel, io.req2.rw, io.req1.rw)
  io.out.byteEn := Mux(sel, io.req2.byteEn, io.req1.byteEn)
  io.out.wData := Mux(sel, io.req2.wData, io.req1.wData)
  io.outCoreID := Mux(sel, io.req2CoreID, io.req1CoreID)
}
