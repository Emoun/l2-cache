package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._

/**
 * Demultiplexer for core responses.
 */
class CoreRespDemux(nPorts: Int, dataWidth: Int, reqIdWidth: Int) extends Module {
  val io = IO(new Bundle {
    val sel = Input(UInt(log2Up(nPorts).W))
    val in = Flipped(new CacheResponseIO(dataWidth, reqIdWidth))
    val resps = Vec(nPorts, new CacheResponseIO(dataWidth, reqIdWidth))
  })

  for (i <- 0 until nPorts) {
    // Default assignments
    io.resps(i).reqId.valid := false.B
    io.resps(i).reqId.bits := DontCare
    io.resps(i).rData := DontCare
    io.resps(i).responseStatus := DontCare

    when(i.U === io.sel) {
      io.in <> io.resps(i)
    }
  }
}