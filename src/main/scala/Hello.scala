
import chisel3._

class Hello extends Module {
  val io = IO(new Bundle{
    val o = Output(UInt(32.W))
  })
  io.o := 42.U
}

object Hello extends App {
  emitVerilog(new Hello())
}
