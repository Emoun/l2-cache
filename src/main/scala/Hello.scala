
import chisel3._

class Hello(n: Int) extends Module {
  val io = IO(new Bundle {
    val ports = Vec(n, new CpuInterface())
  })

  for (i <- 0 until n) {
    io.ports(i).ack := true.B
    io.ports(i).rdData := 0.U
  }
}

object Hello extends App {
  println("Generating Verilog")
  emitVerilog(new Hello(4))
}
