import chisel3._

/**
 * A simple IO interface, as seen from the Slave, similar to soc-comm.
 * ack is used for acknowledgement in the following clock cycle, or later. (like OCPcore in Patmos).
 * Can be used to stall the CPU.
 */
class CpuInterface() extends Bundle {
  val address = Input(UInt(32.W))
  val rd = Input(Bool())
  val wr = Input(Bool())
  val rdData = Output(UInt(32.W))
  val wrData = Input(UInt(32.W))
  val wrMask = Input(UInt(4.W))
  val ack = Output(Bool())
  val highPrio = Input(Bool())
}