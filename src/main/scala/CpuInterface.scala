import chisel3._

/**
 * A simple IO interface, as seen from the CPU.
 * ack is used for acknowledgement in the following clock cycle, or later. (like OCPcore in Patmos).
 * Can be used to stall the CPU.
 */
class CpuInterface() extends Bundle {
  val address = Output(UInt(32.W))
  val rd = Output(Bool())
  val wr = Output(Bool())
  val rdData = Input(UInt(32.W))
  val wrData = Output(UInt(32.W))
  // val wrMask = Input(UInt(4.W))
  val ack = Input(Bool())
  val highPrio = Output(Bool())
}