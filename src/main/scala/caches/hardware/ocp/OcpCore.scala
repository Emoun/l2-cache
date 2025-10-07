package caches.hardware.ocp

import chisel3._

// Masters include a byte-enable signal
class OcpCoreMasterSignals(override val addrWidth: Int, override val dataWidth: Int)
  extends OcpMasterSignals(addrWidth, dataWidth) {
  val ByteEn = UInt((dataWidth / 8).W)
}

// Slave port is reverse of master port
class OcpCoreSlavePort(val addrWidth: Int, val dataWidth: Int) extends Bundle() {
  val M = Input(new OcpCoreMasterSignals(addrWidth, dataWidth))
  val S = Output(new OcpSlaveSignals(dataWidth))
}

