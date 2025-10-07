package caches.hardware.ocp

import chisel3._

// Burst masters provide handshake signals
class OcpBurstMasterSignals(addrWidth: Int, dataWidth: Int)
  extends OcpMasterSignals(addrWidth, dataWidth) {
  val DataValid = Output(UInt(1.W))
  val DataByteEn = Output(UInt((dataWidth / 8).W))
}

// Burst slaves provide handshake signal
class OcpBurstSlaveSignals(dataWidth: Int)
  extends OcpSlaveSignals(dataWidth) {
  val CmdAccept = Input(UInt(1.W))
  val DataAccept = Input(UInt(1.W))
}

// Master port
class OcpBurstMasterPort(addrWidth: Int, dataWidth: Int, burstLen: Int) extends Bundle() {
  val burstLength = burstLen
  val M = Output(new OcpBurstMasterSignals(addrWidth, dataWidth))
  val S = Input(new OcpBurstSlaveSignals(dataWidth))
}

// Slave port is reverse of master port
class OcpBurstSlavePort(val addrWidth: Int, val dataWidth: Int, val burstLen: Int) extends Bundle() {
  val burstLength = burstLen
  val M = Input(new OcpBurstMasterSignals(addrWidth, dataWidth))
  val S = Output(new OcpBurstSlaveSignals(dataWidth))
}