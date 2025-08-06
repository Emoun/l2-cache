package caches.hardware.pipelined.cache

import ocp._
import chisel3._
import chisel3.util._
import caches.hardware.reppol._

class OcpCacheWrapperPort(
                           nCores: Int,
                           addrWidth: Int,
                           coreDataWidth: Int,
                           coreBurstLen: Int,
                           memDataWidth: Int,
                           memBurstLen: Int,
                         ) extends Bundle {
  val core = new OcpBurstSlavePort(addrWidth, coreDataWidth, coreBurstLen)
  val inCoreId = Input(UInt(log2Up(nCores).W))
  val mem = new OcpBurstMasterPort(addrWidth, memDataWidth, memBurstLen)
  val scheduler = new SchedulerIO(nCores) // TODO: Should probably be an ocp interface as well
}

/**
 * A module that wraps a shared pipelined cache into an OCP interface.
 * @param l2Cache A l2 cache generating function
 */
class OcpCacheWrapper(
                       nCores: Int,
                       addrWidth: Int,
                       coreDataWidth: Int,
                       coreBurstLen: Int,
                       memDataWidth: Int,
                       memBurstLen: Int,
                       l2Cache: () => SharedPipelinedCache
                     ) extends Module {
  val io = IO(new OcpCacheWrapperPort(nCores, addrWidth, coreDataWidth, coreBurstLen, memDataWidth, memBurstLen))

  val ocpSlaveAdapter = Module(new OcpBurstSlaveToCacheAdapter(addrWidth, coreDataWidth, coreBurstLen))
  val ocpMasterAdapter = Module(new CacheToOcpBurstMasterAdapter(addrWidth, memDataWidth, memBurstLen))
  val cache = Module(l2Cache())

  cache.io.scheduler <> io.scheduler

  ocpSlaveAdapter.io.ocpBurst <> io.core
  cache.io.core <> ocpSlaveAdapter.io.corePort
  // TODO: Think about how to pass a core ID, since patmos memory arbiter does not provide a core ID
  cache.io.inCoreId := io.inCoreId

  cache.io.mem <> ocpMasterAdapter.io.cache
  io.mem <> ocpMasterAdapter.io.ocpBurst
}
