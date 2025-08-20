package caches.hardware.pipelined

import ocp._
import chisel3._
import chisel3.util._
import caches.hardware.util.Constants.CONTENTION_LIMIT_WIDTH

class OcpCacheWrapperPort(
                           nCores: Int,
                           addrWidth: Int,
                           coreDataWidth: Int,
                           coreBurstLen: Int,
                           memDataWidth: Int,
                           memBurstLen: Int,
                         ) extends Bundle {
  val core = new OcpBurstSlavePort(addrWidth, coreDataWidth, coreBurstLen)
  val mem = new OcpBurstMasterPort(addrWidth, memDataWidth, memBurstLen)
  val scheduler = new OcpCoreSlavePort(log2Up(nCores), CONTENTION_LIMIT_WIDTH)
}

/**
 * A module that wraps a shared pipelined cache into an OCP compatible interface.
 * This wrapper has an OCP burst interface for the accessing core, the OCPCore interface for the scheduler,
 * and a single OCP burst interface for the memory.
 *
 * @param l2Cache A l2 cache generating function
 */
class OcpCacheWrapperSingleCore(
                       addrWidth: Int,
                       coreDataWidth: Int,
                       coreBurstLen: Int,
                       memDataWidth: Int,
                       memBurstLen: Int,
                       l2Cache: () => SharedPipelinedCache
                     ) extends Module {
  val io = IO(new OcpCacheWrapperPort(1, addrWidth, coreDataWidth, coreBurstLen, memDataWidth, memBurstLen))

  val cache = Module(l2Cache())
  val ocpSlaveAdapter = Module(new OcpBurstSlaveToCacheRequestAdapter(addrWidth, coreDataWidth, coreBurstLen))
  val ocpMasterAdapter = Module(new CacheMemToOcpBurstMasterAdapter(addrWidth, memDataWidth, memBurstLen))
  val ocpCoreAdapter = Module(new OcpCoreSlaveToSchedulerAdapter(1, cache.schedulerDataWidth))

  ocpCoreAdapter.io.core <> io.scheduler
  cache.io.scheduler <> ocpCoreAdapter.io.scheduler

  cache.io.inCoreId := 0.U
  ocpSlaveAdapter.io.ocpBurst <> io.core
  cache.io.core <> ocpSlaveAdapter.io.corePort

  cache.io.mem <> ocpMasterAdapter.io.cache
  io.mem <> ocpMasterAdapter.io.ocpBurst
}
