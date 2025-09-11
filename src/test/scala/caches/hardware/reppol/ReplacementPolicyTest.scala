package caches.hardware.reppol

import chisel3._
import chiseltest._

object ReplacementPolicyTest {
  def setCoreAsCritical(dut: SharedCacheReplacementPolicyType, coreID: Int, wData: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.WR)
    dut.io.scheduler.addr.poke(coreID.U)
    dut.io.scheduler.wData.poke(wData.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }

  def unsetCoreAsCritical(dut: SharedCacheReplacementPolicyType, coreID: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.scheduler.addr.poke(coreID.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
  }

  def performEvictionRequest(dut: SharedCacheReplacementPolicyType, coreId: Int, setIdx: Int, expectedEvictionCandidate: Option[Int] = None): Unit = {
    val expectedCandidate = expectedEvictionCandidate match {
      case Some(data) => data.U
      case None => 0.U
    }

    val evictionSetEmpty = expectedEvictionCandidate match {
      case Some(_) => true.B
      case None => false.B
    }

    // Make a request on behalf of the requesting core
    dut.io.control.setIdx.poke(setIdx.U)
    dut.io.control.coreId.poke(coreId.U)

    dut.clock.step(2) // Need tro clock cycle delay here since the base policy is separated from contention logic by two pipeline stage

    dut.io.control.evict.poke(true.B)
    dut.io.control.coreId.poke(coreId.U)

    dut.io.control.isValid.expect(evictionSetEmpty)
    if(expectedEvictionCandidate.isDefined) {
      dut.io.control.replaceWay.expect(expectedCandidate)
    }

    dut.clock.step(1)

    dut.io.control.coreId.poke(0.U)
    dut.io.control.evict.poke(false.B)
    dut.io.control.setIdx.poke(0.U)
  }

  def performUpdateRequest(dut: SharedCacheReplacementPolicyType, coreId: Int, setIdx: Int, hitWay: Int): Unit = {
    dut.io.control.setIdx.poke(setIdx.U)
    dut.io.control.coreId.poke(coreId.U)

    dut.clock.step()

    dut.io.control.setIdx.poke(0.U)
    dut.io.control.coreId.poke(0.U)

    dut.clock.step()

    dut.io.control.update.valid.poke(true.B)
    dut.io.control.update.bits.poke(hitWay.U)
    dut.io.control.updateCoreId.poke(coreId.U)

    dut.clock.step()

    dut.io.control.update.valid.poke(false.B)
    dut.io.control.update.bits.poke(0.U)
    dut.io.control.updateCoreId.poke(0.U)
  }

  def defaultAssignments(dut: SharedCacheReplacementPolicyType): Unit = {
    dut.io.control.update.valid.poke(false.B)
    dut.io.control.update.bits.poke(0.U)
    dut.io.control.updateCoreId.poke(0.U)
    dut.io.control.setIdx.poke(0.U)
    dut.io.control.coreId.poke(0.U)
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }
}
