package caches.hardware.reppol

import chisel3._
import chiseltest._
import firrtl.options.ProgramArgsAnnotation
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class ContentionReplacementPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  def setCoreAsCritical(dut: ContentionReplacementPolicy, coreID: Int, contentionLimit: Int): Unit = {
    dut.io.scheduler.coreId.valid.poke(true.B)
    dut.io.scheduler.coreId.bits.poke(coreID.U)
    dut.io.scheduler.setCritical.poke(true.B)
    dut.io.scheduler.contentionLimit.poke(contentionLimit.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.coreId.valid.poke(false.B)
    dut.io.scheduler.coreId.bits.poke(0.U)
    dut.io.scheduler.setCritical.poke(false.B)
    dut.io.scheduler.contentionLimit.poke(0.U)
  }

  def unsetCoreAsCritical(dut: ContentionReplacementPolicy, coreID: Int): Unit = {
    dut.io.scheduler.coreId.valid.poke(true.B)
    dut.io.scheduler.coreId.bits.poke(coreID.U)
    dut.io.scheduler.unsetCritical.poke(true.B)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.coreId.valid.poke(false.B)
    dut.io.scheduler.coreId.bits.poke(0.U)
    dut.io.scheduler.unsetCritical.poke(false.B)
  }

  def performEvictionRequest(dut: ContentionReplacementPolicy, coreId: Int, setIdx: Int, expectedEvictionCandidate: Option[Int] = None): Unit = {
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
    dut.io.control.evict.poke(true.B)
    dut.io.control.reqId.poke(coreId.U)

    dut.io.control.replaceWay.expect(expectedCandidate)
    dut.io.control.isValid.expect(evictionSetEmpty)

    dut.clock.step(1)

    dut.io.control.reqId.poke(0.U)
    dut.io.control.evict.poke(false.B)
    dut.io.control.setIdx.poke(0.U)
  }

  def performUpdateRequest(dut: ContentionReplacementPolicy, coreId: Int, setIdx: Int, hitWay: Int): Unit = {
    dut.io.control.setIdx.poke(setIdx.U)
    dut.io.control.update.valid.poke(true.B)
    dut.io.control.update.bits.poke(hitWay.U)
    dut.io.control.reqId.poke(coreId.U)

    dut.clock.step(1)

    dut.io.control.reqId.poke(0.U)
    dut.io.control.update.valid.poke(false.B)
    dut.io.control.update.bits.poke(0.U)
    dut.io.control.setIdx.poke(0.U)
  }

  "ContentionReplacementPolicy" should "reach contention limit for BitPlru" in {
    val (ways, sets, nCores) = (4, 2, 3)
    test(new ContentionReplacementPolicy(ways, sets, nCores, () => new BitPlruReplacementAlgorithm(ways))).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.control.update.valid.poke(false.B)
      dut.io.control.update.bits.poke(0.U)
      dut.io.control.setIdx.poke(0.U)
      dut.io.control.reqId.poke(0.U)
      dut.io.scheduler.coreId.valid.poke(false.B)
      dut.io.scheduler.coreId.bits.poke(0.U)
      dut.io.scheduler.setCritical.poke(false.B)
      dut.io.scheduler.unsetCritical.poke(false.B)
      dut.io.scheduler.contentionLimit.poke(0.U)

      dut.clock.step(1)

      // Expect the policy to output the first way of the base policy as the LRU way
      dut.io.control.replaceWay.expect(0.U)
      dut.io.control.isValid.expect(true.B)

      // Set the first core as critical with a contention limit of 2
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 2)

      dut.clock.step(1)

      // Assign the two ways to second core first
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(2))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 2)

      dut.clock.step(1)

      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 3)

      dut.clock.step(1)

      // Evict critical core lines
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      // Evict critical core lines
      performEvictionRequest(dut, coreId = 0, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 0, setIdx = 0, hitWay = 1)

      dut.clock.step(1)

      // Evict a non-critical line since we now cannot evict the LRU way since it's core has reached a limit, so we evict
      // the second LRU way
      performEvictionRequest(dut, coreId = 2, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 2, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(3))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 3)

      dut.clock.step(1)

      // Expect the critical core can evict non-critical cores
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(0))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 0)

      dut.clock.step(1)

      // Expect that all cores are now assigned to a critical core that has reached capacity
      performEvictionRequest(dut, coreId = 3, setIdx = 0, expectedEvictionCandidate = None)
      dut.clock.step(1)

      // Unset the core as critical
      unsetCoreAsCritical(dut, coreID = 1)

      dut.clock.step(1)

      // Expect that a non-critical core can evict a previously critical core line
      performEvictionRequest(dut, coreId = 1, setIdx = 0, expectedEvictionCandidate = Some(1))
      dut.clock.step(1)
      performUpdateRequest(dut, coreId = 1, setIdx = 0, hitWay = 1)

      dut.clock.step(1)
    }
  }

  "ContentionReplacementPolicy" should "reach contention limit for 4 cores and 8 ways" in {
    val (ways, sets, nCores) = (8, 512, 4)
    test(new ContentionReplacementPolicy(ways, sets, nCores, () => new BitPlruReplacementAlgorithm(ways)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.control.update.valid.poke(false.B)
      dut.io.control.update.bits.poke(0.U)
      dut.io.control.setIdx.poke(0.U)
      dut.io.control.reqId.poke(0.U)
      dut.io.scheduler.coreId.valid.poke(false.B)
      dut.io.scheduler.coreId.bits.poke(0.U)
      dut.io.scheduler.setCritical.poke(false.B)
      dut.io.scheduler.unsetCritical.poke(false.B)
      dut.io.scheduler.contentionLimit.poke(0.U)

      dut.clock.step(1)
    }
  }
}
