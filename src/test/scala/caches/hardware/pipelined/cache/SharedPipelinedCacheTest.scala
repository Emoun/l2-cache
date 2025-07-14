package caches.hardware.pipelined.cache

import chisel3._
import chiseltest._
import caches.hardware.reppol._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  def setCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreID: Int, contentionLimit: Int): Unit = {
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

  def unsetCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreID: Int): Unit = {
    dut.io.scheduler.coreId.valid.poke(true.B)
    dut.io.scheduler.coreId.bits.poke(coreID.U)
    dut.io.scheduler.unsetCritical.poke(true.B)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.coreId.valid.poke(false.B)
    dut.io.scheduler.coreId.bits.poke(0.U)
    dut.io.scheduler.unsetCritical.poke(false.B)
  }

  def performCacheRequest(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: String, rw: Boolean, wData: Option[String] = None): Unit = {
    // Expect the cache to be ready to accept a request
    dut.io.cache.coreReqs(coreId).reqId.ready.expect(true.B)

    // Make request on behalf of the second core
    dut.io.cache.coreReqs(coreId).reqId.valid.poke(true.B)
    dut.io.cache.coreReqs(coreId).reqId.bits.poke(reqId.U)
    dut.io.cache.coreReqs(coreId).addr.poke(addr.U)
    dut.io.cache.coreReqs(coreId).rw.poke(rw.B)

    val wDataValue = wData match {
      case Some(data) => data.U
      case None => 0.U
    }

    dut.io.cache.coreReqs(coreId).wData.poke(wDataValue)

    // TODO: Extend this function to handle the case when the arbiter is not ready to accept the request
    dut.clock.step(1)

    dut.io.cache.coreReqs(coreId).reqId.valid.poke(false.B)
    dut.io.cache.coreReqs(coreId).reqId.bits.poke(0.U)
    dut.io.cache.coreReqs(coreId).addr.poke(0.U)
    dut.io.cache.coreReqs(coreId).rw.poke(false.B)
    dut.io.cache.coreReqs(coreId).wData.poke(0.U)
  }

  def expectCacheResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, expectedData: String, okResponse: Boolean = true): Unit = {
    dut.io.cache.coreResps(coreId).reqId.valid.expect(true.B)
    dut.io.cache.coreResps(coreId).reqId.bits.expect(reqId.U)
    dut.io.cache.coreResps(coreId).rData.expect(expectedData.U)
    dut.io.cache.coreResps(coreId).responseStatus.expect(okResponse.asBool)

    dut.io.cache.coreResps(coreId).reqId.ready.poke(true.B)

    dut.clock.step(1)

    dut.io.cache.coreResps(coreId).reqId.ready.poke(false.B)
  }

  "SharedPipelinedCache" should "work with lru replacement policy and 8 ways" in  {
    val sizeInBytes = 512
    val nCores = 4
    val nWays = 8
    val addressWidth = 16
    val reqIdWidth = 6
    val bytesPerBlock = 16
    val bytesPerSubBlock = 4
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val l2RepPolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val memFile = "./hex/test_mem_32w.hex"

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      bytesPerBurst = bytesPerSubBlock,
      l2RepPolicy,
      Some(memFile)
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default inputs
      for(i <- 0 until nCores) {
        dut.io.cache.coreReqs(i).reqId.valid.poke(false.B)
        dut.io.cache.coreReqs(i).reqId.bits.poke(0.U)
        dut.io.cache.coreReqs(i).addr.poke(0.U)
        dut.io.cache.coreReqs(i).rw.poke(false.B)
        dut.io.cache.coreReqs(i).wData.poke(0.U)
        dut.io.cache.coreResps(i).reqId.ready.poke(false.B)
      }

      dut.io.scheduler.coreId.valid.poke(false.B)
      dut.io.scheduler.coreId.bits.poke(0.U)
      dut.io.scheduler.setCritical.poke(false.B)
      dut.io.scheduler.unsetCritical.poke(false.B)
      dut.io.scheduler.contentionLimit.poke(0.U)

      dut.clock.step(5)

      // NOTE:
      //  addr is assumed to be byte addressable
      //  first two bits in addr are for byte offset
      //  second two bits in addr are for block offset
      //  third two bits in addr are for index
      //  remaining bits are for the tag

      // Access data that is not yet in the cache (put in way: 0)
      performCacheRequest(dut, coreId = 1, reqId = 0, addr = "b000000000", rw = false)
      dut.clock.step(8) // Pipeline delay due to cache miss
      expectCacheResponse(dut, coreId = 1, reqId = 0, expectedData = "hbeefdead")

      // Access data that is already in the cache
      performCacheRequest(dut, coreId = 1, reqId = 1, addr = "b000000100", rw = false)
      dut.clock.step(2)
      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hdeadbeef")

      // Write to an existing line in the cache
      performCacheRequest(dut, coreId = 1, reqId = 2, addr = "b000001000", rw = true, wData = Some("hd00dfeed"))
      dut.clock.step(2)
      expectCacheResponse(dut, coreId = 1, reqId = 2, expectedData = "hbabecafe")

      // Bring in another line into the cache (put in way: 1)
      performCacheRequest(dut, coreId = 3, reqId = 0, addr = "b001001100", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 3, reqId = 0, expectedData = "hbbadbeef")

      // Bring in another line into the cache (put in way: 2)
      performCacheRequest(dut, coreId = 2, reqId = 0, addr = "b010000000", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 2, reqId = 0, expectedData = "hfacefeed")

      // Bring in another line into the cache (put in way: 3) and write some data to it too
      performCacheRequest(dut, coreId = 3, reqId = 1, addr = "b011001100", rw = true, wData = Some("hbeefdead"))
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 3, reqId = 1, expectedData = "hbd42f9c3")

      // Bring in another line into the cache (put in way: 4)
      performCacheRequest(dut, coreId = 0, reqId = 0, addr = "b100000000", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 0, reqId = 0, expectedData = "h40cbde98")

      // Bring in another line into the cache (put in way: 5)
      performCacheRequest(dut, coreId = 1, reqId = 3, addr = "b101000100", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 1, reqId = 3, expectedData = "haf10c5be")

      // Bring in another line into the cache (put in way: 6)
      performCacheRequest(dut, coreId = 2, reqId = 1, addr = "b110001000", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 2, reqId = 1, expectedData = "hace04f29")

      // Bring in another line into the cache (put in way: 7)
      performCacheRequest(dut, coreId = 3, reqId = 2, addr = "b111001100", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 3, reqId = 2, expectedData = "hf01c27ae")

      // Bring in the first cache line that will result in eviction of way 0 (put in way: 0)
      performCacheRequest(dut, coreId = 0, reqId = 1, addr = "b1000000100", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 0, reqId = 1, expectedData = "h49e1af73")

      // Try to refetch the evicted cache and check if the written data was written to main memory
      // (put in way: 1)
      performCacheRequest(dut, coreId = 1, reqId = 1, addr = "b000001000", rw = false)
      dut.clock.step(12) // Since a write back starts at the same time as the request we have to wait for a wb before we can fetch the line back
      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hd00dfeed")
    }
  }

  "SharedPipelinedCache" should "work with contention replacement policy and 8 ways" in  {
    val sizeInBytes = 512
    val nCores = 4
    val nWays = 8
    val reqIdWidth = 4
    val addressWidth = 16
    val bytesPerBlock = 16
    val bytesPerSubBlock = 4
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val basePolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val l2RepPolicy = () => new ContentionReplacementPolicy(nWays, nSets, nCores, basePolicy)
    val memFile = "./hex/test_mem_32w.hex"

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      bytesPerBurst = bytesPerSubBlock,
      l2RepPolicy,
      Some(memFile)
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default inputs
      for(i <- 0 until nCores) {
        dut.io.cache.coreReqs(i).reqId.valid.poke(false.B)
        dut.io.cache.coreReqs(i).reqId.bits.poke(0.U)
        dut.io.cache.coreReqs(i).addr.poke(0.U)
        dut.io.cache.coreReqs(i).rw.poke(false.B)
        dut.io.cache.coreReqs(i).wData.poke(0.U)
        dut.io.cache.coreResps(i).reqId.ready.poke(true.B)
      }

      dut.io.scheduler.coreId.valid.poke(false.B)
      dut.io.scheduler.coreId.bits.poke(0.U)
      dut.io.scheduler.setCritical.poke(false.B)
      dut.io.scheduler.unsetCritical.poke(false.B)
      dut.io.scheduler.contentionLimit.poke(0.U)

      dut.clock.step(5)

      setCoreAsCritical(dut, coreID = 1, contentionLimit = 2)

      dut.clock.step(1)

      setCoreAsCritical(dut, coreID = 3, contentionLimit = 2)

      dut.clock.step(1)

      // NOTE:
      //  addr is assumed to be byte addressable
      //  first two bits in addr are for byte offset
      //  second two bits in addr are for block offset
      //  third two bits in addr are for index
      //  remaining bits are for the tag

      // Access data that is not yet in the cache (put in way: 0)
      performCacheRequest(dut, coreId = 1, reqId = 1, addr = "b00000000", rw = false)
      dut.clock.step(8) // Pipeline delay due to cache miss
      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hbeefdead")

      // Access data that is already in the cache
      performCacheRequest(dut, coreId = 1, reqId = 1, addr = "b00000100", rw = false)
      dut.clock.step(2)
      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hdeadbeef")

      // Write to an existing line in the cache
      performCacheRequest(dut, coreId = 1, reqId = 1, addr = "b00001000", rw = true, wData = Some("hd00dfeed"))
      dut.clock.step(2)
      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hbabecafe")

      // Bring in another line into the cache (put in way: 1)
      performCacheRequest(dut, coreId = 2, reqId = 2, addr = "b01000000", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 2, reqId = 2, expectedData = "hbadc0ffe")

      // Bring in another line into the cache (put in way: 2)
      performCacheRequest(dut, coreId = 2, reqId = 2, addr = "b10000000", rw = false)
      dut.clock.step(8)
      expectCacheResponse(dut, coreId = 2, reqId = 2, expectedData = "hfacefeed")
    }
  }
}
