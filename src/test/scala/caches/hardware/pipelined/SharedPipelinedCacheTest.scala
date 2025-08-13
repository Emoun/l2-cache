package caches.hardware.pipelined

import caches.hardware.reppol._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

case class CacheRequest(
                         coreId: Int,
                         reqId: Int,
                         addr: String,
                         rw: Boolean,
                         expectedData: String,
                         isExpectedMiss: Boolean = false,
                         isExpectedWb: Boolean = false,
                         wData: Option[String] = None,
                         okResponse: Boolean = true,
                         responseCC: Int = 0,
                         memoryEntryCC: Int = 0
                       )

class SharedPipelinedCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  def setCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreID: Int, contentionLimit: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.WR)
    dut.io.scheduler.addr.poke(coreID.U)
    dut.io.scheduler.wData.poke(contentionLimit.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
    dut.io.scheduler.wData.poke(0.U)
  }

  def unsetCoreAsCritical(dut: SharedPipelinedCacheTestTop, coreID: Int): Unit = {
    dut.io.scheduler.cmd.poke(SchedulerCmd.RD)
    dut.io.scheduler.addr.poke(coreID.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
    dut.io.scheduler.addr.poke(0.U)
  }

  def assertAccesses(
                      dut: SharedPipelinedCacheTestTop,
                      instructions: Array[CacheRequest],
                      hitLatency: Int,
                      fullMissLatency: Int,
                      stalledMissLatency: Int,
                      memAccessLatency: Int,
                      entryToWbLatency: Int,
                      printInfo: Boolean = false
                    ): Unit = {
    var currentCC = 0
    var instructionIdx = 0
    var done = false
    var lastInstrIssued = false

    var issuedRequests = mutable.Set[CacheRequest]()

    while (!done) {
      val currReq = instructions(instructionIdx)

      val coreAlreadyHasPendingReq = issuedRequests.exists(req => req.coreId === currReq.coreId)
      val firstExpectedResponse = if (issuedRequests.nonEmpty) Some(issuedRequests.minBy(req => req.responseCC)) else None

      // Check if we need to expect a request in the current CC
      if (firstExpectedResponse.isDefined) {
        val firstExpectedResponseValue = firstExpectedResponse.get

        if (firstExpectedResponseValue.responseCC === currentCC) {
          expectCacheResponse(dut, coreId = firstExpectedResponseValue.coreId, reqId = firstExpectedResponseValue.reqId, expectedData = firstExpectedResponseValue.expectedData, rw = firstExpectedResponseValue.rw, okResponse = firstExpectedResponseValue.okResponse)

          // Remove the request after we have received the response
          issuedRequests.remove(firstExpectedResponseValue)
          if (printInfo) println(s"Received response for request ${firstExpectedResponseValue.reqId} for core ${firstExpectedResponseValue.coreId} at CC: $currentCC.")
        }
      }

      val issueReq = !coreAlreadyHasPendingReq && !lastInstrIssued

      def updateRequestsUponWB(requests: mutable.Set[CacheRequest], memAccessLatency: Int, wbEntryCC: Int): mutable.Set[CacheRequest] = {
        // Find all misses after the WB and add the WB latency to them
        requests.map(req => if (req.memoryEntryCC > wbEntryCC) req.copy(responseCC = req.responseCC + memAccessLatency) else req)
      }

      // Check if the current request can be issued: the corresponding core has no pending requests
      if (issueReq) {
        performCacheRequest(dut, coreId = currReq.coreId, reqId = currReq.reqId, addr = currReq.addr, rw = currReq.rw, wData = currReq.wData)

        // If we issue a request we add the request to the issuedRequest queue and compute it's expected response time
        // If it is a miss the delay will depend on other previous misses
        var respCC: Int = (currReq.isExpectedWb, currReq.isExpectedMiss, issuedRequests.count(req => req.isExpectedMiss) > 0) match {
          case (true, true, true) => issuedRequests.filter(req => req.isExpectedMiss).maxBy(req => req.responseCC).responseCC + stalledMissLatency + memAccessLatency // If Miss under a miss causes a WB
          case (false, true, true) => issuedRequests.filter(req => req.isExpectedMiss).maxBy(req => req.responseCC).responseCC + stalledMissLatency // If Miss under a Miss
          case (false, true, false) => currentCC + fullMissLatency // If first miss
          case (_, false, _) => currentCC + hitLatency // If hit
          case (_, _, _) => 0
        }

        // If there is a request already issued with the same response CC, we must delay the current
        // requests response CC by one cycle, since the WB will stall any hit request
        val matchingRequest = issuedRequests.find(req => req.responseCC === respCC)
        if (matchingRequest.isDefined) {
          respCC += 1
        }

        if (currReq.isExpectedWb) {
          issuedRequests = updateRequestsUponWB(issuedRequests, memAccessLatency = memAccessLatency, wbEntryCC = currentCC + entryToWbLatency)
        }

        if (printInfo) println(s"Pushed request ${currReq.reqId} with expected response CC: $respCC for core ${currReq.coreId}.")
        issuedRequests.add(currReq.copy(responseCC = respCC, memoryEntryCC = respCC - memAccessLatency))

        if (instructionIdx === instructions.length - 1) {
          lastInstrIssued = true
        }

        if (instructionIdx < instructions.length - 1) {
          instructionIdx = instructionIdx + 1
        }
      }

      // If we did not issue a request, and we still have pending requests we step to the first expected response
      val clockSteps = (issueReq, firstExpectedResponse.isDefined) match {
        case (false, true) => firstExpectedResponse.get.responseCC - currentCC
        case (_, _) => 0
      }

      dut.clock.step(clockSteps)

      // Either move on to issuing a new request or step to the CC of the first expected response
      currentCC = if (!issueReq && firstExpectedResponse.isDefined) {
        firstExpectedResponse.get.responseCC
      } else {
        currentCC + 1
      }

      if (printInfo) println()

      if (instructionIdx === (instructions.length - 1) && issuedRequests.isEmpty) {
        done = true
      }
    }
  }

  def performCacheRequest(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: String, rw: Boolean, wData: Option[String] = None, byteEn: Option[String] = None): Unit = {
    // Expect the cache to be ready to accept a request
    dut.io.requests.cores(coreId).req.reqId.ready.expect(true.B)

    // Make request on behalf of the second core
    dut.io.requests.cores(coreId).req.reqId.valid.poke(true.B)
    dut.io.requests.cores(coreId).req.reqId.bits.poke(reqId.U)
    dut.io.requests.cores(coreId).req.addr.poke(addr.U)
    dut.io.requests.cores(coreId).req.rw.poke(rw.B)

    val wDataValue = wData match {
      case Some(data) => data.U
      case None => 0.U
    }

    val byteEnValue = byteEn match {
      case Some(data) => data.U
      case None => (math.pow(2, dut.subBlockDataWidth / 8).toInt - 1).U
    }

    dut.io.requests.cores(coreId).req.wData.poke(wDataValue)
    dut.io.requests.cores(coreId).req.byteEn.poke(byteEnValue)

    // TODO: Extend this function to handle the case when the arbiter is not ready to accept the request
    dut.clock.step(1)

    dut.io.requests.cores(coreId).req.reqId.valid.poke(false.B)
    dut.io.requests.cores(coreId).req.reqId.bits.poke(0.U)
    dut.io.requests.cores(coreId).req.addr.poke(0.U)
    dut.io.requests.cores(coreId).req.rw.poke(false.B)
    dut.io.requests.cores(coreId).req.wData.poke(0.U)
  }

  def expectCacheResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, expectedData: String, rw: Boolean = false, okResponse: Boolean = true): Unit = {
    if(!rw) dut.io.requests.cores(coreId).resp.reqId.valid.expect(true.B, s"Did not receive a response for request: $reqId.")
    dut.io.requests.cores(coreId).resp.reqId.bits.expect(reqId.U, s"Did not receive a response for request: $reqId.")
    dut.io.requests.cores(coreId).resp.rData.expect(expectedData.U, s"Did not receive correct data for a request: $reqId.")
    dut.io.requests.cores(coreId).resp.responseStatus.expect(okResponse.asBool, s"Did receive correct response for a request: $reqId.")
  }

  "SharedPipelinedCache" should "work with lru replacement policy and 8 ways and 4 sets" in {
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

    val fullMissLatency = 11
    val hitLatency = 4

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = bytesPerSubBlock,
      memBurstLen = bytesPerBlock / bytesPerSubBlock,
      l2RepPolicy,
      Some(memFile)
    )).withAnnotations(Seq(WriteVcdAnnotation, WriteFstAnnotation)) { dut =>
      // Default inputs
      for (i <- 0 until nCores) {
        dut.io.requests.cores(i).req.reqId.valid.poke(false.B)
        dut.io.requests.cores(i).req.reqId.bits.poke(0.U)
        dut.io.requests.cores(i).req.addr.poke(0.U)
        dut.io.requests.cores(i).req.rw.poke(false.B)
        dut.io.requests.cores(i).req.wData.poke(0.U)
      }

      dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
      dut.io.scheduler.addr.poke(0.U)

      dut.clock.step(5)

      // NOTE:
      //  addr is assumed to be byte addressable
      //  first two bits in addr are for byte offset
      //  second two bits in addr are for block offset
      //  third two bits in addr are for index
      //  remaining bits are for the tag

      // Access data that is not yet in the cache (put in way: 0)
      performCacheRequest(dut, coreId = 1, reqId = 0, addr = "b000000000", rw = false)
      dut.clock.step(fullMissLatency - 1) // Pipeline delay due to cache miss
      expectCacheResponse(dut, coreId = 1, reqId = 0, expectedData = "hbeefdead")

      // Access data that is already in the cache
      performCacheRequest(dut, coreId = 1, reqId = 1, addr = "b000000100", rw = false)
      dut.clock.step(hitLatency - 1)
      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hdeadbeef")

      // Write to an existing line in the cache
      performCacheRequest(dut, coreId = 1, reqId = 2, addr = "b000001000", rw = true, wData = Some("hd00dfeed"), byteEn = Some("b1010"))
      dut.clock.step(hitLatency - 1)
//      expectCacheResponse(dut, coreId = 1, reqId = 2, expectedData = "hbabecafe")

      // Bring in another line into the cache (put in way: 1)
      performCacheRequest(dut, coreId = 3, reqId = 3, addr = "b001001100", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 3, reqId = 3, expectedData = "hbbadbeef")

      // Bring in another line into the cache (put in way: 2)
      performCacheRequest(dut, coreId = 2, reqId = 4, addr = "b010000000", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 2, reqId = 4, expectedData = "hfacefeed")

      // Bring in another line into the cache (put in way: 3) and write some data to it too
      performCacheRequest(dut, coreId = 3, reqId = 5, addr = "b011001100", rw = true, wData = Some("hbeefdead"))
      dut.clock.step(fullMissLatency - 1)
//      expectCacheResponse(dut, coreId = 3, reqId = 5, expectedData = "hbd42f9c3")

      // Bring in another line into the cache (put in way: 4)
      performCacheRequest(dut, coreId = 0, reqId = 6, addr = "b100000000", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 0, reqId = 6, expectedData = "h40cbde98")

      // Bring in another line into the cache (put in way: 5)
      performCacheRequest(dut, coreId = 1, reqId = 7, addr = "b101000100", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 1, reqId = 7, expectedData = "haf10c5be")

      // Bring in another line into the cache (put in way: 6)
      performCacheRequest(dut, coreId = 2, reqId = 8, addr = "b110001000", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 2, reqId = 8, expectedData = "hace04f29")

      // Bring in another line into the cache (put in way: 7)
      performCacheRequest(dut, coreId = 3, reqId = 9, addr = "b111001100", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 3, reqId = 9, expectedData = "hf01c27ae")

      // Bring in the first cache line that will result in eviction of way 0 (put in way: 0)
      performCacheRequest(dut, coreId = 0, reqId = 10, addr = "b1000000100", rw = false)
      dut.clock.step(fullMissLatency - 1)
      expectCacheResponse(dut, coreId = 0, reqId = 10, expectedData = "h49e1af73")

      // Try to refetch the evicted cache and check if the written data was written to main memory
      // (put in way: 1)
      performCacheRequest(dut, coreId = 1, reqId = 11, addr = "b000001000", rw = false)
      dut.clock.step(15) // Since a write back starts at the same time as the request we have to wait for a wb before we can fetch the line back
      expectCacheResponse(dut, coreId = 1, reqId = 11, expectedData = "hd00dfeed")
    }
  }

  "SharedPipelinedCache" should "work with lru replacement policy and 4 ways 256 sets" in {
    val sizeInBytes = 32768
    val nCores = 4
    val nWays = 4
    val addressWidth = 16
    val reqIdWidth = 6
    val bytesPerBlock = 32
    val bytesPerSubBlock = 4
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val l2RepPolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val memFile = "./hex/test_mem_32w.hex"

    val fullMissLatency = 11
    val hitLatency = 4

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = bytesPerSubBlock,
      memBurstLen = bytesPerBlock / bytesPerSubBlock,
      l2RepPolicy,
      Some(memFile)
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default inputs
      for (i <- 0 until nCores) {
        dut.io.requests.cores(i).req.reqId.valid.poke(false.B)
        dut.io.requests.cores(i).req.reqId.bits.poke(0.U)
        dut.io.requests.cores(i).req.addr.poke(0.U)
        dut.io.requests.cores(i).req.rw.poke(false.B)
        dut.io.requests.cores(i).req.wData.poke(0.U)
      }

      dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
      dut.io.scheduler.addr.poke(0.U)

      dut.clock.step(5)

      // NOTE:
      //  addr is assumed to be byte addressable
      //  first two bits in addr are for byte offset
      //  second two bits in addr are for block offset
      //  third three bits in addr are for index
      //  remaining bits are for the tag

      // MISS: WAY - 0, INDEX - 0, TAG - 0
      performCacheRequest(dut, coreId = 1, reqId = 0, addr = "b000000000000", rw = false)
      dut.clock.step(fullMissLatency - 1) // Pipeline delay due to cache miss
      expectCacheResponse(dut, coreId = 1, reqId = 0, expectedData = "hbeefdead")

      // MISS: WAY - 0, INDEX - 79, TAG - 3
      performCacheRequest(dut, coreId = 3, reqId = 1, addr = "b11010011111100", rw = true, wData = Some("hfaceb00c"))
//      dut.clock.step(fullMissLatency - 1)
//      expectCacheResponse(dut, coreId = 3, reqId = 9, expectedData = "hf01c27ae")

      // Access data that is already in the cache (HIT)
      performCacheRequest(dut, coreId = 1, reqId = 2, addr = "b00000000000100", rw = false)
//      dut.clock.step(hitLatency - 1)
//      expectCacheResponse(dut, coreId = 1, reqId = 1, expectedData = "hdeadbeef")

      // Write to an existing line in the cache (HIT)
      performCacheRequest(dut, coreId = 1, reqId = 3, addr = "b000000001000", rw = true, wData = Some("hd00dfeed"))
//      dut.clock.step(hitLatency - 1)
//      expectCacheResponse(dut, coreId = 1, reqId = 2, expectedData = "hbabecafe")

      // MISS: WAY - 0, INDEX - 45, TAG - 2
      performCacheRequest(dut, coreId = 3, reqId = 4, addr = "b10001011010100", rw = true, wData = Some("hfaceb00c"))
//      dut.clock.step(fullMissLatency - 1)

      // MISS: WAY - 0, INDEX - 159, TAG - 5
      performCacheRequest(dut, coreId = 2, reqId = 5, addr = "b101100111111000", rw = true, wData = Some("hcafeface"))
      dut.clock.step(fullMissLatency)

      // MISS: WAY - 0, INDEX - 255, TAG - 8
      performCacheRequest(dut, coreId = 0, reqId = 6, addr = "b1000111111111100", rw = true, wData = Some("hb00cface"))
//      dut.clock.step(fullMissLatency - 1)

      // MISS: WAY - 1, INDEX - 255, TAG - 5
      performCacheRequest(dut, coreId = 2, reqId = 7, addr = "b101111111110100", rw = true, wData = Some("hd15ea555"))
//      dut.clock.step(fullMissLatency - 1)

      // MISS: WAY - 2, INDEX - 255, TAG - 7
      performCacheRequest(dut, coreId = 3, reqId = 8, addr = "b101111111110100", rw = false)
      dut.clock.step(fullMissLatency)

      // MISS: WAY - 3, INDEX - 255, TAG - 13
      performCacheRequest(dut, coreId = 1, reqId = 9, addr = "b1101111111111100", rw = true, wData = Some("hfee1dead"))

      dut.clock.step(fullMissLatency)

      // MISS AND EVICT: WAY - 0, INDEX - 255, TAG - 11
      performCacheRequest(dut, coreId = 0, reqId = 10, addr = "b1011111111110100", rw = true, wData = Some("hdeadfee1"))

      dut.clock.step(40)

      // MISS AND EVICT: WAY - 0, INDEX - 255, TAG - 11
      performCacheRequest(dut, coreId = 0, reqId = 11, addr = "b1011111111111000", rw = false)
      dut.clock.step(5)

//      performCacheRequest(dut, coreId = 3, reqId = 7, addr = "b101 11111111 0100", rw = true, wData = Some("hb00cface"))
    }
  }

  "SharedPipelinedCache" should "work with contention replacement policy and 8 ways with pipelined requests" in {
    val sizeInBytes = 512
    val nCores = 4
    val nWays = 8
    val reqIdWidth = 16
    val addressWidth = 16
    val bytesPerBlock = 16
    val bytesPerSubBlock = 4
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val basePolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val l2RepPolicy = () => new ContentionReplacementPolicy(nWays, nSets, nCores, basePolicy)
    val memFile = "./hex/test_mem_32w.hex"

    val fullMissLatency = 11
    val stalledMissLatency = 8
    val hitLatency = 4
    val memAccessLatency = 8
    val entryToWbLatency = 4

    val printInfo = true

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = bytesPerSubBlock,
      memBurstLen = bytesPerBlock / bytesPerSubBlock,
      l2RepPolicy,
      Some(memFile)
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default inputs
      for (i <- 0 until nCores) {
        dut.io.requests.cores(i).req.reqId.valid.poke(false.B)
        dut.io.requests.cores(i).req.reqId.bits.poke(0.U)
        dut.io.requests.cores(i).req.addr.poke(0.U)
        dut.io.requests.cores(i).req.rw.poke(false.B)
        dut.io.requests.cores(i).req.wData.poke(0.U)
      }

      dut.io.scheduler.cmd.poke(SchedulerCmd.NULL)
      dut.io.scheduler.addr.poke(0.U)
      dut.io.scheduler.wData.poke(0.U)

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

      val firstSetOfInstructions = Array(
        CacheRequest(coreId = 1, reqId = 0, addr = "b0000000000", rw = false, expectedData = "hbeefdead", isExpectedMiss = true), // Bring new line into the cache (put in way: 0, idx: 0)
        CacheRequest(coreId = 1, reqId = 1, addr = "b0001000000", rw = false, expectedData = "hbadc0ffe", isExpectedMiss = true), // Bring new line into the cache (put in way: 1, idx: 0)
        CacheRequest(coreId = 1, reqId = 2, addr = "b0010000000", rw = false, expectedData = "hfacefeed", isExpectedMiss = true), // Bring new line into the cache (put in way: 2, idx: 0)
        CacheRequest(coreId = 0, reqId = 3, addr = "b0100000000", rw = false, expectedData = "h40cbde98", isExpectedMiss = true), // Bring new line into the cache (put in way: 3, idx: 0)
        CacheRequest(coreId = 1, reqId = 4, addr = "b0000000100", rw = false, expectedData = "hdeadbeef"),
        CacheRequest(coreId = 1, reqId = 5, addr = "b0000001000", rw = true, expectedData = "hbabecafe", wData = Some("hd00dfeed")),
        CacheRequest(coreId = 1, reqId = 6, addr = "b0001000100", rw = false, expectedData = "he0ddf00d"),
        CacheRequest(coreId = 1, reqId = 7, addr = "b0010001100", rw = false, expectedData = "hbeefcace"),
        CacheRequest(coreId = 1, reqId = 8, addr = "b0101000100", rw = false, expectedData = "haf10c5be", isExpectedMiss = true), // Bring new line into the cache (put in way: 4, idx: 0)
        CacheRequest(coreId = 2, reqId = 9, addr = "b0110001000", rw = false, expectedData = "hace04f29", isExpectedMiss = true), // Bring new line into the cache (put in way: 5, idx: 0)
        CacheRequest(coreId = 3, reqId = 10, addr = "b0111001100", rw = false, expectedData = "hf01c27ae", isExpectedMiss = true), // Bring new line into the cache (put in way: 6, idx: 0)
        CacheRequest(coreId = 3, reqId = 11, addr = "b0011001100", rw = false, expectedData = "hbd42f9c3", isExpectedMiss = true), // Bring new line into the cache (put in way: 7, idx: 0)

        CacheRequest(coreId = 1, reqId = 12, addr = "b000111000", rw = false, expectedData = "h10ccdead", isExpectedMiss = true), // Bring new line into the cache (put in way: 0, idx: 3)
        CacheRequest(coreId = 2, reqId = 13, addr = "b000101100", rw = false, expectedData = "hfee1dead", isExpectedMiss = true), // Bring new line into the cache (put in way: 0, idx: 2)

        CacheRequest(coreId = 3, reqId = 14, addr = "b1000000100", rw = false, expectedData = "h49e1af73", isExpectedMiss = true, isExpectedWb = true), // Bring new line into the cache (put in way: 0, idx: 0)
        CacheRequest(coreId = 0, reqId = 15, addr = "b1001001100", rw = false, expectedData = "h5ca1ab1e", isExpectedMiss = true), // Bring new line into the cache (put in way: 1, idx: 0), reach contention limit for core 1
        CacheRequest(coreId = 3, reqId = 16, addr = "b0000001000", rw = false, expectedData = "hd00dfeed", isExpectedMiss = true), // Bring new line into the cache (put in way: 3, idx: 0),
        CacheRequest(coreId = 1, reqId = 17, addr = "b1010000100", rw = false, expectedData = "hfaceb00c", isExpectedMiss = true), // Bring new line into the cache (put in way: 5, idx: 0),

        CacheRequest(coreId = 1, reqId = 18, addr = "b0010110100", rw = false, expectedData = "h0fabc987", isExpectedMiss = true), // Bring new line into the cache (put in way: 1, idx: 3),

        CacheRequest(coreId = 1, reqId = 19, addr = "b1011001100", rw = false, expectedData = "hcafef00d", isExpectedMiss = true), // Bring new line into the cache (put in way: 6, idx: 0),
        CacheRequest(coreId = 1, reqId = 20, addr = "b1100000000", rw = false, expectedData = "hdeafbabe", isExpectedMiss = true), // Bring new line into the cache (put in way: 7, idx: 0), reach contention limit for core 3
        CacheRequest(coreId = 3, reqId = 21, addr = "b1101000100", rw = false, expectedData = "hca11fade", isExpectedMiss = true), // Bring new line into the cache (put in way: 1, idx: 0),
        CacheRequest(coreId = 2, reqId = 22, addr = "b1110000100", rw = false, expectedData = "h49e1af73", okResponse = false), // Perform an eviction request by a non-critical core, expect rejection
        CacheRequest(coreId = 1, reqId = 23, addr = "b0100000000", rw = false, expectedData = "h40cbde98", isExpectedMiss = true), // Bring new line into the cache (put in way: 0, idx: 0), expect a critical core to evict another critical core
      )

      assertAccesses(dut, firstSetOfInstructions, hitLatency, fullMissLatency, stalledMissLatency, memAccessLatency, entryToWbLatency, printInfo)

      dut.clock.step(1)

      unsetCoreAsCritical(dut, coreID = 1)

      dut.clock.step(1)

      val secondSetOfInstructions = Array(
        CacheRequest(coreId = 3, reqId = 24, addr = "b1000110000", rw = false, expectedData = "hbaddc0de", isExpectedMiss = true), // Bring new line into the cache (put in way: 2, idx: 3)
        CacheRequest(coreId = 3, reqId = 25, addr = "b0001011000", rw = false, expectedData = "hea5e0d15", isExpectedMiss = true), // Bring new line into the cache (put in way: 0, idx: 1)
        CacheRequest(coreId = 3, reqId = 26, addr = "b0010110100", rw = false, expectedData = "h0fabc987"),

        CacheRequest(coreId = 0, reqId = 27, addr = "b0110000000", rw = false, expectedData = "h39be471d", isExpectedMiss = true), // Bring new line into the cache (put in way: 2, idx: 0)
        CacheRequest(coreId = 2, reqId = 28, addr = "b0001001000", rw = false, expectedData = "hbeefbbad", isExpectedMiss = true), // Bring new line into the cache (put in way: 4, idx: 0)
      )

      // Evict some previously critical caches
      assertAccesses(dut, secondSetOfInstructions, hitLatency, fullMissLatency, stalledMissLatency, memAccessLatency, entryToWbLatency, printInfo)
    }
  }
}
