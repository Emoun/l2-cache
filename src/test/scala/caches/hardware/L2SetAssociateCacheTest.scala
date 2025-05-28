package caches.hardware

import caches.hardware.reppol.{BitPlruReplacementAlgorithm, ContentionReplacementPolicy, TreePlruReplacementPolicy}
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class L2SetAssociateCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  def performRead(
                   dut: L2SetAssociateCacheTestTop,
                   reqId: Int,
                   readAddress: String,
                   expectedReadData: String,
                   expectRejection: Boolean = false,
                   lowerLevelMissData: Option[String] = None,
                   lowerLevelAccessLatency: Int = 5,
                   expectedReplaceWay: Int = 0,
                   dirtyAddress: Option[String] = None,
                   dirtyData: Option[String] = None
                 ): Unit = {
    dut.io.higher.req.poke(true.B)
    dut.io.higher.reqId.poke(reqId.U)
    dut.io.higher.rw.poke(false.B)
    dut.io.higher.addr.poke(readAddress.U)

    dut.clock.step(1)

    // De-assert a request
    dut.io.higher.req.poke(false.B)
    dut.io.higher.reqId.poke(0.U)
    dut.io.higher.rw.poke(false.B)
    dut.io.higher.addr.poke(0.U)

    if (!expectRejection) {
      if (lowerLevelMissData.isDefined) {
        expectMiss(dut,
          readAddress,
          lowerLevelMissData.get,
          lowerLevelAccessLatency,
          expectedReplaceWay = expectedReplaceWay,
          dirtyAddress = dirtyAddress,
          dirtyData = dirtyData
        )
      }

      expectHit(dut, Some(expectedReadData))
    } else {
      dut.io.higher.ack.expect(true.B)
      dut.io.higher.responseStatus.expect(1.U)
      dut.clock.step(1)
    }
  }

  def expectMiss(
                  dut: L2SetAssociateCacheTestTop,
                  address: String,
                  lowerLevelMissData: String,
                  lowerLevelLatency: Int,
                  expectedReplaceWay: Int,
                  dirtyAddress: Option[String] = None,
                  dirtyData: Option[String] = None
                ): Unit = {
    dut.clock.step(1)

    if (dirtyAddress.isDefined) {
      assert(dirtyData.isDefined, "Dirty data must be provided if expecting a dirty miss.")

      dut.io.lower.req.expect(true.B)
      dut.io.lower.rw.expect(true.B)
      dut.io.lower.addr.expect(dirtyAddress.get.U)
      dut.io.lower.wData.expect(dirtyData.get.U)

      dut.clock.step(lowerLevelLatency) // Latency from lower level memory

      // Inform the cache that the write operation completed successfully
      dut.io.lower.ack.poke(true.B)

      dut.clock.step(1)

      dut.io.lower.ack.poke(false.B) // De-assert ack for fetching a new line
    }

    dut.io.lower.req.expect(true.B)
    dut.io.lower.rw.expect(false.B)
    dut.io.lower.addr.expect(address.U)

    dut.clock.step(lowerLevelLatency) // Latency from lower level memory

    dut.io.dbg.replaceWay.expect(expectedReplaceWay) // Expect a replacement way

    // Inform the cache that the requested data from the lower level is available
    dut.io.lower.rData.poke(lowerLevelMissData.U)
    dut.io.lower.ack.poke(true.B)

    dut.clock.step(1)

    dut.io.lower.ack.poke(false.B)
  }

  def expectHit(dut: L2SetAssociateCacheTestTop, readData: Option[String]): Unit = {
    dut.io.higher.ack.expect(true.B)

    if (readData.isDefined) {
      dut.io.higher.rData.expect(readData.get.U)
    }

    dut.clock.step(1)
  }

  def performWrite(
                    dut: L2SetAssociateCacheTestTop,
                    reqId: Int,
                    writeAddress: String,
                    writeData: String,
                    lowerLevelMissData: Option[String] = None,
                    lowerLevelLatency: Int = 5,
                    expectedReplaceWay: Int = 0,
                    dirtyAddress: Option[String] = None,
                    dirtyData: Option[String] = None
                  ): Unit = {
    dut.io.higher.req.poke(true.B)
    dut.io.higher.reqId.poke(reqId.U)
    dut.io.higher.rw.poke(true.B)
    dut.io.higher.wData.poke(writeData.U)
    dut.io.higher.addr.poke(writeAddress.U)

    dut.clock.step(1)

    // De-assert a request
    dut.io.higher.req.poke(false.B)
    dut.io.higher.reqId.poke(0.U)
    dut.io.higher.rw.poke(false.B)
    dut.io.higher.addr.poke(0.U)

    if (lowerLevelMissData.isDefined) {
      expectMiss(
        dut,
        writeAddress,
        lowerLevelMissData.get,
        lowerLevelLatency,
        expectedReplaceWay = expectedReplaceWay,
        dirtyAddress = dirtyAddress,
        dirtyData = dirtyData
      )
    }

    expectHit(dut, None)
  }

  def assertSetContents(
                         dut: L2SetAssociateCacheTestTop,
                         index: Int,
                         indexOffset: Int,
                         expectedTags: Array[String],
                         expectedData: Array[Array[String]],
                         printContents: Boolean = false
                       ): Unit = {
    dut.io.higher.addr.poke((index << indexOffset).U)

    dut.clock.step(1)

    val setTags = dut.io.dbg.setTags
    for (wayIdx <- setTags.indices) {
      setTags(wayIdx).expect(expectedTags(wayIdx).U)
    }

    val setData = dut.io.dbg.setData
    for (wayIdx <- setData.indices) {
      val block = setData(wayIdx)

      for (wordIdx <- block.indices) {
        block(wordIdx).expect(expectedData(wayIdx)(wordIdx).U)
      }
    }

    if (printContents) printSetContents(dut)
  }

  def printSetContents(dut: L2SetAssociateCacheTestTop): Unit = {
    val setTags = dut.io.dbg.setTags
    val setData = dut.io.dbg.setData
    val tags = setTags.map(_.peekInt()).toArray
    val data = setData.map(_.map(_.peekInt().toString(16)).toArray).toArray

    for (wayIdx <- tags.indices) {
      println(s"Tag for way $wayIdx: ${tags(wayIdx)}")
    }

    println("~~~~~~~~~~~~~~~~~~~~~~")

    for (wayIdx <- data.indices) {
      val block = data(wayIdx)

      for (wordIdx <- block.indices) {
        println(s"Word for way $wayIdx at ($wordIdx): ${block(wordIdx)}")
      }
    }
  }

  def setCoreAsCritical(dut: L2SetAssociateCacheTestTop, coreID: Int, contentionLimit: Int): Unit = {
    dut.io.scheduler.setCritical.valid.poke(true.B)
    dut.io.scheduler.setCritical.bits.poke(coreID.U)
    dut.io.scheduler.contentionLimit.poke(contentionLimit.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.setCritical.valid.poke(false.B)
    dut.io.scheduler.setCritical.bits.poke(0.U)
    dut.io.scheduler.contentionLimit.poke(0.U)
  }

  def unsetCoreAsCritical(dut: L2SetAssociateCacheTestTop, coreID: Int): Unit = {
    dut.io.scheduler.unsetCritical.valid.poke(true.B)
    dut.io.scheduler.unsetCritical.bits.poke(coreID.U)

    dut.clock.step(1)

    // Reset the signals
    dut.io.scheduler.unsetCritical.valid.poke(false.B)
    dut.io.scheduler.unsetCritical.bits.poke(0.U)
  }

  "L2SetAssociateCache" should "decode correct address fields" in {
    val size = 256
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 2
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.reqId.poke(0.U)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)
      dut.io.lower.responseStatus.poke(0.U)
      dut.io.scheduler.setCritical.valid.poke(false.B)
      dut.io.scheduler.setCritical.bits.poke(0.U)
      dut.io.scheduler.contentionLimit.poke(0.U)
      dut.io.scheduler.unsetCritical.valid.poke(false.B)
      dut.io.scheduler.unsetCritical.bits.poke(0.U)

      dut.clock.step(1)

      dut.io.higher.addr.poke("b01010101110".U)

      dut.io.dbg.byteOffset.expect(2.U)
      dut.io.dbg.blockOffset.expect(1.U)
      dut.io.dbg.index.expect(5.U)
      dut.io.dbg.tag.expect(10.U)

      dut.clock.step(1)

      dut.io.higher.addr.poke("b00111011001".U)

      dut.io.dbg.byteOffset.expect(1.U)
      dut.io.dbg.blockOffset.expect(0.U)
      dut.io.dbg.index.expect(3.U)
      dut.io.dbg.tag.expect(7.U)

      dut.clock.step(1)

      dut.io.higher.addr.poke("b010010110111".U)

      dut.io.dbg.byteOffset.expect(3.U)
      dut.io.dbg.blockOffset.expect(1.U)
      dut.io.dbg.index.expect(6.U)
      dut.io.dbg.tag.expect(18.U)

      dut.clock.step(1)
    }
  }

  "L2LruCache" should "fetch and update a cache line" in {
    val size = 256
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 2
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.reqId.poke(0.U)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)
      dut.io.lower.responseStatus.poke(0.U)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b01010101000", expectedReadData = "hdeadbeef", lowerLevelMissData = Some("hbeefdeaddeadbeef"))

      dut.clock.step(1)

      // Read the first word of the cache line brought in to the cache, expect a hit
      performRead(dut, reqId = 0, "b01010101000", "hdeadbeef")

      dut.clock.step(1)

      // Read the second word of the cache line brought in to the cache, expect a hit
      performRead(dut, reqId = 0, "b01010101100", "hbeefdead")

      dut.clock.step(1)

      // Perform a write to an existing block, expect a hit
      performWrite(dut, reqId = 0, "b01010101100", "hcafebabe")

      dut.clock.step(1)

      // Read the data we have written, expect a hit
      performRead(dut, reqId = 0, "b01010101100", "hcafebabe")

      dut.clock.step(1)
    }
  }

  "L2LruCache" should "evict correct ways in the same set" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 2
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.reqId.poke(0.U)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)
      dut.io.lower.responseStatus.poke(0.U)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b00000000", expectedReadData = "hdeadbeef", lowerLevelMissData = Some("hbeefdeaddeadbeef"))

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b00100100", expectedReadData = "hcacebeef", lowerLevelMissData = Some("hcacebeefbeefcace"), expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b01000000", expectedReadData = "hdeadbabe", lowerLevelMissData = Some("hbabedeaddeadbabe"), expectedReplaceWay = 1)

      dut.clock.step(1)


      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b01100100", expectedReadData = "hbabecafe", lowerLevelMissData = Some("hbabecafecafebabe"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read, expect a miss and evict a cache line
      performRead(dut, reqId = 0, readAddress = "b10000000", expectedReadData = "hfeedface", lowerLevelMissData = Some("hfacefeedfeedface"))

      dut.clock.step(1)

      // Perform a read to an address that was loaded, but was evicted
      performRead(dut, reqId = 0, readAddress = "b00000000", expectedReadData = "hdeadbeef", lowerLevelMissData = Some("hbeefdeaddeadbeef"), expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read to an address that was loaded, but was evicted
      performRead(dut, reqId = 0, readAddress = "b01100000", expectedReadData = "hcafebabe")

      dut.clock.step(1)

      // Perform a read to an address that was loaded, but was evicted
      performRead(dut, reqId = 0, readAddress = "b10000100", expectedReadData = "hfacefeed")

      dut.clock.step(1)

      assertSetContents(
        dut,
        0,
        3,
        Array("b100", "b010", "b000", "b011"),
        Array(Array("hfeedface", "hfacefeed"), Array("hdeadbabe", "hbabedead"), Array("hdeadbeef", "hbeefdead"), Array("hcafebabe", "hbabecafe"))
      )
    }
  }

  "L2LruCache" should "evict correct ways over multiple sets" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 2
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.reqId.poke(0.U)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)
      dut.io.lower.responseStatus.poke(0.U)

      dut.clock.step(1)

      // TODO: Write this test

      pending
    }
  }

  "L2LruCache" should "handle a dirty miss" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 2
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.reqId.poke(0.U)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)
      dut.io.lower.responseStatus.poke(0.U)

      dut.clock.step(1)

      // Perform a write, expect a miss, this cache block will be set as dirty
      performWrite(dut, reqId = 0, writeAddress = "b10100000", writeData = "hd00df33d", lowerLevelMissData = Some("hbeefdeaddeadbeef"), expectedReplaceWay = 0)

      dut.clock.step(1)

      // Perform a write, expect a compulsory miss, cache block will be set as dirty
      performWrite(dut, reqId = 0, writeAddress = "b00100100", writeData = "hf33dd00d", lowerLevelMissData = Some("hcacebeefbeefcace"),expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b01000000", expectedReadData = "hdeadbabe", lowerLevelMissData = Some("hbabedeaddeadbabe"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b01100100", expectedReadData = "hbabecafe", lowerLevelMissData = Some("hbabecafecafebabe"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read, expect a dirty read miss
      performRead(
        dut,
        reqId = 0,
        readAddress = "b10000000",
        expectedReadData = "hfeedface",
        lowerLevelMissData = Some("hfacefeedfeedface"),
        dirtyAddress = Some("b010100000"),
        dirtyData = Some("hbeefdeadd00df33d")
      )

      dut.clock.step(1)

      // Perform a write, expect a dirty write miss
      performWrite(
        dut,
        reqId = 0,
        writeAddress = "b11000100",
        writeData = "hdeadf00d",
        lowerLevelMissData = Some("hc0dedeaddeadc0de"),
        dirtyAddress = Some("b00100000"),
        dirtyData = Some("hf33dd00dbeefcace"),
        expectedReplaceWay = 2
      )

      dut.clock.step(1)

      performRead(dut, reqId = 0, readAddress = "b11000100", expectedReadData = "hdeadf00d")

      dut.clock.step(1)
    }
  }

  "L2ContentionTrackingCache" should "evict correct ways in the same set" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 4
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new ContentionReplacementPolicy(
      ways = ways,
      sets = nSets,
      nCores = nCores,
      missLatency = 5,
      basePolicy = () => new BitPlruReplacementAlgorithm(ways)
    )

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.reqId.poke(0.U)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)
      dut.io.lower.responseStatus.poke(0.U)

      dut.clock.step(1)

      setCoreAsCritical(dut, coreID = 0, contentionLimit = 12)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b00000000", expectedReadData = "hdeadbeef", lowerLevelMissData = Some("hbeefdeaddeadbeef"))

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 0, readAddress = "b00100100", expectedReadData = "hcacebeef", lowerLevelMissData = Some("hcacebeefbeefcace"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 2, readAddress = "b01000000", expectedReadData = "hdeadbabe", lowerLevelMissData = Some("hbabedeaddeadbabe"), expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, reqId = 3, readAddress = "b01100100", expectedReadData = "hbabecafe", lowerLevelMissData = Some("hbabecafecafebabe"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read, expect a miss and evict a critical cache line
      performRead(dut, reqId = 3, readAddress = "b10000000", expectedReadData = "hfeedface", lowerLevelMissData = Some("hfacefeedfeedface"))

      dut.clock.step(1)

      // Perform a read, expect a miss, evict a critical cache line, and reach contention limit
      performRead(dut, reqId = 2, readAddress = "b10100000", expectedReadData = "hc00010ff", lowerLevelMissData = Some("h10ffc000c00010ff"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Fetch another critical cache line
      performRead(dut, reqId = 0, readAddress = "b11000000", expectedReadData = "hfee1dead", lowerLevelMissData = Some("hdeadfee1fee1dead"), expectedReplaceWay = 2)

      dut.clock.step(1)

      // Fetch another critical cache line
      performRead(dut, reqId = 0, readAddress = "b11100100", expectedReadData = "h2badd00d", lowerLevelMissData = Some("h2badd00dd00d2bad"))

      dut.clock.step(1)

      // Refetch non-critical cache line that was evicted
      performRead(dut, reqId = 2, readAddress = "b01000000", expectedReadData = "hdeadbabe", lowerLevelMissData = Some("hbabedeaddeadbabe"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Fetch non-critical cache line
      performRead(dut, reqId = 3, readAddress = "b100000100", expectedReadData = "h10ccdead", lowerLevelMissData = Some("h10ccdeaddead10cc"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read here that is a miss and is prevented from evicting a critical line due to contention limit
      performRead(dut, reqId = 3, readAddress = "b100100000", expectedReadData = "hfadedead", lowerLevelMissData = Some("hdeadfadefadedead"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Request a read from a critical core that will bring in a new cache line evicting a non-critical line
      performRead(dut, reqId = 0, readAddress = "b101000000", expectedReadData = "h0d15ea5e", lowerLevelMissData = Some("hea5e0d150d15ea5e"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Request a read from a critical core that will bring in a new cache line evicting a non-critical line, assigning all the lines to critical cores in the set
      performRead(dut, reqId = 0, readAddress = "b101100000", expectedReadData = "hcafed00d", lowerLevelMissData = Some("hd00dcafecafed00d"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Request a read from a non-critical core that will be rejected since all the lines are assigned to a critical core
      performRead(dut, reqId = 2, readAddress = "b110000000", expectedReadData = "h0", expectRejection = true)

      dut.clock.step(1)

      // Request a read from a critical core that will be able to evict any line                                         hbadc0ffee0ddf00d
      performRead(dut, reqId = 0, readAddress = "b110100100", expectedReadData = "hbeefbbad", lowerLevelMissData = Some("hbeefbbadbbadbeef"))

      dut.clock.step(1)

      // Unset the core as critical, this will allow the core to evict lines that were assigned to it
      unsetCoreAsCritical(dut, coreID = 0)

      dut.clock.step(1)

      // Perform a read request from a non-critical core and expect that we can now evict lines that were assigned to a critical core
      performRead(dut, reqId = 3, readAddress = "b111000000", expectedReadData = "he0ddf00d", lowerLevelMissData = Some("hbadc0ffee0ddf00d"), expectedReplaceWay = 2)

      assertSetContents(
        dut,
        index = 0,
        indexOffset = 3,
        expectedTags = Array("b1101", "b1010", "b1110", "b1011"),
        expectedData = Array(Array("hbbadbeef", "hbeefbbad"), Array("h0d15ea5e", "hea5e0d15"), Array("he0ddf00d", "hbadc0ffe"), Array("hcafed00d", "hd00dcafe")),
        printContents = true
      )

      dut.clock.step(1)
    }
  }
}
