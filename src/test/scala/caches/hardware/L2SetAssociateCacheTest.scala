package caches.hardware

import caches.hardware.reppol._
import caches.hardware.util.Constants._
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

    if (printContents) printSetContents(dut, index, indexOffset)
  }

  def printSetContents(dut: L2SetAssociateCacheTestTop, index: Int, indexOffset: Int): Unit = {
    dut.io.higher.addr.poke((index << indexOffset).U)

    dut.clock.step(1)

    val setTags = dut.io.dbg.setTags
    val setData = dut.io.dbg.setData
    val tags = setTags.map(_.peekInt()).toArray
    val data = setData.map(_.map(_.peekInt().toString(16)).toArray).toArray

    for (wayIdx <- tags.indices) {
      println(s"Tag for way $wayIdx: ${tags(wayIdx)}, Dirty: ${dut.io.dbg.dirtyTags.peekInt().testBit(wayIdx)}")
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
    val addressWidth = ADDRESS_WIDTH
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
    val size = 256
    val ways = 8
    val bytesPerBlock = 16
    val bytesPerWord = 4
    val nCores = 8
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new BitPlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

      // Perform a read, expect a compulsory miss (set = 0, tag = 1, word = 0)
      performRead(dut, reqId = 0, readAddress = "b000100000", expectedReadData = "hdeadbeef", lowerLevelMissData = Some("hcacebeefbeefcacebeefdeaddeadbeef"))

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 2, word = 1)
      performRead(dut, reqId = 0, readAddress = "b001000100", expectedReadData = "he7230764", lowerLevelMissData = Some("h44b50af47bfb1f2ce72307642386db52"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 3, word = 2)
      performRead(dut, reqId = 0, readAddress = "b001101000", expectedReadData = "h55af25e1", lowerLevelMissData = Some("hb43cb56655af25e1f8a54af33462cdcd"), expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 4, word = 0)
      performRead(dut, reqId = 0, readAddress = "b010000000", expectedReadData = "h1c2644f1", lowerLevelMissData = Some("h6c181536c3f99c508f84c2aa1c2644f1"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 5, word = 1)
      performRead(dut, reqId = 0, readAddress = "b010100100", expectedReadData = "h6656c5c9", lowerLevelMissData = Some("hdb99f1e7494a6e4a6656c5c976091a99"), expectedReplaceWay = 4)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 6, word = 3)
      performRead(dut, reqId = 0, readAddress = "b011001100", expectedReadData = "he44b16e2", lowerLevelMissData = Some("he44b16e237bfd2fcfc521e5a312fc6a9"), expectedReplaceWay = 5)

      dut.clock.step(1)

      // Perform a write, expect a compulsory miss (set = 1, tag = 7, word = 1)
      performRead(dut, reqId = 0, readAddress = "b011110100", expectedReadData = "hf3cee332", lowerLevelMissData = Some("hdbc760783fe13052f3cee3322610103c"))

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 8, word = 2)
      performRead(dut, reqId = 0, readAddress = "b100001000", expectedReadData = "h3686b48f", lowerLevelMissData = Some("h2fa414363686b48fec0386f6309da6b0"), expectedReplaceWay = 6)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 9, word = 3)
      performRead(dut, reqId = 0, readAddress = "b100111100", expectedReadData = "h5f306fdc", lowerLevelMissData = Some("h5f306fdc13ad6d5e2a5e211250af2cda"), expectedReplaceWay = 1)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 10, word = 0)
      performRead(dut, reqId = 0, readAddress = "b101000000", expectedReadData = "hf5e21cb1", lowerLevelMissData = Some("he40e618b705e59ce9620543bf5e21cb1"), expectedReplaceWay = 7)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 11, word = 1)
      performRead(dut, reqId = 0, readAddress = "b101110100", expectedReadData = "h278a23a3", lowerLevelMissData = Some("hdc0876d7cd0a5c8d278a23a3477e3a4b"), expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 12, word = 2)
      performRead(dut, reqId = 0, readAddress = "b110011000", expectedReadData = "hf5e88978", lowerLevelMissData = Some("h7b77dfa7f5e88978e7b8f725be417679"), expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 13, word = 3)
      performRead(dut, reqId = 0, readAddress = "b110111100", expectedReadData = "h85d77b4d", lowerLevelMissData = Some("h85d77b4d652dec71ae90a63835970f54"), expectedReplaceWay = 4)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 14, word = 0)
      performWrite(dut, reqId = 0, writeAddress = "b111010000", writeData = "hdeadbeef", lowerLevelMissData = Some("had74896eec1799466c5506e400bd6af6"), expectedReplaceWay = 5)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 0, tag = 15, word = 1)
      performRead(dut, reqId = 0, readAddress = "b111100100", expectedReadData = "ha75496f2", lowerLevelMissData = Some("hcfff6c214adce522a75496f211222b9d"))

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 16, word = 1)
      performRead(dut, reqId = 0, readAddress = "b1000010100", expectedReadData = "hbe99bbda", lowerLevelMissData = Some("h02cd556a811928d5be99bbdad59687ee"), expectedReplaceWay = 6)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss (set = 1, tag = 17, word = 3)
      performRead(dut, reqId = 0, readAddress = "b1000111100", expectedReadData = "h5a62c614", lowerLevelMissData = Some("h5a62c61422a49b3ecd2e78160945c927"), expectedReplaceWay = 7)

      dut.clock.step(1)

      // Perform a read (set = 1, tag = 7, word = 3)
      performRead(dut, reqId = 0, readAddress = "b0011111100", expectedReadData = "hdbc76078")

      dut.clock.step(1)

      // Perform a read (set = 1, tag = 9, word = 1)
      performRead(dut, reqId = 0, readAddress = "b0100110100", expectedReadData = "h2a5e2112", expectedReplaceWay = 1)

      dut.clock.step(1)

      // Perform a read (set = 1, tag = 11, word = 2)
      performRead(dut, reqId = 0, readAddress = "b0101111000", expectedReadData = "hcd0a5c8d", expectedReplaceWay = 2)

      dut.clock.step(1)

      // Perform a read (set = 1, tag = 12, word = 0)
      performRead(dut, reqId = 0, readAddress = "b0110010000", expectedReadData = "hbe417679", expectedReplaceWay = 3)

      dut.clock.step(1)

      // Perform a read (set = 1, tag = 13, word = 1)
      performRead(dut, reqId = 0, readAddress = "b0110110100", expectedReadData = "hae90a638", expectedReplaceWay = 4)

      dut.clock.step(1)

      // Perform a read, expect a dirty miss (set = 1, tag = 18, word = 1)
      performRead(dut,
        reqId = 0,
        readAddress = "b1001010100",
        expectedReadData = "h9fccb11c",
        lowerLevelMissData = Some("h06b3d00d0678987b9fccb11cca5d39d9"),
        expectedReplaceWay = 5,
        dirtyData = Some("had74896eec1799466c5506e4deadbeef"),
        dirtyAddress = Some("b111010000")
      )

      dut.clock.step(1)
    }
  }

  "L2LruCache" should "handle a dirty miss" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nCores = 2
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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
      performWrite(dut, reqId = 0, writeAddress = "b10100000", writeData = "hd00df33d", lowerLevelMissData = Some("hbeefdeaddeadbeef"))

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
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new ContentionReplacementPolicy(
      ways = ways,
      sets = nSets,
      nCores = nCores,
      basePolicy = () => new BitPlruReplacementAlgorithm(ways)
    )

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

      setCoreAsCritical(dut, coreID = 0, contentionLimit = 2)

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
        expectedData = Array(Array("hbbadbeef", "hbeefbbad"), Array("h0d15ea5e", "hea5e0d15"), Array("he0ddf00d", "hbadc0ffe"), Array("hcafed00d", "hd00dcafe"))
      )

      dut.clock.step(1)
    }
  }

  "L2ContentionTrackingCache" should "evict correct ways over 8 ways" in {
    val size = 256
    val ways = 8
    val bytesPerBlock = 16
    val bytesPerWord = 4
    val nCores = 8
    val addressWidth = ADDRESS_WIDTH
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new BitPlruReplacementPolicy(ways, nSets, nCores)

    test(new L2SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, nCores, addressWidth, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

      // TODO: Finish this test

      pending
    }
  }
}
