package caches.hardware

import caches.hardware.reppol.TreePlruReplacementPolicy
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SetAssociateCacheTest extends AnyFlatSpec with ChiselScalatestTester {
  def performRead(
                   dut: SetAssociateCacheTestTop,
                   readAddress: String,
                   expectedReadData: String,
                   shouldExpectMiss: Boolean = false,
                   lowerLevelMissData: String = "",
                   lowerLevelAccessLatency: Int = 5,
                   dirtyMiss: Boolean = false,
                   dirtyAddress: String = "",
                   dirtyData: String = ""
                 ): Unit = {
    dut.io.higher.req.poke(true.B)
    dut.io.higher.rw.poke(false.B)
    dut.io.higher.addr.poke(readAddress.U)

    dut.clock.step(1)

    // De-assert a request
    dut.io.higher.req.poke(false.B)
    dut.io.higher.rw.poke(false.B)
    dut.io.higher.addr.poke(0.U)

    if (shouldExpectMiss) {
      expectMiss(dut,
        readAddress,
        lowerLevelMissData,
        lowerLevelAccessLatency,
        dirty = dirtyMiss,
        dirtyAddress = dirtyAddress,
        dirtyData = dirtyData
      )
    }

    expectHit(dut, Some(expectedReadData))
  }

  def expectMiss(
                  dut: SetAssociateCacheTestTop,
                  address: String,
                  lowerLevelMissData: String,
                  lowerLevelLatency: Int,
                  dirty: Boolean = false,
                  dirtyAddress: String = "",
                  dirtyData: String = ""
                ): Unit = {
    dut.clock.step(1)

    if (dirty) {
      dut.io.lower.req.expect(true.B)
      dut.io.lower.rw.expect(true.B)
      dut.io.lower.addr.expect(dirtyAddress.U)
      dut.io.lower.wData.expect(dirtyData.U)

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

    // Inform the cache that the requested data from the lower level is available
    dut.io.lower.rData.poke(lowerLevelMissData.U)
    dut.io.lower.ack.poke(true.B)

    dut.clock.step(1)

    dut.io.lower.ack.poke(false.B)
  }

  def expectHit(dut: SetAssociateCacheTestTop, readData: Option[String]): Unit = {
    dut.io.higher.ack.expect(true.B)

    if (readData.isDefined) {
      dut.io.higher.rData.expect(readData.get.U)
    }

    dut.clock.step(1)
  }

  def performWrite(
                    dut: SetAssociateCacheTestTop,
                    writeAddress: String,
                    writeData: String,
                    shouldExpectMiss: Boolean = false,
                    lowerLevelMissData: String = "",
                    lowerLevelLatency: Int = 5,
                    dirtyMiss: Boolean = false,
                    dirtyAddress: String = "",
                    dirtyData: String = ""
                  ): Unit = {
    dut.io.higher.req.poke(true.B)
    dut.io.higher.rw.poke(true.B)
    dut.io.higher.wData.poke(writeData.U)
    dut.io.higher.addr.poke(writeAddress.U)

    dut.clock.step(1)

    // De-assert a request
    dut.io.higher.req.poke(false.B)
    dut.io.higher.rw.poke(false.B)
    dut.io.higher.addr.poke(0.U)

    if (shouldExpectMiss) {
      expectMiss(
        dut,
        writeAddress,
        lowerLevelMissData,
        lowerLevelLatency,
        dirty = dirtyMiss,
        dirtyAddress = dirtyAddress,
        dirtyData = dirtyData
      )
    }

    expectHit(dut, None)
  }

  def assertSetContents(
                         dut: SetAssociateCacheTestTop,
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

      val tag = setTags(wayIdx).peekInt()
      if (printContents) println(s"Tag for way $wayIdx: $tag")
    }

    if (printContents) println("~~~~~~~~~~~~~~~~~~~~~~")

    val setData = dut.io.dbg.setData
    for (wayIdx <- setData.indices) {
      val block = setData(wayIdx)

      for (wordIdx <- block.indices) {
        block(wordIdx).expect(expectedData(wayIdx)(wordIdx).U)

        val word = block(wordIdx).peekInt().toString(16)
        if (printContents) println(s"Word for way $wayIdx at ($wordIdx): $word")
      }
    }

    if (printContents) println("")
  }

  "LruCache" should "decode correct address fields" in {
    val size = 256
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets)

    test(new SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)

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

  "LruCache" should "fetch and update a cache line" in {
    val size = 256
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets)

    test(new SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)

      dut.clock.step(1)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b01010101000", "hdeadbeef", shouldExpectMiss = true, "hbeefdeaddeadbeef")

      dut.clock.step(1)

      // Read the first word of the cache line brought in to the cache, expect a hit
      performRead(dut, "b01010101000", "hdeadbeef")

      dut.clock.step(1)

      // Read the second word of the cache line brought in to the cache, expect a hit
      performRead(dut, "b01010101100", "hbeefdead")

      dut.clock.step(1)

      // Perform a write to an existing block, expect a hit
      performWrite(dut, "b01010101100", "hcafebabe")

      dut.clock.step(1)

      // Read the data we have written, expect a hit
      performRead(dut, "b01010101100", "hcafebabe")

      dut.clock.step(1)
    }
  }

  "LruCache" should "evict correct ways in the same set" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets)

    test(new SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(0.U)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b00000000", "hdeadbeef", shouldExpectMiss = true, "hbeefdeaddeadbeef")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(2.U)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b00100100", "hcacebeef", shouldExpectMiss = true, "hcacebeefbeefcace")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(1.U)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b01000000", "hdeadbabe", shouldExpectMiss = true, "hbabedeaddeadbabe")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(3.U)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b01100100", "hbabecafe", shouldExpectMiss = true, "hbabecafecafebabe")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(0.U)

      // Perform a read, expect a miss and evict a cache line
      performRead(dut, "b10000000", "hfeedface", shouldExpectMiss = true, "hfacefeedfeedface")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(2.U)

      // Perform a read to an address that was loaded, but was evicted
      performRead(dut, "b00000000", "hdeadbeef", shouldExpectMiss = true, "hbeefdeaddeadbeef")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(1.U)

      // Perform a read to an address that was loaded, but was evicted
      performRead(dut, "b01100000", "hcafebabe")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(1.U)

      // Perform a read to an address that was loaded, but was evicted
      performRead(dut, "b10000100", "hfacefeed")

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

  "LruCache" should "evict correct ways over multiple sets" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets)

    test(new SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)

      dut.clock.step(1)

      // TODO: Write this test

      pending
    }
  }

  "LruCache" should "handle a dirty miss" in {
    val size = 128
    val ways = 4
    val bytesPerBlock = 8
    val bytesPerWord = 4
    val nSets = (size / bytesPerBlock) / ways
    val repPolicyGen = () => new TreePlruReplacementPolicy(ways, nSets)

    test(new SetAssociateCacheTestTop(size, ways, bytesPerBlock, bytesPerWord, repPolicyGen)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Default assignments
      dut.io.higher.req.poke(false.B)
      dut.io.higher.addr.poke(0.U)
      dut.io.higher.rw.poke(false.B)
      dut.io.higher.wData.poke(0.U)
      dut.io.higher.wMask.poke(0.U)
      dut.io.lower.rData.poke(0.U)
      dut.io.lower.ack.poke(false.B)

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(0.U)

      // Perform a write, expect a miss, this cache block will be set as dirty
      performWrite(dut, "b10100000", "hd00df33d", shouldExpectMiss = true, "hbeefdeaddeadbeef")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(2.U)

      // Perform a write, expect a compulsory miss, cache block will be set as dirty
      performWrite(dut, "b00100100", "hf33dd00d", shouldExpectMiss = true, "hcacebeefbeefcace")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(1.U)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b01000000", "hdeadbabe", shouldExpectMiss = true, "hbabedeaddeadbabe")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(3.U)

      // Perform a read, expect a compulsory miss
      performRead(dut, "b01100100", "hbabecafe", shouldExpectMiss = true, "hbabecafecafebabe")

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(0.U)

      // Perform a read, expect a dirty read miss
      performRead(
        dut,
        "b10000000",
        "hfeedface",
        shouldExpectMiss = true,
        "hfacefeedfeedface",
        dirtyMiss = true,
        dirtyAddress = "b010100000",
        dirtyData = "hbeefdeadd00df33d"
      )

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(2.U)

      // Perform a write, expect a dirty write miss
      performWrite(
        dut,
        "b11000100",
        "hdeadf00d",
        shouldExpectMiss = true,
        "hc0dedeaddeadc0de",
        dirtyMiss = true,
        dirtyAddress = "b00100000",
        dirtyData = "hf33dd00dbeefcace"
      )

      dut.clock.step(1)

      dut.io.dbg.replaceWay.expect(1.U)

      performRead(dut, "b11000100", "hdeadf00d")

      dut.clock.step(1)
    }
  }
}
