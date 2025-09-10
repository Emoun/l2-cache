package caches.hardware.pipelined

import caches.hardware.reppol._
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable

trait TestAction {}

case class CacheRequest(
                         coreId: Int,
                         reqId: Int,
                         rw: Boolean,
                         tag: Int,
                         index: Int,
                         blockOffset: Int,
                         byteOffset: Int = 0,
                         rejected: Boolean = false,
                         byteEn: Option[String] = None,
                         wData: Option[String] = None,
                         expectedData: Option[String] = None
                       ) extends TestAction

case class Stall(stallCycles: Int = 0) extends TestAction()

case class ExpectFinishedRejectedResponse(coreId: Int, reqId: Int, expectedData: String) extends TestAction()

case class CacheResponse(
                          receivedCC: Int,
                          coreId: Int,
                          reqId: Int,
                          data: String
                        )

object Tests {
  val testActions1: Array[TestAction] = Array(
    CacheRequest(coreId = 1, reqId = 0, tag = 0, index = 0, blockOffset = 0, rw = false, expectedData = Some("cafebabebabecafedeadbeefbeefdead")), // (tag = 0, idx = 0, blockOff = 0, way = 0)
    CacheRequest(coreId = 1, reqId = 1, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 2, tag = 0, index = 0, blockOffset = 2, rw = true, wData = Some("hd00dfeed0000000000000000"), byteEn = Some("b0000011000000000")), // (HIT)
    CacheRequest(coreId = 3, reqId = 3, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("h000000000000000000000000deadba55"), byteEn = Some("b0000000000001111")),
    CacheRequest(coreId = 3, reqId = 4, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("h0000000000000000ba55d00d00000000"), byteEn = Some("b0000000011110000")),
    CacheRequest(coreId = 3, reqId = 5, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("h00000000f00df17e0000000000000000"), byteEn = Some("b0000111100000000")),
    CacheRequest(coreId = 3, reqId = 6, tag = 3, index = 55, blockOffset = 0, rw = true, wData = Some("hb17ebabe0000000000000000deadba55"), byteEn = Some("b1111000000000000")),
    CacheRequest(coreId = 3, reqId = 7, tag = 0, index = 2, blockOffset = 3, rw = false, expectedData = Some("6a8fdc4ede91ab230fabc9877c3a12ed")), // (tag = 0, idx = 2, blockOff = 3, way = 0)
    CacheRequest(coreId = 2, reqId = 8, tag = 2, index = 0, blockOffset = 0, rw = false, expectedData = Some("013bb292b95895f88cde7faf55adaaba")), // (tag = 2, idx = 0, blockOff = 0, way = 1)
    CacheRequest(coreId = 2, reqId = 9, tag = 2, index = 0, blockOffset = 1, rw = true, wData = Some("h01234567"), byteEn = Some("b0000000000001111")),
    CacheRequest(coreId = 2, reqId = 10, tag = 2, index = 0, blockOffset = 1, rw = true, wData = Some("h09abcdef00000000"), byteEn = Some("b0000000011110000")),
    CacheRequest(coreId = 3, reqId = 11, tag = 3, index = 0, blockOffset = 3, rw = true, wData = Some("hbeefdead000000000000000000000000"), byteEn = Some("b0101000000000000")), // (tag = 3, idx = 0, blockOff = 3, way = 2)
    CacheRequest(coreId = 0, reqId = 12, tag = 4, index = 0, blockOffset = 0, rw = false, expectedData = Some("7c7f2e45dab3dea3508d8aeec21e6aea")), // (tag = 4, idx = 0, blockOff = 0, way = 3)
    CacheRequest(coreId = 1, reqId = 13, tag = 5, index = 0, blockOffset = 1, rw = false, expectedData = Some("cc5fd511c237a27e2451003dbc4d8025")), // (tag = 5, idx = 0, blockOff = 1, way = 4)
    CacheRequest(coreId = 1, reqId = 14, tag = 3, index = 55, blockOffset = 2, rw = false, expectedData = Some("2fbcdc49bd5651eb531b4ef64e1393ed")),
    CacheRequest(coreId = 2, reqId = 15, tag = 6, index = 0, blockOffset = 2, rw = false, expectedData = Some("b58d852edb0bf08973f52e36fcbf3fc4")), // (tag = 6, idx = 0, blockOff = 2, way = 5)
    CacheRequest(coreId = 3, reqId = 16, tag = 3, index = 55, blockOffset = 0, rw = false, expectedData = Some("b17ebabef00df17eba55d00ddeadba55")),
    CacheRequest(coreId = 3, reqId = 17, tag = 7, index = 0, blockOffset = 3, rw = false, expectedData = Some("ab59ff12bd819a95cf599088f0b54814")), // (tag = 7, idx = 0, blockOff = 3, way = 6)
    CacheRequest(coreId = 0, reqId = 18, tag = 8, index = 0, blockOffset = 1, rw = false, expectedData = Some("094b46853d585cb5c0d488396f1c65fd")), // (tag = 8, idx = 0, blockOff = 1, way = 7)
    CacheRequest(coreId = 1, reqId = 19, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 20, tag = 3, index = 0, blockOffset = 3, rw = false, expectedData = Some("ddef31ad9e4ec895a20047db0a108d2d")), // (HIT)
    CacheRequest(coreId = 1, reqId = 21, tag = 9, index = 0, blockOffset = 0, rw = false, expectedData = Some("3e653a9dbf432147e4d0eef71a9d9897")), // (tag = 0, idx = 0, blockOff = 0, way = 0) Evict way 1
    CacheRequest(coreId = 3, reqId = 22, tag = 3, index = 79, blockOffset = 2, rw = true, wData = Some("hfaceb00c0000000000000000"), byteEn = Some("b0000111100000000")), // (tag = 3, idx = 79, blockOff = 2, way = 0)
    CacheRequest(coreId = 1, reqId = 23, tag = 10, index = 0, blockOffset = 2, rw = false, expectedData = Some("0cae4f56f5aad7083c53d5a9edc9e37c")), // (tag = 0, idx = 0, blockOff = 2, way = 1) Evict way 2
    CacheRequest(coreId = 1, reqId = 24, tag = 2, index = 0, blockOffset = 1, rw = false, expectedData = Some("1338e010da28bd0209abcdef01234567")), // Test the writeback, (tag = 0, idx = 0, blockOff = 0, way = 2) Evict way 2
    // Write to an existing line in the cache (HIT)
    CacheRequest(coreId = 1, reqId = 25, tag = 0, index = 2, blockOffset = 0, rw = true, wData = Some("hd00dfeed"), byteEn = Some("b0000000000001111")), // (tag = 0, idx = 0, blockOff = 2, way = ???)
    // MISS: WAY - 0, INDEX - 45, TAG - 2
    CacheRequest(coreId = 3, reqId = 26, tag = 2, index = 45, blockOffset = 1, rw = true, wData = Some("hfaceb00c00000000"), byteEn = Some("b0000000011110000")), // (tag = 2, idx = 45, blockOff = 1, way = ???)
    // MISS: WAY - 0, INDEX - 159, TAG - 5
    CacheRequest(coreId = 2, reqId = 27, tag = 5, index = 159, blockOffset = 2, rw = true, wData = Some("hcafeface0000000000000000"), byteEn = Some("b0000111100000000")), // (tag = 5, idx = 159, blockOff = 1, way = ???)
    // HIT: WAY - 1, INDEX - 0, TAG - 2
    CacheRequest(coreId = 0, reqId = 28, tag = 2, index = 0, blockOffset = 1, rw = false, expectedData = Some("1338e010da28bd0209abcdef01234567")),
    // MISS: WAY - 0, INDEX - 127, TAG - 8
    CacheRequest(coreId = 0, reqId = 29, tag = 8, index = 127, blockOffset = 3, rw = true, wData = Some("hb00cface000000000000000000000000"), byteEn = Some("b1111000000000000")),
    // MISS: WAY - 1, INDEX - 127, TAG - 5
    CacheRequest(coreId = 0, reqId = 30, tag = 5, index = 127, blockOffset = 1, rw = true, wData = Some("hd15ea55500000000"), byteEn = Some("b0000000011110000")),
    // MISS: WAY - 2, INDEX - 127, TAG - 7
    CacheRequest(coreId = 3, reqId = 31, tag = 7, index = 127, blockOffset = 1, rw = false, expectedData = Some("68667763d4de48b2ec721696cf7b782f")),
    // MISS: WAY - 3, INDEX - 127, TAG - 13
    CacheRequest(coreId = 1, reqId = 32, tag = 13, index = 127, blockOffset = 3, rw = true, wData = Some("hfee1dead000000000000000000000000"), byteEn = Some("b1111000000000000")),
    // MISS AND EVICT: WAY - 0, INDEX - 127, TAG - 11
    CacheRequest(coreId = 0, reqId = 33, tag = 11, index = 127, blockOffset = 1, rw = true, wData = Some("hdeadfee100000000"), byteEn = Some("b0000000011110000")),
    //MISS AND EVICT: WAY - 0, INDEX - 127, TAG - 11
    CacheRequest(coreId = 0, reqId = 34, tag = 11, index = 127, blockOffset = 2, rw = false, expectedData = Some("a8f2ed21c6a55c1d9051b72d6422762a")),
    CacheRequest(coreId = 0, reqId = 35, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    CacheRequest(coreId = 1, reqId = 36, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    CacheRequest(coreId = 2, reqId = 37, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    CacheRequest(coreId = 3, reqId = 38, tag = 221, index = 62, blockOffset = 0, rw = false, expectedData = Some("1bc046d6a45fd8ac65676a660e8c3047")),
    Stall(stallCycles = 100),
    CacheRequest(coreId = 0, reqId = 39, tag = 16, index = 113, blockOffset = 0, rw = false, expectedData = Some("a4d868e8605871a722f93960ce9195c6")),
    Stall(stallCycles = 21),
    CacheRequest(coreId = 0, reqId = 40, tag = 16, index = 113, blockOffset = 1, rw = false, expectedData = Some("18d3be76d8a863e9b85207e7cac5155b")),
    CacheRequest(coreId = 0, reqId = 41, tag = 16, index = 113, blockOffset = 2, rw = false, expectedData = Some("358958f999ddee22de0082b374f1b3f5")),
  )

  val testActions2: Array[TestAction] = Array(
    CacheRequest(coreId = 1, reqId = 0, tag = 0, index = 0, blockOffset = 0, rw = false, expectedData = Some("cafebabebabecafedeadbeefbeefdead")), // Bring new line into the cache (put in way: 0, idx: 0)

    CacheRequest(coreId = 1, reqId = 1, tag = 0, index = 4, blockOffset = 0, rw = false, expectedData = Some("8fb2741c9dea0137bc3f0e2a40cbde98")), // Bring new line into the cache (put in way: 0, idx: 4)

    CacheRequest(coreId = 1, reqId = 2, tag = 1, index = 0, blockOffset = 0, rw = false, expectedData = Some("bbef1226751129196ede4c8a9dc4fbd4")), // Bring new line into the cache (put in way: 1, idx: 0)
    CacheRequest(coreId = 3, reqId = 3, tag = 2, index = 0, blockOffset = 0, rw = false, expectedData = Some("013bb292b95895f88cde7faf55adaaba")), // Bring new line into the cache (put in way: 2, idx: 0)
    CacheRequest(coreId = 1, reqId = 4, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 5, tag = 0, index = 0, blockOffset = 2, rw = true, wData = Some("hd00dfeed"), byteEn = Some("b0000000000001111")), // Write to an existing line in the cache (HIT)

    CacheRequest(coreId = 1, reqId = 6, tag = 0, index = 4, blockOffset = 1, rw = false, expectedData = Some("ce28f9010fdabe34b93e1a7d7ac4d2f1")), // (HIT)

    CacheRequest(coreId = 1, reqId = 7, tag = 1, index = 0, blockOffset = 3, rw = false, expectedData = Some("107709677fdfcc120da699175fad2fc1")), // (HIT)

    CacheRequest(coreId = 1, reqId = 8, tag = 2, index = 4, blockOffset = 1, rw = false, expectedData = Some("a2cc291a987d1f7c4d0f31f40e755553")), // Bring new line into the cache (put in way: 1, idx: 4)

    CacheRequest(coreId = 3, reqId = 9, tag = 3, index = 0, blockOffset = 2, rw = false, expectedData = Some("c70485594aeb67d9904d7f5fd0cbca8d")), // Bring new line into the cache (put in way: 3, idx: 0)

    CacheRequest(coreId = 3, reqId = 10, tag = 3, index = 4, blockOffset = 3, rw = false, expectedData = Some("52c626535c92c204ebcdf992fdc70d3f")), // Bring new line into the cache (put in way: 2, idx: 4)
    CacheRequest(coreId = 3, reqId = 11, tag = 1, index = 4, blockOffset = 3, rw = false, expectedData = Some("ba8c556a6162ff795bcfa1e8a6c78ad4")), // Bring new line into the cache (put in way: 3, idx: 4)

    CacheRequest(coreId = 1, reqId = 12, tag = 0, index = 3, blockOffset = 2, rw = false, expectedData = Some("56789abcef01234593cd78aba1e2f8d0")), // Bring new line into the cache (put in way: 0, idx: 3)
    CacheRequest(coreId = 2, reqId = 13, tag = 0, index = 2, blockOffset = 3, rw = false, expectedData = Some("6a8fdc4ede91ab230fabc9877c3a12ed")), // Bring new line into the cache (put in way: 0, idx: 2)

    CacheRequest(coreId = 3, reqId = 14, tag = 4, index = 0, blockOffset = 1, rw = false, expectedData = Some("734ccecdbe11b44dae6eaf60cb218892")), // Bring new line into the cache (put in way: 4, idx: 0)

    CacheRequest(coreId = 0, reqId = 15, tag = 4, index = 4, blockOffset = 3, rw = false, expectedData = Some("640df405821ff856a0436b31f624cd8b")), // (HIT)

    CacheRequest(coreId = 3, reqId = 16, tag = 0, index = 0, blockOffset = 2, rw = false, expectedData = Some("fee1deaddeadfee1c00010ffd00dfeed")), // (HIT)
    CacheRequest(coreId = 1, reqId = 17, tag = 5, index = 0, blockOffset = 1, rw = false, expectedData = Some("cc5fd511c237a27e2451003dbc4d8025")), // Bring new line into the cache (put in way: 5, idx: 0),

    CacheRequest(coreId = 1, reqId = 18, tag = 1, index = 3, blockOffset = 1, rw = false, expectedData = Some("30464e5bf598385749afdfcfba3bae98")), // Bring new line into the cache (put in way: 1, idx: 3),

    CacheRequest(coreId = 1, reqId = 19, tag = 5, index = 4, blockOffset = 3, rw = false, expectedData = Some("07b14887beaa396de0919c32e9127550")), // Bring new line into the cache (put in way: 4, idx: 4),

    CacheRequest(coreId = 1, reqId = 20, tag = 6, index = 0, blockOffset = 0, rw = false, expectedData = Some("2888e103997223a9003bc584e091cc8a")), // Bring new line into the cache (put in way: 6, idx: 0),

    CacheRequest(coreId = 3, reqId = 21, tag = 6, index = 4, blockOffset = 1, rw = false, expectedData = Some("3bc5171128c41fbfe21a727455ae40f9")), // Bring new line into the cache (put in way: 5, idx: 4),

    CacheRequest(coreId = 3, reqId = 22, tag = 7, index = 0, blockOffset = 1, rw = false, expectedData = Some("040fd41d7771f0535a07ec451db97efb")), // Bring new line into the cache (put in way: 7, idx: 0),
    CacheRequest(coreId = 0, reqId = 23, tag = 8, index = 0, blockOffset = 0, rw = false, expectedData = Some("39df6c998739192bae26debd84620423")), // Bring new line into the cache (put in way: 0, idx: 0),
    CacheRequest(coreId = 2, reqId = 24, tag = 9, index = 0, blockOffset = 0, rw = false, expectedData = Some("3e653a9dbf432147e4d0eef71a9d9897")), // Bring new line into the cache (put in way: 1, idx: 0) reach contention limit for core 1,
    CacheRequest(coreId = 0, reqId = 25, tag = 10, index = 0, blockOffset = 0, rw = false, expectedData = Some("af675aee062fe9dc7fcb9bda56a92dbc")), // Bring new line into the cache (put in way: 2, idx: 0),
    CacheRequest(coreId = 2, reqId = 26, tag = 11, index = 0, blockOffset = 0, rw = false, expectedData = Some("03ca3d2c8469be21134a3cb1702b246e")), // Bring new line into the cache (put in way: 2, idx: 0),
    CacheRequest(coreId = 1, reqId = 27, tag = 12, index = 0, blockOffset = 0, rw = false, expectedData = Some("56fc51c6be6e2e632bba17208a3b9cec")), // Bring new line into the cache (put in way: 0, idx: 0),
    CacheRequest(coreId = 3, reqId = 28, tag = 13, index = 0, blockOffset = 0, rw = false, expectedData = Some("b97d19f32ca140ba54915eb145da4782")), // Bring new line into the cache (put in way: 1, idx: 0),
    CacheRequest(coreId = 3, reqId = 29, tag = 14, index = 0, blockOffset = 0, rw = false, expectedData = Some("117e27b41fb7ac5b8ed5184c7f9453ae")), // Bring new line into the cache (put in way: 2, idx: 0),
    CacheRequest(coreId = 3, reqId = 30, tag = 15, index = 0, blockOffset = 0, rw = false, expectedData = Some("dd27e4c14bf57c1ca196400c0e5d5ce7")), // Bring new line into the cache (put in way: 3, idx: 0),
    CacheRequest(coreId = 2, reqId = 31, tag = 16, index = 0, blockOffset = 0, rw = false, expectedData = Some("40a13302ac635051b398eb9a4cec416c"), rejected = true), // Rejected response (once free, put in way: 4, idx: 0),
    CacheRequest(coreId = 0, reqId = 32, tag = 17, index = 0, blockOffset = 0, rw = false, expectedData = Some("d1f8fbeb63d88f19a2af35a0cd00f5ef"), rejected = true), // Rejected response (once free, put in way: 4, idx: 0),
    CacheRequest(coreId = 1, reqId = 33, tag = 18, index = 0, blockOffset = 0, rw = false, expectedData = Some("ca065d4eea469633ab2fd69681debb57")), // Critical core can evict any other core
  )

  val testActions3: Array[TestAction] = Array(
    ExpectFinishedRejectedResponse(coreId = 2, reqId = 31, expectedData = "40a13302ac635051b398eb9a4cec416c"),
    ExpectFinishedRejectedResponse(coreId = 0, reqId = 32, expectedData = "d1f8fbeb63d88f19a2af35a0cd00f5ef"),
    CacheRequest(coreId = 3, reqId = 34, tag = 8, index = 4, blockOffset = 0, rw = false, expectedData = Some("bf7ecefbef86e816b49f6740df6d0069")),
    CacheRequest(coreId = 3, reqId = 35, tag = 0, index = 5, blockOffset = 2, rw = false, expectedData = Some("b7e1903f47db2c8efa81039b23a5f64e")),
    CacheRequest(coreId = 3, reqId = 36, tag = 1, index = 3, blockOffset = 1, rw = false, expectedData = Some("30464e5bf598385749afdfcfba3bae98")),
    CacheRequest(coreId = 0, reqId = 37, tag = 17, index = 0, blockOffset = 0, rw = false, expectedData = Some("d1f8fbeb63d88f19a2af35a0cd00f5ef")),
    CacheRequest(coreId = 2, reqId = 38, tag = 0, index = 4, blockOffset = 2, rw = false, expectedData = Some("314a8f9cd7e40cb26f19de83a481b2dc")), // (HIT)
  )

  val testActions4: Array[TestAction] = Array(
    // Test sequential reads from different memory regions
    CacheRequest(coreId = 0, reqId = 0, tag = 0, index = 0, blockOffset = 0, rw = false, expectedData = Some("cafebabebabecafedeadbeefbeefdead")), // Words 0-3: beefdead, deadbeef, babecafe, cafebabe
    CacheRequest(coreId = 1, reqId = 1, tag = 0, index = 0, blockOffset = 1, rw = false, expectedData = Some("deadbabebabedeadfeedfacefacefeed")), // Words 4-7: facefeed, feedface, babedead, deadbabe
    CacheRequest(coreId = 2, reqId = 2, tag = 0, index = 0, blockOffset = 2, rw = false, expectedData = Some("fee1deaddeadfee1c00010ff10ffc000")), // Words 8-11: 10ffc000, c00010ff, deadfee1, fee1dead
    CacheRequest(coreId = 3, reqId = 3, tag = 0, index = 0, blockOffset = 3, rw = false, expectedData = Some("dead10cc10ccdeadd00d2bad2badd00d")), // Words 12-15: 2badd00d, d00d2bad, 10ccdead, dead10cc

    // Test reads from mid-range addresses (around word 1000)
    CacheRequest(coreId = 0, reqId = 4, tag = 1, index = 0, blockOffset = 0, rw = false, expectedData = Some("bbef1226751129196ede4c8a9dc4fbd4")), // Words 1000-1003 region
    CacheRequest(coreId = 1, reqId = 5, tag = 1, index = 0, blockOffset = 1, rw = false, expectedData = Some("f8ab4690dd99254eb0eeda4a06a360bc")), // Words 1004-1007 region

    // Test reads from higher addresses (around word 5000)
    CacheRequest(coreId = 2, reqId = 6, tag = 5, index = 0, blockOffset = 0, rw = false, expectedData = Some("1172becdb246ca947b61e7da07672879")),
    CacheRequest(coreId = 3, reqId = 7, tag = 5, index = 0, blockOffset = 1, rw = false, expectedData = Some("cc5fd511c237a27e2451003dbc4d8025")),

    // Test interleaved reads and writes
    CacheRequest(coreId = 0, reqId = 8, tag = 0, index = 1, blockOffset = 0, rw = false, expectedData = Some("bbadbeefbeefbbade0ddf00dbadc0ffe")), // Words 16-19
    CacheRequest(coreId = 1, reqId = 9, tag = 0, index = 1, blockOffset = 1, rw = true, wData = Some("hdeadcafe00000000"), byteEn = Some("b0000000011110000")), // Partial write
    Stall(5),
//    CacheRequest(coreId = 2, reqId = 10, tag = 0, index = 1, blockOffset = 1, rw = false, expectedData = Some("0d15ea5eea5e0d15deadcafed00dcafe")), // Read back modified data

    // Test cache conflicts and evictions with different tags, same index
    CacheRequest(coreId = 3, reqId = 11, tag = 2, index = 1, blockOffset = 0, rw = false, expectedData = Some("5094fd4c8f779c01498c95738c2435e3")), // Force different tag, same index
    CacheRequest(coreId = 0, reqId = 12, tag = 3, index = 1, blockOffset = 0, rw = false, expectedData = Some("30e2a17ee045d14ab58defac3308495e")), // Another different tag, same index
    CacheRequest(coreId = 1, reqId = 13, tag = 4, index = 1, blockOffset = 0, rw = false, expectedData = Some("8c6cab16656b3baa17613c7518180499")), // Yet another different tag

    // Test boundary conditions - end of memory regions
    CacheRequest(coreId = 2, reqId = 14, tag = 10, index = 127, blockOffset = 0, rw = false, expectedData = Some("817b131959fd217505137e68dc457d74")), // High index
    CacheRequest(coreId = 3, reqId = 15, tag = 10, index = 127, blockOffset = 3, rw = false, expectedData = Some("073645cb83479e5c4002236602d01f20")), // High index, high block offset

    // Test mixed access patterns - read, write, read same location
    CacheRequest(coreId = 0, reqId = 16, tag = 0, index = 2, blockOffset = 0, rw = false, expectedData = Some("beefcacecacebeeffeedfacefacefeed")), // Words 32-35
    CacheRequest(coreId = 1, reqId = 17, tag = 0, index = 2, blockOffset = 0, rw = true, wData = Some("hcafebabe000000000000000000000000"), byteEn = Some("b1111000000000000")),
    Stall(5),
//    CacheRequest(coreId = 2, reqId = 18, tag = 0, index = 2, blockOffset = 0, rw = false, expectedData = Some("cafebabecacebeeffeedfacefacefeed")), // Read modified

    // Test all cores accessing same cache line simultaneously
    CacheRequest(coreId = 0, reqId = 19, tag = 0, index = 3, blockOffset = 0, rw = false, expectedData = Some("bd42f9c33e0b8a6ff71d24c989bc3e10")), // Words 36-39
    CacheRequest(coreId = 1, reqId = 20, tag = 0, index = 3, blockOffset = 1, rw = false, expectedData = Some("d239c4fa41e0bc97caf3b2186f83d1a5")), // Words 40-43
    CacheRequest(coreId = 2, reqId = 21, tag = 0, index = 3, blockOffset = 2, rw = false, expectedData = Some("56789abcef01234593cd78aba1e2f8d0")), // Words 44-47
    CacheRequest(coreId = 3, reqId = 22, tag = 0, index = 3, blockOffset = 3, rw = false, expectedData = Some("e1d0a5f737ed5f90cbfae10310293abc")), // Words 48-51

    // Test write-through behavior with byte enables
    CacheRequest(coreId = 0, reqId = 23, tag = 0, index = 4, blockOffset = 0, rw = true, wData = Some("hdeadbeef0000000000000000"), byteEn = Some("b011000000000")), // 2-byte write
    CacheRequest(coreId = 1, reqId = 24, tag = 0, index = 4, blockOffset = 0, rw = true, wData = Some("h000000000000000000000000cafebabe"), byteEn = Some("b0000000000001111")), // Different bytes
    Stall(5),
//    CacheRequest(coreId = 2, reqId = 25, tag = 0, index = 4, blockOffset = 0, rw = false, expectedData = Some("8fb2741c9adbe137bc3f0e2acafebabe")), // Read combined write

    // Test large address space coverage
    CacheRequest(coreId = 3, reqId = 26, tag = 15, index = 63, blockOffset = 2, rw = false, expectedData = Some("1e7ddd1da8695c92691e32ca820d4be5")), // Mid-range tag and index
    CacheRequest(coreId = 0, reqId = 27, tag = 31, index = 31, blockOffset = 1, rw = false, expectedData = Some("db19b1635794a999575ef4b5da482462")), // Higher tag and index

    // Test rapid sequential access pattern
    CacheRequest(coreId = 1, reqId = 28, tag = 0, index = 10, blockOffset = 0, rw = false, expectedData = Some("feed123410badf00faceb00cc0ffeeee")), // Words around 640
    CacheRequest(coreId = 1, reqId = 29, tag = 0, index = 10, blockOffset = 1, rw = false, expectedData = Some("fa11babebabefacec01dbabea11face1")),
    CacheRequest(coreId = 1, reqId = 30, tag = 0, index = 10, blockOffset = 2, rw = false, expectedData = Some("ba5efacef005ba11d00dbabedeadfeed")),
    CacheRequest(coreId = 1, reqId = 31, tag = 0, index = 10, blockOffset = 3, rw = false, expectedData = Some("c0dec0debaddbabedead1337bad15ead")),

    // Test cross-core write conflicts
    CacheRequest(coreId = 2, reqId = 32, tag = 0, index = 20, blockOffset = 0, rw = true, wData = Some("haaaaaaaa0000000000000000"), byteEn = Some("b0000011000000000")),
    CacheRequest(coreId = 3, reqId = 33, tag = 0, index = 20, blockOffset = 0, rw = true, wData = Some("h00000000bbbbbbbb00000000"), byteEn = Some("b0000000011000000")),
    Stall(5),
//    CacheRequest(coreId = 0, reqId = 34, tag = 0, index = 20, blockOffset = 0, rw = false, expectedData = Some("aaaaaaaabbbbbbbb0000000000000000")), // Read combined

    // Final stress test with high-frequency mixed operations
    CacheRequest(coreId = 1, reqId = 35, tag = 100, index = 100, blockOffset = 0, rw = false, expectedData = Some("7bdce363af7a7d087b11f4d10c7df004")), // High addresses
    CacheRequest(coreId = 2, reqId = 36, tag = 100, index = 100, blockOffset = 1, rw = true, wData = Some("hffffffff00000000"), byteEn = Some("b0000000011110000")),
    CacheRequest(coreId = 3, reqId = 37, tag = 100, index = 100, blockOffset = 2, rw = false, expectedData = Some("35d4c8b68346564ff6448cbc036d040b")),
    CacheRequest(coreId = 0, reqId = 38, tag = 100, index = 100, blockOffset = 3, rw = true, wData = Some("h000000000000000012345678"), byteEn = Some("b0000000000001111")),
    Stall(5),
//    CacheRequest(coreId = 1, reqId = 39, tag = 100, index = 100, blockOffset = 3, rw = false, expectedData = Some("21c7660216f71ee6e3f0f7c2b1feb075")) // Read final state
  )
}

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

  def expectCacheResponse(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, expectedData: String): Unit = {
    dut.io.requests.cores(coreId).resp.reqId.valid.expect(true.B, s"Did not receive a response for request: $reqId.")
    dut.io.requests.cores(coreId).resp.reqId.bits.expect(reqId.U, s"Did not receive a response for request: $reqId.")
    dut.io.requests.cores(coreId).resp.rData.expect(expectedData.U, s"Did not receive correct data for a request: $reqId.")
  }

  def assertAccesses[T <: TestAction] (
                      dut: SharedPipelinedCacheTestTop,
                      nCores: Int,
                      testActions: Array[T],
                      indexWidth: Int,
                      blockOffsetWidth: Int,
                      byteOffsetWidth: Int,
                      maxCCs: Int,
                      printResults: Boolean = false
                    ): Unit = {
    val responses = mutable.Set[CacheResponse]()
    var previousRequestCore: Option[Int] = None
    var actionIdx = 0
    var currentCC = 0
    var stallCycle: Option[Int] = None

    while (currentCC < maxCCs) {

      // Need to unset the request signals for the previous request
      if (previousRequestCore.isDefined) {
        val coreIdx = previousRequestCore.get
        dut.io.requests.cores(coreIdx).req.reqId.valid.poke(false.B)
        dut.io.requests.cores(coreIdx).req.reqId.bits.poke(0.U)
        dut.io.requests.cores(coreIdx).req.addr.poke(0.U)
        dut.io.requests.cores(coreIdx).req.rw.poke(false.B)
        dut.io.requests.cores(coreIdx).req.wData.poke(0.U)
      }

      if (actionIdx < testActions.length) {
        if(stallCycle.isEmpty) { // If we are stalling, we do not perform any actions
          val action = testActions(actionIdx)

          action match {
            case CacheRequest(coreId, reqId, rw, tag, index, blockOffset, byteOffset, _, byteEn, wData, _) =>
              if (dut.io.requests.cores(coreId).req.reqId.ready.peekBoolean()) {
                val addr = tag << (indexWidth + blockOffsetWidth + byteOffsetWidth) |
                  index << (blockOffsetWidth + byteOffsetWidth) |
                  blockOffset << byteOffsetWidth |
                  byteOffset

                println(s"Issued request at CC $currentCC: Core: $coreId, ReqId: $reqId, Addr: ${addr.toHexString}, RW: $rw, WData: ${wData.getOrElse("None")}, ByteEn: ${byteEn.getOrElse((math.pow(2, dut.subBlockDataWidth / 8).toInt - 1).toBinaryString)}")

                performCacheRequest(dut, coreId = coreId, reqId = reqId, addr = addr, rw = rw, wData = wData, byteEn = byteEn)
                previousRequestCore = Some(coreId)

                actionIdx += 1
              } else {
                previousRequestCore = None
              }
            case Stall(cycles) =>
              println(s"Waiting for $cycles cycles at CC $currentCC, before issuing next request.")
              previousRequestCore = None
              stallCycle = Some(currentCC + cycles)
              actionIdx += 1
            case ExpectFinishedRejectedResponse(_, _, _) =>
              actionIdx += 1
            case t => throw new Exception(s"Received unexpected action type: ${t.getClass.getSimpleName}")
          }
        } else {
          if (stallCycle.get == currentCC) { // Once we reached the stall cycle, we can clear it
            stallCycle = None
          }
        }
      } else {
        previousRequestCore = None
      }

      val response = checkForResponse(dut, currentCC, nCores)

      if (response.isDefined) {
        responses.add(response.get)
      }

      dut.clock.step(1)
      currentCC += 1
    }

    if (printResults) {
      println("")
      val sortedResponse = responses.toSeq.sortBy(req => req.receivedCC)
      println(s"Received ${sortedResponse.size} responses in total.")
      for (resp <- sortedResponse) {
        println(s"Response: CC: ${resp.receivedCC}, Core: ${resp.coreId}, ReqId: ${resp.reqId}, Data: ${resp.data}")
      }
    }

    // Assert the number of responses matches expected number of responses
    val requestsWithExpectedResponse = testActions.filter{
      case CacheRequest(_, _, rw, _, _, _, _, rejected, _, _, _) if !rejected && !rw => true
      case ExpectFinishedRejectedResponse(_, _, _) => true
      case _ => false
    }

    // Assert the received data
    for (req <- requestsWithExpectedResponse) {
      req match {
        case CacheRequest(coreId, reqId, _, _, _, _, _, _, _, _, expectedData) =>
          val response = responses.find(resp => resp.coreId == coreId && resp.reqId == reqId)

          assert(response.isDefined, s"Did not receive a response for request: $reqId from core: $coreId.")
          assert(response.get.data == expectedData.get, s"Received data: ${response.get.data} does not match expected data: ${expectedData.get} for request: $reqId from core: $coreId.")
        case ExpectFinishedRejectedResponse(coreId, reqId, expectedData) =>
          val response = responses.find(resp => resp.coreId == coreId && resp.reqId == reqId)

          assert(response.isDefined, s"Did not receive a response for a previously rejected request: $reqId from core: $coreId.")
          assert(response.get.data == expectedData, s"Received data: ${response.get.data} does not match expected data: $expectedData for rejected request: $reqId from core: $coreId.")
        case t => throw new Exception(s"Received action type other than ${CacheRequest.getClass.getSimpleName}: ${t.getClass.getSimpleName}")
      }
    }

    if (printResults) {
      println("")
    }
  }

  def checkForResponse(dut: SharedPipelinedCacheTestTop, clockCycle: Int, nCores: Int): Option[CacheResponse] = {
    for (i <- 0 until nCores) {
      if (dut.io.requests.cores(i).resp.reqId.valid.peek().litToBoolean) {
        val reqId = dut.io.requests.cores(i).resp.reqId.bits.peek().litValue.toInt
        val data = dut.io.requests.cores(i).resp.rData.peek().litValue.toString(16).reverse.padTo(dut.subBlockDataWidth / 4, '0').reverse

        return Some(CacheResponse(
          receivedCC = clockCycle,
          coreId = i,
          reqId = reqId,
          data = data
        ))
      }
    }

    None
  }

  def performCacheRequest(dut: SharedPipelinedCacheTestTop, coreId: Int, reqId: Int, addr: Int, rw: Boolean, wData: Option[String] = None, byteEn: Option[String] = None): Unit = {
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
  }

  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, and plru policy configuration" in {
    val sizeInBytes = 65536 // 64 KiB
    val nCores = 4
    val nWays = 8
    val addressWidth = 25
    val reqIdWidth = 16
    val bytesPerBlock = 64
    val bytesPerSubBlock = 16
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val l2RepPolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val memFile = "./hex/test_mem_32w.hex"

    val memBeatSize = 4
    val memBurstLen = 4

    val byteOffsetWidth = log2Up(bytesPerSubBlock)
    val blockOffsetWidth = log2Up(bytesPerBlock / bytesPerSubBlock)
    val indexWidth = log2Up(nSets)

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = memBeatSize,
      memBurstLen = memBurstLen,
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

      assertAccesses(
        dut,
        nCores,
        Tests.testActions1,
        indexWidth,
        blockOffsetWidth,
        byteOffsetWidth,
        700,
        printResults = true
      )
    }
  }

  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, and contention policy configuration" in {
    val sizeInBytes = 65536 // 64 KiB
    val nCores = 4
    val nWays = 8
    val addressWidth = 25
    val reqIdWidth = 16
    val bytesPerBlock = 64
    val bytesPerSubBlock = 16
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val basePolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val l2RepPolicy = () => new ContentionReplacementPolicy(nWays, nSets, nCores, basePolicy)
    val memFile = "./hex/test_mem_32w.hex"

    val printResults = true

    val memBeatSize = 4
    val memBurstLen = 4

    val byteOffsetWidth = log2Up(bytesPerSubBlock)
    val blockOffsetWidth = log2Up(bytesPerBlock / bytesPerSubBlock)
    val indexWidth = log2Up(nSets)

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = memBeatSize,
      memBurstLen = memBurstLen,
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

      // Set the second core as critical
      setCoreAsCritical(dut, coreID = 1, contentionLimit = 2)

      dut.clock.step(1)

      // Set the fourth core as critical
      setCoreAsCritical(dut, coreID = 3, contentionLimit = 2)

      dut.clock.step(1)

      // Issue the first set of requests
      assertAccesses(dut, nCores, Tests.testActions2, indexWidth, blockOffsetWidth, byteOffsetWidth, 870, printResults = printResults)

      dut.clock.step(1)

      // Unset the second core as critical
      unsetCoreAsCritical(dut, coreID = 1)

      dut.clock.step(1)

      // Evict some previously critical caches
      assertAccesses(dut, nCores, Tests.testActions3, indexWidth, blockOffsetWidth, byteOffsetWidth, 100, printResults = printResults)
    }
  }

  "SharedPipelinedCache" should "handle stress test with plru policy" in {
    val sizeInBytes = 65536 // 64 KiB
    val nCores = 4
    val nWays = 8
    val addressWidth = 25
    val reqIdWidth = 16
    val bytesPerBlock = 64
    val bytesPerSubBlock = 16
    val nSets = sizeInBytes / (nWays * bytesPerBlock)
    val l2RepPolicy = () => new BitPlruReplacementPolicy(nWays, nSets, nCores)
    val memFile = "./hex/test_mem_32w.hex"

    val memBeatSize = 4
    val memBurstLen = 4

    val byteOffsetWidth = log2Up(bytesPerSubBlock)
    val blockOffsetWidth = log2Up(bytesPerBlock / bytesPerSubBlock)
    val indexWidth = log2Up(nSets)

    test(new SharedPipelinedCacheTestTop(
      sizeInBytes = sizeInBytes,
      nWays = nWays,
      nCores = nCores,
      reqIdWidth = reqIdWidth,
      addressWidth = addressWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = bytesPerSubBlock,
      memBeatSize = memBeatSize,
      memBurstLen = memBurstLen,
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

      // Execute the elaborate stress test
      assertAccesses(
        dut,
        nCores,
        Tests.testActions4,
        indexWidth,
        blockOffsetWidth,
        byteOffsetWidth,
        1000, // Increased cycles to handle more complex test patterns
        printResults = true
      )
    }
  }
}
