package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheBitPlruTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, with bit plru policy" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache._1.apply()).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      performTestActions(
        dut,
        cache._2,
        Tests.testActions1,
        cache._3,
        cache._4,
        cache._5,
        700,
        printResults = true
      )

      dut.clock.step(1)
    }
  }

  "SharedPipelinedCache" should "work with mshr entries that are full of cmds" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache._1.apply()).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache._2,
        Tests.testActions5,
        cache._3,
        cache._4,
        cache._5,
        150,
        printResults = PRINT_RESULTS
      )

      dut.clock.step(1)
    }
  }
}
