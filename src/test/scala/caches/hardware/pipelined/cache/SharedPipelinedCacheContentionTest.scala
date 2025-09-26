package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheContentionTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, with contention policy" in {
    val cache = generateDut(CacheConfigs.config64Cont)

    test(cache._1.apply()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache._2,
        Tests.testActions2,
        cache._3,
        cache._4,
        cache._5,
        1100,
        printResults = PRINT_RESULTS
      )

      dut.clock.step(1)
    }
  }
}
