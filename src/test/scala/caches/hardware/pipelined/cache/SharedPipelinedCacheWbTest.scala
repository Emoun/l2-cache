package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheWbTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "work with wb contention events" in {
    val cache = generateDut(CacheConfigs.config64ContWb)

    test(cache._1.apply()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache._2,
        Tests.testActions6,
        cache._3,
        cache._4,
        cache._5,
        900,
        printResults = PRINT_RESULTS
      )

      dut.clock.step(1)
    }
  }
}
