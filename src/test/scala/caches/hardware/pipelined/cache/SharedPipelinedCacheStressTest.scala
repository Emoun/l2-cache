package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheStressTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "handle stress test with bit plru policy" in {
    val cache = generateDut(CacheConfigs.config64BitPlru)

    test(cache._1.apply()).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      performTestActions(
        dut,
        cache._2,
        Tests.testActions3,
        cache._3,
        cache._4,
        cache._5,
        1000,
        printResults = PRINT_RESULTS
      )
    }
  }
}
