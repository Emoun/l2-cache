package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheTestMissQ extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "work with miss-q and precedent events for 8 ways and 128 sets" in {
    val cache = generateDut(CacheConfigs.config64ContMimPrec)

    test(cache._1.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache._2,
        Tests.testActions4,
        cache._3,
        cache._4,
        cache._5,
        500,
        printResults = PRINT_RESULTS
      )

      dut.clock.step(1)
    }
  }
}
