package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheContWithAllEvents extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "work with all contention events" in {
    val cache = generateDut(CacheConfigs.config64ContMimPrecWb)

    test(cache._1.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      // Issue the first set of requests
      performTestActions(
        dut,
        cache._2,
        Tests.testActions8,
        cache._3,
        cache._4,
        cache._5,
        1000,
        printResults = PRINT_RESULTS
      )

      dut.clock.step(1)
    }
  }
}
