package caches.hardware.pipelined.cache

import caches.hardware.pipelined.cache.SharedPipelinedCacheTest._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SharedPipelinedCacheTreePlruTest extends AnyFlatSpec with ChiselScalatestTester {
  "SharedPipelinedCache" should "process pipelined requests for 8 ways, 128 sets, with tree plru policy" in {
    val cache = generateDut(CacheConfigs.config64TreePlru)

    test(cache._1.apply()).withAnnotations(Seq(WriteFstAnnotation, VerilatorBackendAnnotation)) { dut =>
      defaultAssignments(dut, cache._2)

      performTestActions(
        dut,
        cache._2,
        Tests.testActions1,
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
