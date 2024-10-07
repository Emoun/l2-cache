package caches

import org.scalatest.funsuite.AnyFunSuite


trait SimpleCacheTests[C<: SoftCache] extends AnyFunSuite {
  def className: String
  def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int): C

  test("Capacity") {
    assert(createInstance(2,2,2,1,10).capacity == 8)
    assert(createInstance(4,2,2,1,10).capacity == 16)
    assert(createInstance(2,4,2,1,10).capacity == 16)
    assert(createInstance(2,2,4,1,10).capacity == 16)
  }

  test("Get second in line") {
    val cache = createInstance(2,2,2,1,10);
    assert(cache.getCacheLine(0,0) == 10)
    assert(cache.getCacheLine(1,0) == 1)
    assert(cache.getCacheLine(2,0) == 10)
    assert(cache.getCacheLine(3,0) == 1)
  }

  test("Get third in line") {
    val cache = createInstance(4,4,2,2,20);
    assert(cache.getCacheLine(0,0) == 20)
    assert(cache.getCacheLine(1,0) == 2)
    assert(cache.getCacheLine(2,0) == 2)
    assert(cache.getCacheLine(3,0) == 2)
    assert(cache.getCacheLine(4,0) == 20)
    assert(cache.getCacheLine(5,0) == 2)
    assert(cache.getCacheLine(6,0) == 2)
    assert(cache.getCacheLine(7,0) == 2)
  }

  test("More sets than ways") {
    val cache = createInstance(3,2,3,2,20);
    assert(cache.getCacheLine(0,0) == 20)
    assert(cache.getCacheLine(9,0) == 20)
    assert(cache.getCacheLine(18,0) == 20)
  }

  test("Get Cold Addresses") {
    val cache = createInstance(2,4,4,11,17);
    assert(cache.getCacheLine(0,0) == 17)
    assert(cache.getCacheLine(2,0) == 17)
    assert(cache.getCacheLine(4,0) == 17)
    assert(cache.getCacheLine(6,0) == 17)
  }
}

trait LruTests[C<: SoftCache with LRUReplacement[_]] extends SimpleCacheTests[C]
{
  test("Replace LRU 1") {
    val cache = createInstance(2,2,2,14,130);
    cache.getCacheLine(0,0) // set 0, way 0
    cache.getCacheLine(2,0) // set 1, way 0
    cache.getCacheLine(4,0) // set 0, way 1
    cache.getCacheLine(6,0) // set 1, way 1

    // Set 0 is full, with '0' being least recently used
    assert(cache.getCacheLine(8,0) == 130)
    assert(cache.getCacheLine(5,0) == 14)
    assert(cache.getCacheLine(1,0) == 130)
    assert(cache.getCacheLine(9,0) == 130)
  }

  test("Replace LRU 2") {
    val cache = createInstance(4,3,3,2,8);
    cache.getCacheLine(12,0) // set 0, way 0
    cache.getCacheLine(0,0) // set 0, way 1
    cache.getCacheLine(24,0) // set 0, way 2

    cache.getCacheLine(12,0) // set 0, way 0
    cache.getCacheLine(0,0) // set 0, way 1
    cache.getCacheLine(24,0) // set 0, way 2

    // Set 0 is full, with '12' being least recently used
    assert(cache.getCacheLine(36,0) == 8)
    assert(cache.getCacheLine(3,0) == 2)
    assert(cache.getCacheLine(25,0) == 2)
    assert(cache.getCacheLine(14,0) == 8)
  }
}

trait PartitionedTests[C<: SoftCache with PartitionedReplacement[_]] extends SimpleCacheTests[C]
{
  test("Evict only own partition") {
    val cache = createInstance(3,3,3,2,8);
    cache.assignWay(0, 0); // Each core gets a way
    cache.assignWay(1, 1);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    assert(cache.getCacheLine(27,2) == 8) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == 2) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == 2) // Ensure was not evicted
    assert(cache.getCacheLine(9,1) == 2) // Ensure was not evicted
  }

  test("May evict unassigned ways") {
    val cache = createInstance(3,3,3,2,8);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    assert(cache.getCacheLine(27,2) == 8) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == 2) // Ensure did get saved

    assert(cache.getCacheLine(9,0) == 2) // Ensure was not evicted
    assert(cache.getCacheLine(18,1) == 2) // Ensure was not evicted
  }

  test("May hit in other's ways") {
    val cache = createInstance(3,3,3,2,8);
    cache.assignWay(0, 0); // Each core gets a way
    cache.assignWay(1, 1);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    // Each hits in the others'
    assert(cache.getCacheLine(9,0) == 2)
    assert(cache.getCacheLine(18,0) == 2)

    assert(cache.getCacheLine(0,1) == 2)
    assert(cache.getCacheLine(18,1) == 2)

    assert(cache.getCacheLine(0,2) == 2)
    assert(cache.getCacheLine(9,2) == 2)
  }
}

class LruCacheTest extends AnyFunSuite with LruTests[LruCache] {
  override def className: String = "LruCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int): LruCache = {
    new LruCache(lineLength, ways, sets, shortLatency,longLatency)
  }
}

class PartitionedCacheTest extends AnyFunSuite with LruTests[PartitionedCache] with PartitionedTests[PartitionedCache] {
  override def className: String = "PartitionedCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int): PartitionedCache = {
    new PartitionedCache(lineLength, ways, sets, shortLatency,longLatency)
  }
}

class ContentionCacheTest extends AnyFunSuite with LruTests[ContentionCache] {
  override def className: String = "ContentionCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int): ContentionCache = {
    new ContentionCache(lineLength, ways, sets, shortLatency,longLatency)
  }

  test("Low criticality cannot evict high criticality at limit") {
    val cache = createInstance(3,3,3,2,8);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == 8) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 8) // Ensure did not get saved

    assert(cache.getCacheLine(0,1) == 2) // Ensure did get saved
    assert(cache.getCacheLine(9,1) == 2)
    assert(cache.getCacheLine(18,1) == 2)
  }

  test("Low criticality evicts other low-criticality if high-criticality at limit") {
    val cache = createInstance(3,3,3,2,8);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,1) // way 2

    assert(cache.getCacheLine(27,1) == 8) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 2) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == 2) // Ensure critical was not evicted
    assert(cache.getCacheLine(18,1) == 2)
    assert(cache.getCacheLine(9,1) == 8)
  }

  test("Low criticality can evict high criticality not at limit") {
    val cache = createInstance(3,3,3,1,3);
    cache.setCriticality(0, 100);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == 3) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 1) // Ensure did get saved
  }

  test("Low criticality can evict high criticality not at limit 2") {
    val cache = createInstance(3,3,3,1,3);
    cache.setCriticality(0, 100);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,2) == 3) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == 1) // Ensure did get saved
    assert(cache.getCacheLine(0,1) == 1) // Ensure limited was saved
    assert(cache.getCacheLine(9,0) == 3) // Ensure limited was saved
  }

  test("Unlimited eviction reaches limit") {
    val cache = createInstance(3,3,3,10, 20);
    cache.setCriticality(0, 10);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2
    // Fill up set 1
    cache.getCacheLine(3,0) // way 0
    cache.getCacheLine(12,0) // way 1
    cache.getCacheLine(21,0) // way 2

    assert(cache.getCacheLine(27,1) == 20) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 10) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == 20) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == 20) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Unlimited eviction reaches limit 2") {
    val cache = createInstance(3,3,3,10, 20);
    cache.setCriticality(0, 20);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2
    // Fill up set 1
    cache.getCacheLine(3,0) // way 0
    cache.getCacheLine(12,0) // way 1
    cache.getCacheLine(21,0) // way 2
    // Fill up set 2
    cache.getCacheLine(6,0) // way 0
    cache.getCacheLine(15,0) // way 1
    cache.getCacheLine(24,0) // way 2

    assert(cache.getCacheLine(27,1) == 20) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 10) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == 20) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == 10) // Ensure did get saved

    assert(cache.getCacheLine(33,1) == 20) // try to use set 2 again
    assert(cache.getCacheLine(33,1) == 20) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Critical can evict non-critical") {
    val cache = createInstance(4,4,4,5, 15);
    cache.setCriticality(1, 20);

    // Fill up set 1 with non-critical
    cache.getCacheLine(4,0) // way 0
    cache.getCacheLine(20,0) // way 1
    cache.getCacheLine(36,0) // way 2
    cache.getCacheLine(52,0) // way 3

    assert(cache.getCacheLine(68,1) == 15) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == 5) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == 15) // Ensure non-critical was evicted
  }

  test("Critical not at limit may be evicted") {
    val cache = createInstance(4,4,4,5, 25);
    cache.setCriticality(1, 20);

    // Fill up set 1
    cache.getCacheLine(4,1) // way 0, critical
    cache.getCacheLine(20,0) // way 1, non-critical
    cache.getCacheLine(36,0) // way 2, non-critical
    cache.getCacheLine(52,0) // way 3, non-critical

    assert(cache.getCacheLine(68,1) == 25) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == 5) // Ensure new access did get saved
    assert(cache.getCacheLine(20,0) == 5) // Ensure non-LRU stay saved
    assert(cache.getCacheLine(36,0) == 5)
    assert(cache.getCacheLine(52,0) == 5)

    assert(cache.getCacheLine(84,1) == 25) // Try another critical load
    assert(cache.getCacheLine(69,1) == 5) // Ensure critical was not evicted because it was at limit
    assert(cache.getCacheLine(36,0) == 5)// Ensure non-LRU stay saved
    assert(cache.getCacheLine(52,0) == 5)
  }

  test("Critical may evict critical without contention") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 10);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 25) // try to use set 0
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 25) // try again
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 5)
  }

  test("Critical may evict critical without contention 2") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 25) // try to use set 0
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 25) // try again
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 5)
  }

  test("Critical hit on non-critical line increases contention limit") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(4,0) == 5) // try to use set 0 with critical

    assert(cache.getCacheLine(10,1) == 25) // try to overwrite set 1 with non-critical
    assert(cache.getCacheLine(10,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(6,1) == 5)
  }

  test("Critical hit on non-critical line makes it critical") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(0,0) == 5) // try to use set 0 with critical
    assert(cache.getCacheLine(10,1) == 25) // overwrite set 1 with non-critical, negating the previous win
    assert(cache.getCacheLine(8,0) == 25) // overwrite set 0 way 1 with critical

    assert(cache.getCacheLine(12,1) == 25) // try to use set 0 with non-critical
    assert(cache.getCacheLine(12,1) == 25) //
  }

  test("Critical hit on critical line does not increase contention count") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0 with crit 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1
    // Fill up set 1 with crit 1
    cache.getCacheLine(2,1) // way 0
    cache.getCacheLine(6,1) // way 1

    assert(cache.getCacheLine(3,0) == 5) // Hit on other crit

    assert(cache.getCacheLine(8,2) == 25) // Try to evict crit
    assert(cache.getCacheLine(8,2) == 25) // Ensure did not work
    assert(cache.getCacheLine(0,0) == 5) // Ensure was not evicted
    assert(cache.getCacheLine(4,0) == 5) // Ensure was not evicted
  }
}

class TimeoutCacheTest extends AnyFunSuite with LruTests[TimeoutCache] {
  override def className: String = "TimeoutCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int): TimeoutCache = {
    new TimeoutCache(lineLength, ways, sets, shortLatency,longLatency, 0)
  }
  def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int, timeout: Int): TimeoutCache = {
    new TimeoutCache(lineLength, ways, sets, shortLatency,longLatency, timeout)
  }

  test("Low criticality cannot evict high criticality before timeout") {
    val cache = createInstance(2,2,2,1,10,2);
    cache.setPriority(0, 0); // Assign all to core 0
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 10) // try to use set 0 again
    assert(cache.getCacheLine(8,1) == 10) // Ensure did not get saved

    assert(cache.getCacheLine(0,0) == 1) // Ensure did get saved
    assert(cache.getCacheLine(4,0) == 1)
  }

  test("Low criticality can evict high-criticality in in unprioritized way") {
    val cache = createInstance(2,2,2,1,10,2);
    cache.setPriority(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,0) // way 1, without prio

    assert(cache.getCacheLine(8,1) == 10) // try to use set 0 again
    assert(cache.getCacheLine(8,1) == 1) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == 1) // Ensure did get saved
  }

  test("High-criticality prefers timedout ways") {
    val cache = createInstance(2,3,2,1,10,2);
    cache.setPriority(0, 0);
    cache.setPriority(1, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,1) // way 1, with prio
    cache.getCacheLine(8,2) // way 2, without prio

    assert(cache.getCacheLine(16,0) == 10) // try to use set 0 again
    assert(cache.getCacheLine(16,1) == 1) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == 1) // Ensure did get saved
    assert(cache.getCacheLine(4,1) == 1) // Ensure did get saved
  }

  test("High-criticality prefers own ways") {
    val cache = createInstance(2,3,2,1,10,2);
    cache.setPriority(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0, without prio
    cache.getCacheLine(4,1) // way 1, with prio
    cache.getCacheLine(8,1) // way 2, without prio
    cache.getCacheLine(0,1) // make sure way 0 isn't LRU

    assert(cache.getCacheLine(16,0) == 10) // try to use set 0 again
    assert(cache.getCacheLine(16,0) == 1) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == 1) // Ensure did get saved
    assert(cache.getCacheLine(8,0) == 1) // Ensure did get saved
  }

  test("Cycles reduce timeout") {
    val cache = createInstance(2,2,2,1,10,1);
    cache.setPriority(0, 0);
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,0) // way 1, with prio

    assert(cache.getCacheLine(8,1) == 10) // try to use without prio
    assert(cache.getCacheLine(8,1) == 10) // Ensure didn't get saved

    cache.advanceCycle() // Make the timeouts run out

    assert(cache.getCacheLine(8,1) == 10) // try to use without prio
    assert(cache.getCacheLine(12,1) == 10) // try to use without prio
    assert(cache.getCacheLine(8,1) == 1) // Ensure did get saved
    assert(cache.getCacheLine(12,1) == 1) // Ensure did get saved
  }

  test("Timeout as given") {
    val cache = createInstance(2,2,2,1,10,5);
    cache.setPriority(0, 0);
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,0) // way 1, with prio


    cache.advanceCycle() // Make timout reach 1
    cache.advanceCycle()
    cache.advanceCycle()
    cache.advanceCycle()

    assert(cache.getCacheLine(8,1) == 10) // Ensure timeout hasn't been reached
    assert(cache.getCacheLine(8,1) == 10) // Ensure didn't get saved

    cache.advanceCycle() // Timeout reached

    assert(cache.getCacheLine(8,1) == 10) // Ensure didn't get saved
    assert(cache.getCacheLine(12,1) == 10) // try to use without prio
    assert(cache.getCacheLine(8,1) == 1) // Ensure did get saved
    assert(cache.getCacheLine(12,1) == 1) // Ensure did get saved
  }

}

class ContentionPartCacheTest extends AnyFunSuite with LruTests[ContentionPartCache] {
  override def className: String = "ContentionPartCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int): ContentionPartCache = {
    new ContentionPartCache(lineLength, ways, sets, shortLatency,longLatency)
  }

  test("Low criticality cannot evict high criticality at limit") {
    val cache = createInstance(3,3,3,2,8);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == 8) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 8) // Ensure did not get saved

    assert(cache.getCacheLine(0,1) == 2) // Ensure did get saved
    assert(cache.getCacheLine(9,1) == 2)
    assert(cache.getCacheLine(18,1) == 2)
  }

  test("Low criticality evicts other low-criticality if high-criticality at limit") {
    val cache = createInstance(3,3,3,2,8);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,1) // way 2

    assert(cache.getCacheLine(27,1) == 8) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 2) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == 2) // Ensure critical was not evicted
    assert(cache.getCacheLine(18,1) == 2)
    assert(cache.getCacheLine(9,1) == 8)
  }

  test("Low criticality can evict high criticality not at limit") {
    val cache = createInstance(3,3,3,1,3);
    cache.setCriticality(0, 100);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == 3) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 1) // Ensure did get saved
  }

  test("Low criticality can evict high criticality not at limit 2") {
    val cache = createInstance(3,3,3,1,3);
    cache.setCriticality(0, 100);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,2) == 3) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == 1) // Ensure did get saved
    assert(cache.getCacheLine(0,1) == 1) // Ensure limited was saved
    assert(cache.getCacheLine(9,0) == 3) // Ensure limited was saved
  }

  test("Unlimited eviction reaches limit") {
    val cache = createInstance(3,3,3,10, 20);
    cache.setCriticality(0, 10);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2
    // Fill up set 1
    cache.getCacheLine(3,0) // way 0
    cache.getCacheLine(12,0) // way 1
    cache.getCacheLine(21,0) // way 2

    assert(cache.getCacheLine(27,1) == 20) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 10) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == 20) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == 20) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Unlimited eviction reaches limit 2") {
    val cache = createInstance(3,3,3,10, 20);
    cache.setCriticality(0, 20);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2
    // Fill up set 1
    cache.getCacheLine(3,0) // way 0
    cache.getCacheLine(12,0) // way 1
    cache.getCacheLine(21,0) // way 2
    // Fill up set 2
    cache.getCacheLine(6,0) // way 0
    cache.getCacheLine(15,0) // way 1
    cache.getCacheLine(24,0) // way 2

    assert(cache.getCacheLine(27,1) == 20) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == 10) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == 20) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == 10) // Ensure did get saved

    assert(cache.getCacheLine(33,1) == 20) // try to use set 2 again
    assert(cache.getCacheLine(33,1) == 20) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Critical can evict non-critical") {
    val cache = createInstance(4,4,4,5, 15);
    cache.setCriticality(1, 20);

    // Fill up set 1 with non-critical
    cache.getCacheLine(4,0) // way 0
    cache.getCacheLine(20,0) // way 1
    cache.getCacheLine(36,0) // way 2
    cache.getCacheLine(52,0) // way 3

    assert(cache.getCacheLine(68,1) == 15) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == 5) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == 15) // Ensure non-critical was evicted
  }

  test("Critical not at limit may be evicted") {
    val cache = createInstance(4,4,4,5, 25);
    cache.setCriticality(1, 20);

    // Fill up set 1
    cache.getCacheLine(4,1) // way 0, critical
    cache.getCacheLine(20,0) // way 1, non-critical
    cache.getCacheLine(36,0) // way 2, non-critical
    cache.getCacheLine(52,0) // way 3, non-critical

    assert(cache.getCacheLine(68,1) == 25) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == 5) // Ensure new access did get saved
    assert(cache.getCacheLine(20,0) == 5) // Ensure non-LRU stay saved
    assert(cache.getCacheLine(36,0) == 5)
    assert(cache.getCacheLine(52,0) == 5)

    assert(cache.getCacheLine(84,1) == 25) // Try another critical load
    assert(cache.getCacheLine(69,1) == 5) // Ensure critical was not evicted because it was at limit
    assert(cache.getCacheLine(36,0) == 5)// Ensure non-LRU stay saved
    assert(cache.getCacheLine(52,0) == 5)
  }

  test("Critical may evict critical without contention") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 10);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 25) // try to use set 0
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 25) // try again
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 5)
  }

  test("Critical may evict critical without contention 2") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 25) // try to use set 0
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 25) // try again
    assert(cache.getCacheLine(8,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(12,1) == 5)
  }

  test("Critical hit on non-critical line increases contention limit") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(4,0) == 5) // try to use set 0 with critical

    assert(cache.getCacheLine(10,1) == 25) // try to overwrite set 1 with non-critical
    assert(cache.getCacheLine(10,1) == 5) // Ensure was saved
    assert(cache.getCacheLine(6,1) == 5)
  }

  test("Critical hit on non-critical line makes it critical") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(0,0) == 5) // try to use set 0 with critical
    assert(cache.getCacheLine(10,1) == 25) // overwrite set 1 with non-critical, negating the previous win
    assert(cache.getCacheLine(8,0) == 25) // overwrite set 0 way 1 with critical

    assert(cache.getCacheLine(12,1) == 25) // try to use set 0 with non-critical
    assert(cache.getCacheLine(12,1) == 25) //
  }

  test("Critical hit on critical line does not increase contention count") {
    val cache = createInstance(2,2,2,5, 25);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0 with crit 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1
    // Fill up set 1 with crit 1
    cache.getCacheLine(2,1) // way 0
    cache.getCacheLine(6,1) // way 1

    assert(cache.getCacheLine(3,0) == 5) // Hit on other crit

    assert(cache.getCacheLine(8,2) == 25) // Try to evict crit
    assert(cache.getCacheLine(8,2) == 25) // Ensure did not work
    assert(cache.getCacheLine(0,0) == 5) // Ensure was not evicted
    assert(cache.getCacheLine(4,0) == 5) // Ensure was not evicted
  }

  test("Critical can only evict own partition") {
    val cache = createInstance(2,2,2,10,60);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,1) == 60) // Try to evict
    assert(cache.getCacheLine(0,0) == 10) // Ensure evicted self
    assert(cache.getCacheLine(4,1) == 60) // Ensure evicted self
  }

  test("Critical can only evict own partition 2") {
    val cache = createInstance(2,2,2,10,60);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,1) == 60) // Try to evict
    assert(cache.getCacheLine(0,0) == 10) // Ensure evicted self
    assert(cache.getCacheLine(4,1) == 60) // Ensure evicted self
  }

  test("Critical can only evict own partition 3") {
    val cache = createInstance(2,2,2,10,60);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 60) // Try to evict
    assert(cache.getCacheLine(8,1) == 60) // Ensure did not work
  }

  test("Critical can evict unassigned way") {
    val cache = createInstance(2,2,2,10,60);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,2) // way 1

    assert(cache.getCacheLine(8,1) == 60) // Try to evict
    assert(cache.getCacheLine(8,1) == 10) // Ensure did work
    assert(cache.getCacheLine(0,0) == 10) // Ensure did not get evicted
  }

  test("Non-critical can evict unlimited assigned way") {
    val cache = createInstance(2,2,2,10,60);
    cache.setCriticality(0, 200);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == 60) // Try to evict
    assert(cache.getCacheLine(8,1) == 10) // Ensure did work
  }


}