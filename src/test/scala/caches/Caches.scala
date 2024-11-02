package caches

import org.scalatest.funsuite.AnyFunSuite

trait SimpleCacheTests[C<: SoftCache] extends AnyFunSuite {
  def className: String
  def createInstance(lineLength: Int, ways: Int, sets: Int): C

  test("Capacity") {
    assert(createInstance(2,2,2).capacity == 8)
    assert(createInstance(4,2,2).capacity == 16)
    assert(createInstance(2,4,2).capacity == 16)
    assert(createInstance(2,2,4).capacity == 16)
  }

  test("Get second in line") {
    val cache = createInstance(2,2,2);
    assert(cache.getCacheLine(0,0) == false)
    assert(cache.getCacheLine(1,0) == true)
    assert(cache.getCacheLine(2,0) == false)
    assert(cache.getCacheLine(3,0) == true)
  }

  test("Get third in line") {
    val cache = createInstance(4,4,2);
    assert(cache.getCacheLine(0,0) == false)
    assert(cache.getCacheLine(1,0) == true)
    assert(cache.getCacheLine(2,0) == true)
    assert(cache.getCacheLine(3,0) == true)
    assert(cache.getCacheLine(4,0) == false)
    assert(cache.getCacheLine(5,0) == true)
    assert(cache.getCacheLine(6,0) == true)
    assert(cache.getCacheLine(7,0) == true)
  }

  test("More sets than ways") {
    val cache = createInstance(3,2,3);
    assert(cache.getCacheLine(0,0) == false)
    assert(cache.getCacheLine(9,0) == false)
    assert(cache.getCacheLine(18,0) == false)
  }

  test("Get Cold Addresses") {
    val cache = createInstance(2,4,4);
    assert(cache.getCacheLine(0,0) == false)
    assert(cache.getCacheLine(2,0) == false)
    assert(cache.getCacheLine(4,0) == false)
    assert(cache.getCacheLine(6,0) == false)
  }

  test("Without Refill") {
    val cache = createInstance(2,4,4);
    assert(cache.getCacheLine(0,0, false) == false)
    assert(cache.getCacheLine(0,0, false) == false)
    assert(cache.getCacheLine(0,0, false) == false)
    assert(cache.getCacheLine(0,0, false) == false)
  }

}

trait LruTests[C<: SoftCache with LRUReplacement[_]] extends SimpleCacheTests[C]
{
  test("Replace LRU 1") {
    val cache = createInstance(2,2,2);
    cache.getCacheLine(0,0) // set 0, way 0
    cache.getCacheLine(2,0) // set 1, way 0
    cache.getCacheLine(4,0) // set 0, way 1
    cache.getCacheLine(6,0) // set 1, way 1

    // Set 0 is full, with '0' being least recently used
    assert(cache.getCacheLine(8,0) == false)
    assert(cache.getCacheLine(5,0) == true)
    assert(cache.getCacheLine(1,0) == false)
    assert(cache.getCacheLine(9,0) == false)
  }

  test("Replace LRU 2") {
    val cache = createInstance(4,3,3);
    cache.getCacheLine(12,0) // set 0, way 0
    cache.getCacheLine(0,0) // set 0, way 1
    cache.getCacheLine(24,0) // set 0, way 2

    cache.getCacheLine(12,0) // set 0, way 0
    cache.getCacheLine(0,0) // set 0, way 1
    cache.getCacheLine(24,0) // set 0, way 2

    // Set 0 is full, with '12' being least recently used
    assert(cache.getCacheLine(36,0) == false)
    assert(cache.getCacheLine(3,0) == true)
    assert(cache.getCacheLine(25,0) == true)
    assert(cache.getCacheLine(14,0) == false)
  }
}

trait PartitionedTests[C<: SoftCache with PartitionedReplacement[_]] extends SimpleCacheTests[C]
{
  test("Evict only own partition") {
    val cache = createInstance(3,3,3);
    cache.assignWay(0, 0); // Each core gets a way
    cache.assignWay(1, 1);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    assert(cache.getCacheLine(27,2) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(9,1) == true) // Ensure was not evicted
  }

  test("May evict unassigned ways") {
    val cache = createInstance(3,3,3);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    assert(cache.getCacheLine(27,2) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == true) // Ensure did get saved

    assert(cache.getCacheLine(9,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(18,1) == true) // Ensure was not evicted
  }

  test("May hit in other's ways") {
    val cache = createInstance(3,3,3);
    cache.assignWay(0, 0); // Each core gets a way
    cache.assignWay(1, 1);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    // Each hits in the others'
    assert(cache.getCacheLine(9,0) == true)
    assert(cache.getCacheLine(18,0) == true)

    assert(cache.getCacheLine(0,1) == true)
    assert(cache.getCacheLine(18,1) == true)

    assert(cache.getCacheLine(0,2) == true)
    assert(cache.getCacheLine(9,2) == true)
  }
}

class LruCacheTest extends AnyFunSuite with LruTests[LruCache] {
  override def className: String = "LruCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int): LruCache = {
    new LruCache(lineLength, ways, sets)
  }
}

class PartitionedCacheTest extends AnyFunSuite with LruTests[PartitionedCache] with PartitionedTests[PartitionedCache] {
  override def className: String = "PartitionedCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int): PartitionedCache = {
    new PartitionedCache(lineLength, ways, sets)
  }
}

class ContentionCacheTest extends AnyFunSuite with LruTests[ContentionCache] {
  override def className: String = "ContentionCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int): ContentionCache = {
    new ContentionCache(lineLength, ways, sets, 0)
  }
  def createInstance(lineLength: Int, ways: Int, sets: Int, contentionCost:Int): ContentionCache = {
    new ContentionCache(lineLength, ways, sets, contentionCost)
  }

  test("Low criticality cannot evict high criticality at limit") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == false) // Ensure did not get saved

    assert(cache.getCacheLine(0,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(9,1) == true)
    assert(cache.getCacheLine(18,1) == true)
  }

  test("Low criticality cannot evict high criticality with contention lower than cost") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 3);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == false) // Ensure did not get saved

    assert(cache.getCacheLine(0,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(9,1) == true)
    assert(cache.getCacheLine(18,1) == true)
  }

  test("Low criticality evicts other low-criticality if high-criticality at limit") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,1) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure critical was not evicted
    assert(cache.getCacheLine(18,1) == true)
    assert(cache.getCacheLine(9,1) == false)
  }

  test("Low criticality can evict high criticality not at limit") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved
  }

  test("Low criticality can evict high criticality not at limit 2") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,2) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == true) // Ensure did get saved
    assert(cache.getCacheLine(0,1) == true) // Ensure limited was saved
    assert(cache.getCacheLine(9,0) == false) // Ensure limited was saved
  }

  test("Unlimited eviction reaches limit") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 10);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2
    // Fill up set 1
    cache.getCacheLine(3,0) // way 0
    cache.getCacheLine(12,0) // way 1
    cache.getCacheLine(21,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == false) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == false) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Unlimited eviction reaches limit 2") {
    val cache = createInstance(3,3,3,10);
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

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == false) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(33,1) == false) // try to use set 2 again
    assert(cache.getCacheLine(33,1) == false) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Critical can evict non-critical") {
    val cache = createInstance(4,4,4,10);
    cache.setCriticality(1, 20);

    // Fill up set 1 with non-critical
    cache.getCacheLine(4,0) // way 0
    cache.getCacheLine(20,0) // way 1
    cache.getCacheLine(36,0) // way 2
    cache.getCacheLine(52,0) // way 3

    assert(cache.getCacheLine(68,1) == false) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == false) // Ensure non-critical was evicted
  }

  test("Critical not at limit may be evicted") {
    val cache = createInstance(4,4,4,20);
    cache.setCriticality(1, 20);

    // Fill up set 1
    cache.getCacheLine(4,1) // way 0, critical
    cache.getCacheLine(20,0) // way 1, non-critical
    cache.getCacheLine(36,0) // way 2, non-critical
    cache.getCacheLine(52,0) // way 3, non-critical

    assert(cache.getCacheLine(68,1) == false) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == true) // Ensure new access did get saved
    assert(cache.getCacheLine(20,0) == true) // Ensure non-LRU stay saved
    assert(cache.getCacheLine(36,0) == true)
    assert(cache.getCacheLine(52,0) == true)

    assert(cache.getCacheLine(84,1) == false) // Try another critical load
    assert(cache.getCacheLine(69,1) == true) // Ensure critical was not evicted because it was at limit
    assert(cache.getCacheLine(36,0) == true)// Ensure non-LRU stay saved
    assert(cache.getCacheLine(52,0) == true)
  }

  test("Critical may evict critical without contention") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 10);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // try to use set 0
    assert(cache.getCacheLine(8,1) == true) // Ensure was saved
    assert(cache.getCacheLine(12,1) == false) // try again
    assert(cache.getCacheLine(8,1) == true) // Ensure was saved
    assert(cache.getCacheLine(12,1) == true)
  }

  test("Critical may evict critical without contention 2") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // try to use set 0
    assert(cache.getCacheLine(8,1) == true) // Ensure was saved
    assert(cache.getCacheLine(12,1) == false) // try again
    assert(cache.getCacheLine(8,1) == true) // Ensure was saved
    assert(cache.getCacheLine(12,1) == true)
  }

  test("Critical hit on non-critical line increases contention limit") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(4,0) == true) // try to use set 0 with critical

    assert(cache.getCacheLine(10,1) == false) // try to overwrite set 1 with non-critical
    assert(cache.getCacheLine(10,1) == true) // Ensure was saved
    assert(cache.getCacheLine(6,1) == true)
  }

  test("Critical hit on non-critical line makes it critical") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(0,0) == true) // try to use set 0 with critical
    assert(cache.getCacheLine(10,1) == false) // overwrite set 1 with non-critical, negating the previous win
    assert(cache.getCacheLine(8,0) == false) // overwrite set 0 way 1 with critical

    assert(cache.getCacheLine(12,1) == false) // try to use set 0 with non-critical
    assert(cache.getCacheLine(12,1) == false) //
  }

  test("Critical hit on critical line does not increase contention count") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0 with crit 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1
    // Fill up set 1 with crit 1
    cache.getCacheLine(2,1) // way 0
    cache.getCacheLine(6,1) // way 1

    assert(cache.getCacheLine(3,0) == true) // Hit on other crit

    assert(cache.getCacheLine(8,2) == false) // Try to evict crit
    assert(cache.getCacheLine(8,2) == false) // Ensure did not work
    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(4,0) == true) // Ensure was not evicted
  }
}

class TimeoutCacheTest extends AnyFunSuite with LruTests[TimeoutCache] {
  override def className: String = "TimeoutCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int): TimeoutCache = {
    new TimeoutCache(lineLength, ways, sets, 0)
  }
  def createInstance(lineLength: Int, ways: Int, sets: Int, timeout: Int): TimeoutCache = {
    new TimeoutCache(lineLength, ways, sets, timeout)
  }

  test("Low criticality cannot evict high criticality before timeout") {
    val cache = createInstance(2,2,2,2);
    cache.setPriority(0, 0); // Assign all to core 0
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(8,1) == false) // Ensure did not get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure did get saved
    assert(cache.getCacheLine(4,0) == true)
  }

  test("Low criticality can evict high-criticality in in unprioritized way") {
    val cache = createInstance(2,2,2,2);
    cache.setPriority(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,0) // way 1, without prio

    assert(cache.getCacheLine(8,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(8,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure did get saved
  }

  test("High-criticality prefers timedout ways") {
    val cache = createInstance(2,3,2,2);
    cache.setPriority(0, 0);
    cache.setPriority(1, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,1) // way 1, with prio
    cache.getCacheLine(8,2) // way 2, without prio

    assert(cache.getCacheLine(16,0) == false) // try to use set 0 again
    assert(cache.getCacheLine(16,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure did get saved
    assert(cache.getCacheLine(4,1) == true) // Ensure did get saved
  }

  test("High-criticality prefers own ways") {
    val cache = createInstance(2,3,2,2);
    cache.setPriority(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0, without prio
    cache.getCacheLine(4,1) // way 1, with prio
    cache.getCacheLine(8,1) // way 2, without prio
    cache.getCacheLine(0,1) // make sure way 0 isn't LRU

    assert(cache.getCacheLine(16,0) == false) // try to use set 0 again
    assert(cache.getCacheLine(16,0) == true) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == true) // Ensure did get saved
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved
  }

  test("Cycles reduce timeout") {
    val cache = createInstance(2,2,2,1);
    cache.setPriority(0, 0);
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,0) // way 1, with prio

    assert(cache.getCacheLine(8,1) == false) // try to use without prio
    assert(cache.getCacheLine(8,1) == false) // Ensure didn't get saved

    cache.advanceCycle() // Make the timeouts run out

    assert(cache.getCacheLine(8,1) == false) // try to use without prio
    assert(cache.getCacheLine(12,1) == false) // try to use without prio
    assert(cache.getCacheLine(8,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(12,1) == true) // Ensure did get saved
  }

  test("Timeout as given") {
    val cache = createInstance(2,2,2,5);
    cache.setPriority(0, 0);
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0, with prio
    cache.getCacheLine(4,0) // way 1, with prio


    cache.advanceCycle() // Make timout reach 1
    cache.advanceCycle()
    cache.advanceCycle()
    cache.advanceCycle()

    assert(cache.getCacheLine(8,1) == false) // Ensure timeout hasn't been reached
    assert(cache.getCacheLine(8,1) == false) // Ensure didn't get saved

    cache.advanceCycle() // Timeout reached

    assert(cache.getCacheLine(8,1) == false) // Ensure didn't get saved
    assert(cache.getCacheLine(12,1) == false) // try to use without prio
    assert(cache.getCacheLine(8,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(12,1) == true) // Ensure did get saved
  }

}

class ContentionPartCacheTest extends AnyFunSuite with LruTests[ContentionPartCache] with PartitionedTests[ContentionPartCache] {
  override def className: String = "ContentionPartCache"

  override def createInstance(lineLength: Int, ways: Int, sets: Int): ContentionPartCache = {
    new ContentionPartCache(lineLength, ways, sets, 0)
  }

  def createInstance(lineLength: Int, ways: Int, sets: Int, contentionCost: Int): ContentionPartCache = {
    new ContentionPartCache(lineLength, ways, sets, contentionCost)
  }


  test("Low criticality cannot evict high criticality at limit") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == false) // Ensure did not get saved

    assert(cache.getCacheLine(0,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(9,1) == true)
    assert(cache.getCacheLine(18,1) == true)
  }

  test("Low criticality cannot evict high criticality with contention lower than cost") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 3);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == false) // Ensure did not get saved

    assert(cache.getCacheLine(0,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(9,1) == true)
    assert(cache.getCacheLine(18,1) == true)
  }

  test("Low criticality evicts other low-criticality if high-criticality at limit") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,1) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure critical was not evicted
    assert(cache.getCacheLine(18,1) == true)
    assert(cache.getCacheLine(9,1) == false)
  }

  test("Low criticality can evict high criticality not at limit") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved
  }

  test("Low criticality can evict high criticality not at limit 2") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2

    assert(cache.getCacheLine(27,2) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == true) // Ensure did get saved
    assert(cache.getCacheLine(0,1) == true) // Ensure limited was saved
    assert(cache.getCacheLine(9,0) == false) // Ensure limited was saved
  }

  test("Unlimited eviction reaches limit") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 10);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,0) // way 1
    cache.getCacheLine(18,0) // way 2
    // Fill up set 1
    cache.getCacheLine(3,0) // way 0
    cache.getCacheLine(12,0) // way 1
    cache.getCacheLine(21,0) // way 2

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == false) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == false) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Unlimited eviction reaches limit 2") {
    val cache = createInstance(3,3,3,10);
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

    assert(cache.getCacheLine(27,1) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(30,1) == false) // try to use set 1 again
    assert(cache.getCacheLine(30,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(33,1) == false) // try to use set 2 again
    assert(cache.getCacheLine(33,1) == false) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Critical can evict non-critical") {
    val cache = createInstance(4,4,4,10);
    cache.setCriticality(1, 20);

    // Fill up set 1 with non-critical
    cache.getCacheLine(4,0) // way 0
    cache.getCacheLine(20,0) // way 1
    cache.getCacheLine(36,0) // way 2
    cache.getCacheLine(52,0) // way 3

    assert(cache.getCacheLine(68,1) == false) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == true) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == false) // Ensure non-critical was evicted
  }

  test("Critical not at limit may be evicted") {
    val cache = createInstance(4,4,4,20);
    cache.setCriticality(1, 20);

    // Fill up set 1
    cache.getCacheLine(4,1) // way 0, critical
    cache.getCacheLine(20,0) // way 1, non-critical
    cache.getCacheLine(36,0) // way 2, non-critical
    cache.getCacheLine(52,0) // way 3, non-critical

    assert(cache.getCacheLine(68,1) == false) // try to use set 1 for critical

    assert(cache.getCacheLine(69,1) == true) // Ensure new access did get saved
    assert(cache.getCacheLine(20,0) == true) // Ensure non-LRU stay saved
    assert(cache.getCacheLine(36,0) == true)
    assert(cache.getCacheLine(52,0) == true)

    assert(cache.getCacheLine(84,1) == false) // Try another critical load
    assert(cache.getCacheLine(69,1) == true) // Ensure critical was not evicted because it was at limit
    assert(cache.getCacheLine(36,0) == true)// Ensure non-LRU stay saved
    assert(cache.getCacheLine(52,0) == true)
  }

  test("Critical hit on non-critical line increases contention limit") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(4,0) == true) // try to use set 0 with critical

    assert(cache.getCacheLine(10,1) == false) // try to overwrite set 1 with non-critical
    assert(cache.getCacheLine(10,1) == true) // Ensure was saved
    assert(cache.getCacheLine(6,1) == true)
  }

  test("Critical hit on non-critical line makes it critical") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,1) // way 1
    // Fill up set 1 with critical
    cache.getCacheLine(2,0) // way 0
    cache.getCacheLine(6,0) // way 1

    assert(cache.getCacheLine(0,0) == true) // try to use set 0 with critical
    assert(cache.getCacheLine(10,1) == false) // overwrite set 1 with non-critical, negating the previous win
    assert(cache.getCacheLine(8,0) == false) // overwrite set 0 way 1 with critical

    assert(cache.getCacheLine(12,1) == false) // try to use set 0 with non-critical
    assert(cache.getCacheLine(12,1) == false) //
  }

  test("Critical hit on critical line does not increase contention count") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0 with crit 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1
    // Fill up set 1 with crit 1
    cache.getCacheLine(2,1) // way 0
    cache.getCacheLine(6,1) // way 1

    assert(cache.getCacheLine(3,0) == true) // Hit on other crit

    assert(cache.getCacheLine(8,2) == false) // Try to evict crit
    assert(cache.getCacheLine(8,2) == false) // Ensure did not work
    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(4,0) == true) // Ensure was not evicted
  }

  ////////////////////////////////////////////////////////////////////////////////////

  test("Critical can only evict own partition") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(0,0) == true) // Ensure evicted self
    assert(cache.getCacheLine(4,1) == false) // Ensure evicted self
  }

  test("Critical can only evict own partition 2") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(0,0) == true) // Ensure evicted self
    assert(cache.getCacheLine(4,1) == false) // Ensure evicted self
  }

  test("Critical can only evict own partition 3") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(8,1) == false) // Ensure did not work
  }

  test("Critical can only evict own partition 4") {
    val cache = createInstance(2,4,2,50);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);
    cache.assignWay(0,0);
    cache.assignWay(0,1);
    cache.assignWay(1,2);
    cache.assignWay(1,3);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1
    cache.getCacheLine(8,1) // way 2
    cache.getCacheLine(12,1) // way 3


    assert(cache.getCacheLine(16,1) == false) // Try to evict
    assert(cache.getCacheLine(16,1) == true) // Ensure evicted self
    assert(cache.getCacheLine(0,0) == true)
    assert(cache.getCacheLine(4,0) == true)
    assert(cache.getCacheLine(12,1) == true)
  }

  test("Critical can evict unassigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,2) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(8,1) == true) // Ensure did work
    assert(cache.getCacheLine(0,0) == true) // Ensure did not get evicted
  }

  test("Non-critical can evict unlimited assigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 50);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(8,1) == true) // Ensure did work
    assert(cache.getCacheLine(4,0) == true)
  }

  test("Non-critical can evict non-critical from limited assigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(8,1) == true) // Ensure can evict non-critical
    assert(cache.getCacheLine(0,0) == true) // Ensure critical was not evicted
  }

  test("Non-critical cannot evict limited assigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict
    assert(cache.getCacheLine(8,1) == false) // Ensure did not work
    assert(cache.getCacheLine(0,0) == true)
    assert(cache.getCacheLine(4,0) == true)
  }

  test("Limited critical self-eviction") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 100);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,4) // way 1

    assert(cache.getCacheLine(8,0) == false) // Try to evict self
    assert(cache.getCacheLine(8,0) == true) // Ensure did work
    assert(cache.getCacheLine(4,4) == true) // Ensure did not use other partition
  }

  test("Non-critical evict of critical in non-assigned") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 50);
    cache.assignWay(0,0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try to evict with on-critical
    assert(cache.getCacheLine(8,1) == true) // Ensure did work
    assert(cache.getCacheLine(4,0) == true)

    assert(cache.getCacheLine(12,1) == false) // Try to evict with on-critical again
    assert(cache.getCacheLine(12,1) == true) // Ensure did not evict critical
    assert(cache.getCacheLine(4,0) == true)
    assert(cache.getCacheLine(8,1) == false)
  }

  test("Evict only own partition 2") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 10)
    cache.setCriticality(1, 10)
    cache.setCriticality(2, 10)
    cache.assignWay(0, 0); // Each core gets a way
    cache.assignWay(1, 1);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(9,1) // way 1
    cache.getCacheLine(18,2) // way 2

    assert(cache.getCacheLine(27,2) == false) // try to use set 0 again
    assert(cache.getCacheLine(27,2) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(9,1) == true) // Ensure was not evicted
  }

  test("Limited critical prefer evicting non-critical in own partition") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 0)
    cache.assignWay(0, 0);
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,0) == false) // Get critical
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
  }

  test("Unlimited critical doesn't prefer evicting non-critical in own partition") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 0);
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,0) == false) // Get critical
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved
    assert(cache.getCacheLine(4,1) == true) // Ensure was not evicted

    assert(cache.getCacheLine(12,0) == false) // Try again, now as limited
    assert(cache.getCacheLine(12,0) == true) // Try again, now as limited
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved
  }

  test("Unlimited critical doesn't prefer evicting non-critical in unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,0) == false) // Get critical
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved
    assert(cache.getCacheLine(4,1) == true) // Ensure was not evicted

    assert(cache.getCacheLine(12,0) == false) // Try again, now limited
    assert(cache.getCacheLine(12,0) == true) // Ensure did get saved
    assert(cache.getCacheLine(8,0) == true) // Ensure was not evicted
  }

  test("Limited critical prefers evicting non-critical in unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 0)
    cache.assignWay(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,1) // way 1

    assert(cache.getCacheLine(8,0) == false) // Get critical
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved

    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
  }

  test("Critical may evict unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,1) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,0) == false) // Get critical
    assert(cache.getCacheLine(8,0) == true) // Ensure did get saved

    assert(cache.getCacheLine(4,0) == true) // Ensure was not evicted
  }

  test("Non-critical triggers contention if evicting critical in unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // Get non-critical
    assert(cache.getCacheLine(8,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(4,0) == true) // Ensure was not evicted

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1

    assert(cache.getCacheLine(8,1) == false) // Try again
    assert(cache.getCacheLine(8,1) == false) // Ensure failed because critical is now limited
    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(4,0) == true) // Ensure was not evicted

  }

  test("Non-critical evicts non-critical in uassigned with other limited critical") {
    val cache = createInstance(2,3,2,10);
    cache.setCriticality(0, 0)
    cache.assignWay(0, 0);

    // Fill up set 0
    cache.getCacheLine(0,0) // way 0
    cache.getCacheLine(4,0) // way 1
    cache.getCacheLine(8,1) // way 1

    assert(cache.getCacheLine(12,1) == false) // Get non-critical
    assert(cache.getCacheLine(12,1) == true) // Ensure did get saved
    assert(cache.getCacheLine(0,0) == true) // Ensure was not evicted
    assert(cache.getCacheLine(4,0) == true) // Ensure was not evicted
  }

}

class CacheTrafficTest extends AnyFunSuite {
  var rand: scala.util.Random = new scala.util.Random;

  test("No Accesses") {
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(1,1,Array(new NoTraffic(1)), (_)=>None),
      new MainMemory(1),
      (_,_) => (),
    )

    for (_ <- 0 until rand.nextInt(10)) {
      assert(cache.requestMemoryAccess().isEmpty)
      cache.triggerCycle()
    }
  }

  test("Serve hit immediately") {
    var wasHit: Option[Boolean] = None
    var done = false
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(new TraceTraffic(1, Array(
          new MemAccess(0,true, 20, 0)
        ).toIterator,(_) => ())),
        (_) => {
          done = true
          None
        }
      ),
      new MainMemory(1),
      (_, isHit) => {
        wasHit = Some(isHit)
      }
    )

    cache.triggerCycle() // One cycle for request
    assert(!cache.isDone() && !done)
    cache.triggerCycle() // One cycle for cache reply
    assert(!cache.isDone() && !done)
    assert(wasHit.contains(true))
    cache.triggerCycle() // One cycle for bus response
    assert(cache.isDone() && done)
    assert(wasHit.contains(true))
  }

  test("Serve miss after external serve") {
    var wasHit: Option[Boolean] = None
    var done = false
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(new TraceTraffic(1, Array(
          new MemAccess(0,true, 20, 0)
        ).toIterator,(_) => ())),
        (_) => {
          done = true
          None
        }
      ),
      new LruCache(1,1,1),
      (_, isHit) => {
        wasHit = Some(isHit)
      }
    )

    assert(cache.requestMemoryAccess().contains((20,())))

    // Wait for external reply
    val randLatency = 1+rand.nextInt(20)
    for(i <- 0 until randLatency) {
      assert(wasHit.contains(false))
      assert(!cache.isDone() && !done)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    cache.triggerCycle() // One cycle for cache response
    assert(!cache.isDone() && !done)
    assert(wasHit.contains(false))
    cache.triggerCycle() // One cycle for bus response
    assert(cache.isDone() && done)
    assert(wasHit.contains(false))
  }

  test("Refill after external") {
    var lruCache = new LruCache(1, 1, 1)
    lruCache.getCacheLine(0,0) // Prefill cache line
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(new SingleTraffic(1,100)),
        (_) => None
      ),
      lruCache,
      (_, isHit) => {}
    )

    assert(cache.requestMemoryAccess().contains((100,())))
    assert(lruCache.getCacheLine(0,0,false)) // Ensure the miss didn't evict the prefilled line already
    cache.triggerCycle()
    assert(lruCache.getCacheLine(0,0,false))
    cache.serveMemoryAccess(())
    assert(!lruCache.getCacheLine(0,0,false)) // After serve, fill should happen
  }

}
