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
    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(1,0,true,true) == ReadHit)
    assert(cache.performAccess(2,0,true,true) == ReadMiss)
    assert(cache.performAccess(3,0,true,true) == ReadHit)
  }

  test("Get third in line") {
    val cache = createInstance(4,4,2);
    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(1,0,true,true) == ReadHit)
    assert(cache.performAccess(2,0,true,true) == ReadHit)
    assert(cache.performAccess(3,0,true,true) == ReadHit)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(5,0,true,true) == ReadHit)
    assert(cache.performAccess(6,0,true,true) == ReadHit)
    assert(cache.performAccess(7,0,true,true) == ReadHit)
  }

  test("More sets than ways") {
    val cache = createInstance(3,2,3);
    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(9,0,true,true) == ReadMiss)
    assert(cache.performAccess(18,0,true,true) == ReadMiss)
  }

  test("Get Cold Addresses") {
    val cache = createInstance(2,4,4);
    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(2,0,true,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(6,0,true,true) == ReadMiss)
  }

  test("Without Refill") {
    val cache = createInstance(2,4,4);
    assert(cache.performAccess(0,0,true,false) == ReadMiss)
    assert(cache.performAccess(0,0,true,false) == ReadMiss)
    assert(cache.performAccess(0,0,true,false) == ReadMiss)
    assert(cache.performAccess(0,0,true,false) == ReadMiss)
  }

  test("Write back 1") {
    val cache = createInstance(2,4,2);
    assert(cache.performAccess(0,0,false,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadMiss)

    assert(cache.performAccess(16,0,true,true) == WriteBack(0))
  }

  test("Write back 2") {
    val cache = createInstance(2,4,2);
    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadMiss)

    assert(cache.performAccess(16,0,false,true) == ReadMiss)

    assert(cache.performAccess(4,0,false,true) == ReadHit)
    assert(cache.performAccess(8,0,true,true) == ReadHit)
    assert(cache.performAccess(12,0,true,true) == ReadHit)
    assert(cache.performAccess(16,0,true,true) == ReadHit)

    assert(cache.performAccess(20,0,true,true) == WriteBack(4))
  }

  test("Write back 3") {
    val cache = createInstance(2,4,2);
    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadMiss)

    assert(cache.performAccess(16,0,false,true) == ReadMiss)

    assert(cache.performAccess(4,0,false,true) == ReadHit)
    assert(cache.performAccess(8,0,true,true) == ReadHit)
    assert(cache.performAccess(12,0,true,true) == ReadHit)

    assert(cache.performAccess(20,0,true,true) == WriteBack(16))
  }

  test("Write back without refill") {
    val cache = createInstance(2,4,2);
    assert(cache.performAccess(0,0,false,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadMiss)

    assert(cache.performAccess(16,0,false,false) == WriteBack(0))

    assert(cache.performAccess(0,0,true,true) == ReadHit)
    assert(cache.performAccess(4,0,false,true) == ReadHit)
    assert(cache.performAccess(8,0,true,true) == ReadHit)
    assert(cache.performAccess(12,0,true,true) == ReadHit)
  }

  test("Evict line") {
    val cache = createInstance(2,4,2);
    assert(cache.performAccess(0,0,false,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadMiss)

    cache.evict(8);

    assert(cache.performAccess(0,0,true,true) == ReadHit)
    assert(cache.performAccess(4,0,true,true) == ReadHit)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadHit)
  }

  test("Evict line 2") {
    val cache = createInstance(2,4,2);
    assert(cache.performAccess(0,0,false,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadMiss)
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(12,0,true,true) == ReadMiss)

    cache.evict(0);

    assert(cache.performAccess(0,0,true,true) == ReadMiss)
    assert(cache.performAccess(4,0,true,true) == ReadHit)
    assert(cache.performAccess(8,0,true,true) == ReadHit)
    assert(cache.performAccess(12,0,true,true) == ReadHit)
  }

}

trait LruTests[C<: SoftCache with LRUReplacement[_]] extends SimpleCacheTests[C]
{
  test("Replace LRU 1") {
    val cache = createInstance(2,2,2);
    cache.performAccess(0,0,true,true) // set 0, way 0
    cache.performAccess(2,0,true,true) // set 1, way 0
    cache.performAccess(4,0,true,true) // set 0, way 1
    cache.performAccess(6,0,true,true) // set 1, way 1

    // Set 0 is full, with '0' being least recently used
    assert(cache.performAccess(8,0,true,true) == ReadMiss)
    assert(cache.performAccess(5,0,true,true) == ReadHit)
    assert(cache.performAccess(1,0,true,true) == ReadMiss)
    assert(cache.performAccess(9,0,true,true) == ReadMiss)
  }

  test("Replace LRU 2") {
    val cache = createInstance(4,3,3);
    cache.performAccess(12,0,true,true) // set 0, way 0
    cache.performAccess(0,0,true,true) // set 0, way 1
    cache.performAccess(24,0,true,true) // set 0, way 2

    cache.performAccess(12,0,true,true) // set 0, way 0
    cache.performAccess(0,0,true,true) // set 0, way 1
    cache.performAccess(24,0,true,true) // set 0, way 2

    // Set 0 is full, with '12' being least recently used
    assert(cache.performAccess(36,0,true,true) == ReadMiss)
    assert(cache.performAccess(3,0,true,true) == ReadHit)
    assert(cache.performAccess(25,0,true,true) == ReadHit)
    assert(cache.performAccess(14,0,true,true) == ReadMiss)
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
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,1,true,true) // way 1
    cache.performAccess(18,2,true,true) // way 2

    assert(cache.performAccess(27,2,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,2,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(9,1,true,true) == ReadHit) // Ensure was not evicted
  }

  test("May evict unassigned ways") {
    val cache = createInstance(3,3,3);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,1,true,true) // way 1
    cache.performAccess(18,2,true,true) // way 2

    assert(cache.performAccess(27,2,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,2,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(9,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(18,1,true,true) == ReadHit) // Ensure was not evicted
  }

  test("May hit in other's ways") {
    val cache = createInstance(3,3,3);
    cache.assignWay(0, 0); // Each core gets a way
    cache.assignWay(1, 1);
    cache.assignWay(2, 2);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,1,true,true) // way 1
    cache.performAccess(18,2,true,true) // way 2

    // Each hits in the others'
    assert(cache.performAccess(9,0,true,true) == ReadHit)
    assert(cache.performAccess(18,0,true,true) == ReadHit)

    assert(cache.performAccess(0,1,true,true) == ReadHit)
    assert(cache.performAccess(18,1,true,true) == ReadHit)

    assert(cache.performAccess(0,2,true,true) == ReadHit)
    assert(cache.performAccess(9,2,true,true) == ReadHit)
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
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == Reject) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == Reject) // Ensure did not get saved

    assert(cache.performAccess(0,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(9,1,true,true) == ReadHit)
    assert(cache.performAccess(18,1,true,true) == ReadHit)
  }

  test("Low criticality cannot evict high criticality with contention lower than cost") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 3);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == Reject) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == Reject) // Ensure did not get saved

    assert(cache.performAccess(0,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(9,1,true,true) == ReadHit)
    assert(cache.performAccess(18,1,true,true) == ReadHit)
  }

  test("Low criticality evicts other low-criticality if high-criticality at limit") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,1,true,true) // way 1
    cache.performAccess(18,1,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure critical was not evicted
    assert(cache.performAccess(18,1,true,true) == ReadHit)
    assert(cache.performAccess(9,1,true,true) == ReadMiss)
  }

  test("Low criticality can evict high criticality not at limit") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved
  }

  test("Low criticality can evict high criticality not at limit 2") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,2,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,2,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(0,1,true,true) == ReadHit) // Ensure limited was saved
    assert(cache.performAccess(9,0,true,true) == ReadMiss) // Ensure limited was saved
  }

  test("Unlimited eviction reaches limit") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 10);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2
    // Fill up set 1
    cache.performAccess(3,0,true,true) // way 0
    cache.performAccess(12,0,true,true) // way 1
    cache.performAccess(21,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(30,1,true,true) == Reject) // try to use set 1 again
    assert(cache.performAccess(30,1,true,true) == Reject) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Unlimited eviction reaches limit 2") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 20);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2
    // Fill up set 1
    cache.performAccess(3,0,true,true) // way 0
    cache.performAccess(12,0,true,true) // way 1
    cache.performAccess(21,0,true,true) // way 2
    // Fill up set 2
    cache.performAccess(6,0,true,true) // way 0
    cache.performAccess(15,0,true,true) // way 1
    cache.performAccess(24,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(30,1,true,true) == ReadMiss) // try to use set 1 again
    assert(cache.performAccess(30,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(33,1,true,true) == Reject) // try to use set 2 again
    assert(cache.performAccess(33,1,true,true) == Reject) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Critical can evict non-critical") {
    val cache = createInstance(4,4,4,10);
    cache.setCriticality(1, 20);

    // Fill up set 1 with non-critical
    cache.performAccess(4,0,true,true) // way 0
    cache.performAccess(20,0,true,true) // way 1
    cache.performAccess(36,0,true,true) // way 2
    cache.performAccess(52,0,true,true) // way 3

    assert(cache.performAccess(68,1,true,true) == ReadMiss) // try to use set 1 for critical

    assert(cache.performAccess(69,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(4,0,true,true) == ReadMiss) // Ensure non-critical was evicted
  }

  test("Critical not at limit may be evicted") {
    val cache = createInstance(4,4,4,20);
    cache.setCriticality(1, 20);

    // Fill up set 1
    cache.performAccess(4,1,true,true) // way 0, critical
    cache.performAccess(20,0,true,true) // way 1, non-critical
    cache.performAccess(36,0,true,true) // way 2, non-critical
    cache.performAccess(52,0,true,true) // way 3, non-critical

    assert(cache.performAccess(68,1,true,true) == ReadMiss) // try to use set 1 for critical

    assert(cache.performAccess(69,1,true,true) == ReadHit) // Ensure new access did get saved
    assert(cache.performAccess(20,0,true,true) == ReadHit) // Ensure non-LRU stay saved
    assert(cache.performAccess(36,0,true,true) == ReadHit)
    assert(cache.performAccess(52,0,true,true) == ReadHit)

    assert(cache.performAccess(84,1,true,true) == ReadMiss) // Try another critical load
    assert(cache.performAccess(69,1,true,true) == ReadHit) // Ensure critical was not evicted because it was at limit
    assert(cache.performAccess(36,0,true,true) == ReadHit)// Ensure non-LRU stay saved
    assert(cache.performAccess(52,0,true,true) == ReadHit)
  }

  test("Critical may evict critical without contention") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 10);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // try to use set 0
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure was saved
    assert(cache.performAccess(12,1,true,true) == ReadMiss) // try again
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure was saved
    assert(cache.performAccess(12,1,true,true) == ReadHit)
  }

  test("Critical may evict critical without contention 2") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // try to use set 0
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure was saved
    assert(cache.performAccess(12,1,true,true) == ReadMiss) // try again
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure was saved
    assert(cache.performAccess(12,1,true,true) == ReadHit)
  }

  test("Critical hit on non-critical line increases contention limit") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1
    // Fill up set 1 with critical
    cache.performAccess(2,0,true,true) // way 0
    cache.performAccess(6,0,true,true) // way 1

    assert(cache.performAccess(4,0,true,true) == ReadHit) // try to use set 0 with critical

    assert(cache.performAccess(10,1,true,true) == ReadMiss) // try to overwrite set 1 with non-critical
    assert(cache.performAccess(10,1,true,true) == ReadHit) // Ensure was saved
    assert(cache.performAccess(6,1,true,true) == ReadHit)
  }

  test("Critical hit on non-critical line makes it critical") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1
    // Fill up set 1 with critical
    cache.performAccess(2,0,true,true) // way 0
    cache.performAccess(6,0,true,true) // way 1

    assert(cache.performAccess(0,0,true,true) == ReadHit) // try to use set 0 with critical
    assert(cache.performAccess(10,1,true,true) == ReadMiss) // overwrite set 1 with non-critical, negating the previous win
    assert(cache.performAccess(8,0,true,true) == ReadMiss) // overwrite set 0 way 1 with critical

    assert(cache.performAccess(12,1,true,true) == Reject) // try to use set 0 with non-critical
    assert(cache.performAccess(12,1,true,true) == Reject) //
  }

  test("Critical hit on critical line does not increase contention count") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0 with crit 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1
    // Fill up set 1 with crit 1
    cache.performAccess(2,1,true,true) // way 0
    cache.performAccess(6,1,true,true) // way 1

    assert(cache.performAccess(3,0,true,true) == ReadHit) // Hit on other crit

    assert(cache.performAccess(8,2,true,true) == Reject) // Try to evict crit
    assert(cache.performAccess(8,2,true,true) == Reject) // Ensure did not work
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(4,0,true,true) == ReadHit) // Ensure was not evicted
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
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == Reject) // try to use set 0 again
    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure did not get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(4,0,true,true) == ReadHit)
  }

  test("Low criticality can evict high criticality after timeout") {
    val cache = createInstance(2,2,2,2);
    cache.setPriority(0, 0); // Assign all to core 0
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    cache.advanceCycle()
    cache.advanceCycle()

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(4,0,true,true) == ReadHit)
  }

  test("Low criticality can evict high-criticality in in unprioritized way") {
    val cache = createInstance(2,2,2,2);
    cache.setPriority(0, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0, with prio
    cache.performAccess(4,0,true,true) // way 1, without prio

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure did get saved
  }

  test("High-criticality doesn't prefer unprioritised") {
    val cache = createInstance(2,3,2,2);
    cache.setPriority(0, 0);
    cache.setPriority(1, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0, with prio
    cache.performAccess(4,1,true,true) // way 1, with prio
    cache.performAccess(8,2,true,true) // way 2, without prio

    assert(cache.performAccess(16,0,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(16,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(4,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(8,2,true,true) == ReadHit) // Ensure did get saved
  }

  test("Cycles reduce timeout") {
    val cache = createInstance(2,2,2,1);
    cache.setPriority(0, 0);
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0, with prio
    cache.performAccess(4,0,true,true) // way 1, with prio

    assert(cache.performAccess(8,1,true,true) == Reject) // try to use without prio
    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure didn't get saved

    cache.advanceCycle() // Make the timeouts run out

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // try to use without prio
    assert(cache.performAccess(12,1,true,true) == ReadMiss) // try to use without prio
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(12,1,true,true) == ReadHit) // Ensure did get saved
  }

  test("Timeout as given") {
    val cache = createInstance(2,2,2,5);
    cache.setPriority(0, 0);
    cache.setPriority(0, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0, with prio
    cache.performAccess(4,0,true,true) // way 1, with prio


    cache.advanceCycle() // Make timout reach 1
    cache.advanceCycle()
    cache.advanceCycle()
    cache.advanceCycle()

    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure timeout hasn't been reached
    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure didn't get saved

    cache.advanceCycle() // Timeout reached

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Ensure didn't get saved
    assert(cache.performAccess(12,1,true,true) == ReadMiss) // try to use without prio
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(12,1,true,true) == ReadHit) // Ensure did get saved
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
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == Reject) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == Reject) // Ensure did not get saved

    assert(cache.performAccess(0,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(9,1,true,true) == ReadHit)
    assert(cache.performAccess(18,1,true,true) == ReadHit)
  }

  test("Low criticality cannot evict high criticality with contention lower than cost") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 3);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == Reject) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == Reject) // Ensure did not get saved

    assert(cache.performAccess(0,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(9,1,true,true) == ReadHit)
    assert(cache.performAccess(18,1,true,true) == ReadHit)
  }

  test("Low criticality evicts other low-criticality if high-criticality at limit") {
    val cache = createInstance(3,3,3,6);
    cache.setCriticality(0, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,1,true,true) // way 1
    cache.performAccess(18,1,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure critical was not evicted
    assert(cache.performAccess(18,1,true,true) == ReadHit)
    assert(cache.performAccess(9,1,true,true) == ReadMiss)
  }

  test("Low criticality can evict high criticality not at limit") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved
  }

  test("Low criticality can evict high criticality not at limit 2") {
    val cache = createInstance(3,3,3,2);
    cache.setCriticality(0, 100);
    cache.setCriticality(1, 0);

    // Fill up set 0
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2

    assert(cache.performAccess(27,2,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,2,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(0,1,true,true) == ReadHit) // Ensure limited was saved
    assert(cache.performAccess(9,0,true,true) == ReadMiss) // Ensure limited was saved
  }

  test("Unlimited eviction reaches limit") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 10);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2
    // Fill up set 1
    cache.performAccess(3,0,true,true) // way 0
    cache.performAccess(12,0,true,true) // way 1
    cache.performAccess(21,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(30,1,true,true) == Reject) // try to use set 1 again
    assert(cache.performAccess(30,1,true,true) == Reject) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Unlimited eviction reaches limit 2") {
    val cache = createInstance(3,3,3,10);
    cache.setCriticality(0, 20);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,0,true,true) // way 1
    cache.performAccess(18,0,true,true) // way 2
    // Fill up set 1
    cache.performAccess(3,0,true,true) // way 0
    cache.performAccess(12,0,true,true) // way 1
    cache.performAccess(21,0,true,true) // way 2
    // Fill up set 2
    cache.performAccess(6,0,true,true) // way 0
    cache.performAccess(15,0,true,true) // way 1
    cache.performAccess(24,0,true,true) // way 2

    assert(cache.performAccess(27,1,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(30,1,true,true) == ReadMiss) // try to use set 1 again
    assert(cache.performAccess(30,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(33,1,true,true) == Reject) // try to use set 2 again
    assert(cache.performAccess(33,1,true,true) == Reject) // Ensure did not get saved (core 0 reached limit from last eviction)
  }

  test("Critical can evict non-critical") {
    val cache = createInstance(4,4,4,10);
    cache.setCriticality(1, 20);

    // Fill up set 1 with non-critical
    cache.performAccess(4,0,true,true) // way 0
    cache.performAccess(20,0,true,true) // way 1
    cache.performAccess(36,0,true,true) // way 2
    cache.performAccess(52,0,true,true) // way 3

    assert(cache.performAccess(68,1,true,true) == ReadMiss) // try to use set 1 for critical

    assert(cache.performAccess(69,1,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(4,0,true,true) == ReadMiss) // Ensure non-critical was evicted
  }

  test("Critical not at limit may be evicted") {
    val cache = createInstance(4,4,4,20);
    cache.setCriticality(1, 20);

    // Fill up set 1
    cache.performAccess(4,1,true,true) // way 0, critical
    cache.performAccess(20,0,true,true) // way 1, non-critical
    cache.performAccess(36,0,true,true) // way 2, non-critical
    cache.performAccess(52,0,true,true) // way 3, non-critical

    assert(cache.performAccess(68,1,true,true) == ReadMiss) // try to use set 1 for critical

    assert(cache.performAccess(69,1,true,true) == ReadHit) // Ensure new access did get saved
    assert(cache.performAccess(20,0,true,true) == ReadHit) // Ensure non-LRU stay saved
    assert(cache.performAccess(36,0,true,true) == ReadHit)
    assert(cache.performAccess(52,0,true,true) == ReadHit)

    assert(cache.performAccess(84,1,true,true) == ReadMiss) // Try another critical load
    assert(cache.performAccess(69,1,true,true) == ReadHit) // Ensure critical was not evicted because it was at limit
    assert(cache.performAccess(36,0,true,true) == ReadHit)// Ensure non-LRU stay saved
    assert(cache.performAccess(52,0,true,true) == ReadHit)
  }

  test("Critical hit on non-critical line increases contention limit") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1
    // Fill up set 1 with critical
    cache.performAccess(2,0,true,true) // way 0
    cache.performAccess(6,0,true,true) // way 1

    assert(cache.performAccess(4,0,true,true) == ReadHit) // try to use set 0 with critical

    assert(cache.performAccess(10,1,true,true) == ReadMiss) // try to overwrite set 1 with non-critical
    assert(cache.performAccess(10,1,true,true) == ReadHit) // Ensure was saved
    assert(cache.performAccess(6,1,true,true) == ReadHit)
  }

  test("Critical hit on non-critical line makes it critical") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);

    // Fill up set 0 with non-critical
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1
    // Fill up set 1 with critical
    cache.performAccess(2,0,true,true) // way 0
    cache.performAccess(6,0,true,true) // way 1

    assert(cache.performAccess(0,0,true,true) == ReadHit) // try to use set 0 with critical
    assert(cache.performAccess(10,1,true,true) == ReadMiss) // overwrite set 1 with non-critical, negating the previous win
    assert(cache.performAccess(8,0,true,true) == ReadMiss) // overwrite set 0 way 1 with critical

    assert(cache.performAccess(12,1,true,true) == Reject) // try to use set 0 with non-critical
    assert(cache.performAccess(12,1,true,true) == Reject) //
  }

  test("Critical hit on critical line does not increase contention count") {
    val cache = createInstance(2,2,2,20);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);

    // Fill up set 0 with crit 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1
    // Fill up set 1 with crit 1
    cache.performAccess(2,1,true,true) // way 0
    cache.performAccess(6,1,true,true) // way 1

    assert(cache.performAccess(3,0,true,true) == ReadHit) // Hit on other crit

    assert(cache.performAccess(8,2,true,true) == Reject) // Try to evict crit
    assert(cache.performAccess(8,2,true,true) == Reject) // Ensure did not work
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(4,0,true,true) == ReadHit) // Ensure was not evicted
  }

  ////////////////////////////////////////////////////////////////////////////////////

  test("Critical can only evict own partition") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 0);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Try to evict
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure evicted self
    assert(cache.performAccess(4,1,true,true) == ReadMiss) // Ensure evicted self
  }

  test("Critical can only evict own partition 2") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Try to evict
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure evicted self
    assert(cache.performAccess(4,1,true,true) == ReadMiss) // Ensure evicted self
  }

  test("Critical can only evict own partition 3") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == Reject) // Try to evict
    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure did not work
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
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1
    cache.performAccess(8,1,true,true) // way 2
    cache.performAccess(12,1,true,true) // way 3


    assert(cache.performAccess(16,1,true,true) == ReadMiss) // Try to evict
    assert(cache.performAccess(16,1,true,true) == ReadHit) // Ensure evicted self
    assert(cache.performAccess(0,0,true,true) == ReadHit)
    assert(cache.performAccess(4,0,true,true) == ReadHit)
    assert(cache.performAccess(12,1,true,true) == ReadHit)
  }

  test("Critical can evict unassigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 200);
    cache.setCriticality(1, 200);
    cache.assignWay(0,0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,2,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Try to evict
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did work
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure did not get evicted
  }

  test("Non-critical can evict unlimited assigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 50);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Try to evict
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did work
    assert(cache.performAccess(4,0,true,true) == ReadHit)
  }

  test("Non-critical can evict non-critical from limited assigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Try to evict
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure can evict non-critical
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure critical was not evicted
  }

  test("Non-critical cannot evict limited assigned way") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.assignWay(0,0);
    cache.assignWay(0,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == Reject) // Try to evict
    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure did not work
    assert(cache.performAccess(0,0,true,true) == ReadHit)
    assert(cache.performAccess(4,0,true,true) == ReadHit)
  }

  test("Limited critical self-eviction") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 0);
    cache.setCriticality(1, 100);
    cache.assignWay(0,0);
    cache.assignWay(1,1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,4,true,true) // way 1

    assert(cache.performAccess(8,0,true,true) == ReadMiss) // Try to evict self
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did work
    assert(cache.performAccess(4,4,true,true) == ReadHit) // Ensure did not use other partition
  }

  test("Non-critical evict of critical in non-assigned") {
    val cache = createInstance(2,2,2,50);
    cache.setCriticality(0, 50);
    cache.assignWay(0,0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Try to evict with on-critical
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did work
    assert(cache.performAccess(4,0,true,true) == ReadHit)

    assert(cache.performAccess(12,1,true,true) == ReadMiss) // Try to evict with on-critical again
    assert(cache.performAccess(12,1,true,true) == ReadHit) // Ensure did not evict critical
    assert(cache.performAccess(4,0,true,true) == ReadHit)
    assert(cache.performAccess(8,1,true,true) == ReadMiss)
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
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(9,1,true,true) // way 1
    cache.performAccess(18,2,true,true) // way 2

    assert(cache.performAccess(27,2,true,true) == ReadMiss) // try to use set 0 again
    assert(cache.performAccess(27,2,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(9,1,true,true) == ReadHit) // Ensure was not evicted
  }

  test("Limited critical prefer evicting non-critical in own partition") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 0)
    cache.assignWay(0, 0);
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,0,true,true) == ReadMiss) // Get critical
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
  }

  test("Unlimited critical doesn't prefer evicting non-critical in own partition") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 0);
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,0,true,true) == ReadMiss) // Get critical
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(4,1,true,true) == ReadHit) // Ensure was not evicted

    assert(cache.performAccess(12,0,true,true) == ReadMiss) // Try again, now as limited
    assert(cache.performAccess(12,0,true,true) == ReadHit) // Try again, now as limited
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did get saved
  }

  test("Unlimited critical doesn't prefer evicting non-critical in unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,0,true,true) == ReadMiss) // Get critical
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(4,1,true,true) == ReadHit) // Ensure was not evicted

    assert(cache.performAccess(12,0,true,true) == ReadMiss) // Try again, now limited
    assert(cache.performAccess(12,0,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure was not evicted
  }

  test("Limited critical prefers evicting non-critical in unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 0)
    cache.assignWay(0, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,1,true,true) // way 1

    assert(cache.performAccess(8,0,true,true) == ReadMiss) // Get critical
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
  }

  test("Critical may evict unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.performAccess(0,1,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,0,true,true) == ReadMiss) // Get critical
    assert(cache.performAccess(8,0,true,true) == ReadHit) // Ensure did get saved

    assert(cache.performAccess(4,0,true,true) == ReadHit) // Ensure was not evicted
  }

  test("Non-critical triggers contention if evicting critical in unassigned") {
    val cache = createInstance(2,2,2,10);
    cache.setCriticality(0, 10)
    cache.assignWay(0, 1);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == ReadMiss) // Get non-critical
    assert(cache.performAccess(8,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(4,0,true,true) == ReadHit) // Ensure was not evicted

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1

    assert(cache.performAccess(8,1,true,true) == Reject) // Try again
    assert(cache.performAccess(8,1,true,true) == Reject) // Ensure failed because critical is now limited
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(4,0,true,true) == ReadHit) // Ensure was not evicted

  }

  test("Non-critical evicts non-critical in uassigned with other limited critical") {
    val cache = createInstance(2,3,2,10);
    cache.setCriticality(0, 0)
    cache.assignWay(0, 0);

    // Fill up set 0
    cache.performAccess(0,0,true,true) // way 0
    cache.performAccess(4,0,true,true) // way 1
    cache.performAccess(8,1,true,true) // way 1

    assert(cache.performAccess(12,1,true,true) == ReadMiss) // Get non-critical
    assert(cache.performAccess(12,1,true,true) == ReadHit) // Ensure did get saved
    assert(cache.performAccess(0,0,true,true) == ReadHit) // Ensure was not evicted
    assert(cache.performAccess(4,0,true,true) == ReadHit) // Ensure was not evicted
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
      (_, hitType) => {
        wasHit = Some(hitType.isHit())
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

  test("Write results in write-back on evict") {
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(new TraceTraffic(1, Array(
          new MemAccess(0,false, 0, 0),
          new MemAccess(0,false, 4, 0),
          new MemAccess(0,false, 8, 0),
        ).toIterator,(_) => ())),
        (_) => None,
      ),
      new LruCache(2,2,2),
      (_, _) => {},
      true
    )

    assert(cache.requestMemoryAccess().contains((0,true,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(cache.requestMemoryAccess().contains((4,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(cache.requestMemoryAccess().contains((0,false,()))) // Third write needs to evict first
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    assert(cache.requestMemoryAccess().contains((8,true,()))) // Third write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response
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
      (_, hitType) => {
        wasHit = Some(hitType.isHit())
      }
    )

    assert(cache.requestMemoryAccess().contains((20,true,())))

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
    lruCache.performAccess(0,0,true,true) // Prefill cache line
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(new SingleTraffic(1,100,true)),
        (_) => None
      ),
      lruCache,
      (_, _) => {}
    )

    assert(cache.requestMemoryAccess().contains((100,true,())))
    assert(lruCache.performAccess(0,0,true,false).isReadHit()) // Ensure the miss didn't evict the prefilled line already
    cache.triggerCycle()
    assert(lruCache.performAccess(0,0,true,false).isReadHit())
    cache.serveMemoryAccess(())
    cache.requestMemoryAccess()
    assert(!lruCache.performAccess(0,0,true,false).isReadHit()) // After serve and at least request, fill should happen
  }

  test("Triggers cycles") {
    var cacheTickCount = 0;
    var trafTicks = 0;
    var cache = new CacheTraffic(8,
      new Traffic[Int] {
        override def burstSize: Int = 1

        override def serveMemoryAccess(token: Int): Boolean = false

        override def requestMemoryAccess(): Option[(Long, Boolean, Int)] = None

        override def triggerCycle(): Unit = {trafTicks += 1}

        override def isDone(): Boolean = false
      },
      new SoftCache(1,1,1) {
        override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = ReadMiss;

        override def isHit(addr: Long): Option[(Int, Int)] = None

        override def printAll(): Unit = {
        }

        override def advanceCycle(): Unit = {
          cacheTickCount += 1
        }

        override def evict(addr: Long): Unit = {}
      },
      (_,_) => (),
    )

    val triggerCount = rand.nextInt(10)
    for (_ <- 0 until triggerCount) {
      assert(cache.requestMemoryAccess().isEmpty)
      cache.triggerCycle()
    }
    assert(triggerCount == cacheTickCount)
  }

  test("Write-Back only report once") {
    var reportCount= 0
    var missCount = 0
    var cache = new CacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(new TraceTraffic(1, Array(
          new MemAccess(0,false, 0, 0),
          new MemAccess(0,false, 4, 0),
          new MemAccess(0,false, 8, 0),
        ).toIterator,(_) => ())),
        (_) => None,
      ),
      new SoftCache(2,2,2) {
        override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = {
          if(withRefill) {
            ReadMiss
          } else {
            WriteBack(300)
          }
        }

        override def isHit(addr: Long): Option[(Int, Int)] = None

        override def printAll(): Unit = {}
        override def evict(addr: Long): Unit = {}
      },
      (_, isHit) => {
        reportCount+=1
        if(isHit.isMiss()) missCount += 1
      },
      true
    )

    assert(cache.requestMemoryAccess().contains((300,false,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))

    assert(cache.requestMemoryAccess().contains((0,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(reportCount == 1)
    assert(missCount == 1)

    assert(cache.requestMemoryAccess().contains((300,false,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))

    assert(cache.requestMemoryAccess().contains((4,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(reportCount == 2)
    assert(missCount == 2)

    assert(cache.requestMemoryAccess().contains((300,false,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))

    assert(cache.requestMemoryAccess().contains((8,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(reportCount == 3)
    assert(missCount == 3)
  }

}

/**
 * A cache that hits on only the given address
 * @param lineSize
 * @param addr
 */
class SelectCache(lineSize:Int, hotAddr:Long) extends SoftCache(lineSize,1,1) {
  override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = {
    if(hotAddr == addr) ReadHit else ReadMiss
  };
  override def isHit(addr: Long): Option[(Int, Int)] = { None }
  override def printAll(): Unit = {}
  override def evict(addr: Long): Unit = {}
}

/**
 * Produces requests to the given addresses in sequence, not waiting for a response
 *
 * Becomes done when the same amount of responses are returned as requests
 *
 * @param traf
 */
class ArrayTraffic(traf: Array[(Long, Boolean)], onDone: () => Unit) extends Traffic[Int] {
  override def burstSize: Int = 1
  var done = 0;
  var status = 0;
  override def serveMemoryAccess(token: Int): Boolean = {
    assert(token < traf.length)
    onDone()
    done += 1
    true
  }

  override def requestMemoryAccess(): Option[(Long, Boolean, Int)] = {

    val result = if(status < traf.length) {
      Some((traf(status)._1, traf(status)._2, status))
    } else {
      None
    }
    status +=1
    result
  }
  override def triggerCycle(): Unit = {}
  override def isDone(): Boolean = {
    done == traf.length
  }
}

class ArrayTrafficUnit(traf: Array[(Long, Boolean)], onDone: () => Unit) extends Traffic[Unit] {
  override def burstSize: Int = 1
  var done = 0;
  var status = 0;

  override def serveMemoryAccess(token: Unit): Boolean = {
    onDone()
    done += 1
    true
  }

  override def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = {

    val result = if(status < traf.length) {
      Some((traf(status)._1, traf(status)._2, ()))
    } else {
      None
    }
    status +=1
    result
  }
  override def triggerCycle(): Unit = {}
  override def isDone(): Boolean = {
    done == traf.length
  }
}

/**
 * A traffic that can be given requests to issue live.
 */
class TriggerTraffic extends Traffic[Unit] {
  override def burstSize: Int = 1
  var nextRequest: Option[(Long, Boolean, Unit)] = None;


  override def serveMemoryAccess(token: Unit): Boolean = true
  override def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = {
    val result = nextRequest
    nextRequest = None
    result
  }
  override def triggerCycle(): Unit = {}
  override def isDone(): Boolean = false

}

class MapCache extends SoftCache(1,1,1) {
  var map: Map[Long,CacheResponse] = Map.empty
  override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = {
    map(addr)
  }

  override def isHit(addr: Long): Option[(Int, Int)] = None
  override def printAll(): Unit = {

  }

  override def evict(addr: Long): Unit = {}
}

class TriggerResponseCache(responseAddr: Long, accept: CacheResponse, reject: CacheResponse) extends SoftCache(1,1,1) {
  var shouldRespond = false
  override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = {
    if(responseAddr == addr && !shouldRespond) {
      reject
    } else {
      accept
    }
  }

  override def isHit(addr: Long): Option[(Int, Int)] = {None}

  override def printAll(): Unit = {}

  override def evict(addr: Long): Unit = {}
}

class BufferedCacheTrafficTest extends AnyFunSuite {
  var rand: scala.util.Random = new scala.util.Random;

  test("No Accesses") {
    var cache = new BufferedCacheTraffic(8,
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
    var cache = new BufferedCacheTraffic(8,
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
        wasHit = Some(isHit.isHit())
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

  test("Serve two consecutive hits") {
    var wasHit: Int = 0
    var done = 0
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(
          new TraceTraffic(1, Array(
            new MemAccess(0,true, 20, 0)
          ).toIterator,(_) => ()),
          new TraceTraffic(1, Array(
            new MemAccess(0,true, 40, 0)
          ).toIterator,(_) => ())),
        (_) => {
          done += 1
          None
        }
      ),
      new MainMemory(1),
      (_, isHit) => {
        if(isHit.isHit()) wasHit += 1;
      }
    )

    cache.triggerCycle() // One cycle for request
    assert(!cache.isDone() && done == 0)
    cache.triggerCycle() // One cycle for cache reply
    assert(!cache.isDone() && done == 0)
    assert(wasHit == 1)
    cache.triggerCycle() // One cycle for bus response and new request
    assert(!cache.isDone() && done == 1)
    assert(wasHit == 2)
    cache.triggerCycle() // One cycle for cache reply
    assert(!cache.isDone() && done == 1)
    assert(wasHit == 2)
    cache.triggerCycle() // One cycle for bus response and new request
    assert(cache.isDone() && done == 2)
    assert(wasHit == 2)
  }

  test("Serve miss after external serve") {
    var wasHit: Option[Boolean] = None
    var done = false
    var cache = new BufferedCacheTraffic(8,
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
        wasHit = Some(isHit.isHit())
      }
    )

    assert(cache.requestMemoryAccess().contains((20,true,())))

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

  test("Serve two consecutive misses") {
    var wasMiss: Int = 0
    var done = 0
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(
          new TraceTraffic(1, Array(
            new MemAccess(0,true, 20, 0)
          ).toIterator,(_) => ()),
          new TraceTraffic(1, Array(
            new MemAccess(0,true, 40, 0)
          ).toIterator,(_) => ())),
        (_) => {
          done += 1
          None
        }
      ),
      new LruCache(1,1,1),
      (_, isHit) => {
        if(isHit.isMiss()) wasMiss += 1;
      }
    )

    assert(cache.requestMemoryAccess().contains((20,true,())))

    // Wait for external reply
    val randLatency = 1+rand.nextInt(20)
    for(i <- 0 until randLatency) {
      assert(wasMiss == 1)
      assert(!cache.isDone() && done==0)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    cache.triggerCycle() // One cycle for cache response
    assert(!cache.isDone() && done==0)
    assert(wasMiss == 1)
    cache.triggerCycle() // One cycle for bus response and new request
    assert(!cache.isDone() && done==1)
    cache.triggerCycle() // One cycle for possible needed wait for new request
    assert(!cache.isDone() && done==1)
    assert(wasMiss == 2)

    assert(cache.requestMemoryAccess().contains((40,true,())))
    // Wait for external reply
    for(i <- 0 until randLatency) {
      assert(wasMiss == 2)
      assert(!cache.isDone() && done==1)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    cache.triggerCycle() // One cycle for cache response
    assert(!cache.isDone() && done==1)
    assert(wasMiss == 2)
    cache.triggerCycle() // One cycle for bus response
    assert(cache.isDone() && done==2)
    assert(wasMiss == 2)
  }

  test("Serve hit during miss") {
    var wasHit: Int = 0
    var wasMiss: Int = 0
    var done = 0
    var cache = new BufferedCacheTraffic(8,
      new ArrayTraffic(Array((20,true),(40,true)), () => done +=1),
      new SelectCache(1,40),
      (_, isHit) => {
        if(isHit.isHit()) wasHit += 1 else wasMiss += 1;
      }
    )

    assert(cache.requestMemoryAccess().contains((20,true,())))
    assert(wasMiss == 1)
    assert(wasHit == 0)
    assert(!cache.isDone() && done==0)
    cache.triggerCycle() // Start miss
    cache.triggerCycle() // One cycle for other access request (serve hit during miss)
    assert(wasMiss == 1)
    assert(wasHit == 1)
    cache.triggerCycle() // One cycle for cache hit
    assert(!cache.isDone() && done==1)

    // Wait for external reply
    val randLatency1 = 1+rand.nextInt(20)
    for(i <- 0 until randLatency1) {
      assert(!cache.isDone() && done==1)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    cache.triggerCycle() // One cycle for cache response
    assert(cache.isDone() && done==2)
    assert(wasHit == 1)
    assert(wasMiss == 1)
  }

  test("Hit after refill") {
    var wasMiss: Int = 0
    var done = 0
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(1,1,
        Array(
          new TraceTraffic(1, Array(
            new MemAccess(0,true, 20, 0)
          ).toIterator,(_) => ()),
          new TraceTraffic(1, Array(
            new MemAccess(0,true, 20, 0)
          ).toIterator,(_) => ())),
        (_) => {
          done += 1
          None
        }
      ),
      new LruCache(1,1,1),
      (_, isHit) => {
        if(isHit.isMiss()) wasMiss += 1;
      }
    )

    assert(cache.requestMemoryAccess().contains((20,true,())))

    // Wait for external reply
    val randLatency = 1+rand.nextInt(20)
    for(i <- 0 until randLatency) {
      assert(wasMiss == 1)
      assert(!cache.isDone() && done==0)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    cache.triggerCycle() // One cycle for cache response
    assert(!cache.isDone() && done==0)
    assert(wasMiss == 1)
    cache.triggerCycle() // One cycle for bus response and new request
    assert(!cache.isDone() && done==1)
    assert(wasMiss == 1)
    cache.triggerCycle() // One cycle for cache response
    assert(!cache.isDone() && done==1)
    cache.triggerCycle() // One cycle for bus response
    cache.triggerCycle() // One cycle for possible arbiter wait
    assert(cache.isDone() && done==2)
  }

  test("Hit after refill 2") {
    var wasMiss: Int = 0
    var wasHitAfterMiss: Int = 0
    var done = 0
    var cache = new BufferedCacheTraffic(8,
      new ArrayTraffic(Array((20,true),(40,true),(20,true)), () => done += 1),
      new LruCache(1,2,1),
      (_, isHit) => {
        isHit match {
          case Hit =>()
          case Miss => wasMiss += 1
          case HitAfterMiss => wasHitAfterMiss += 1
        }
      }
    )

    assert(cache.requestMemoryAccess().contains((20,true,())))
    assert(wasMiss == 1)
    assert(!cache.isDone() && done==0)
    cache.triggerCycle() // Miss starts
    assert(cache.requestMemoryAccess().isEmpty) // second request also misses, must wait
    assert(wasMiss == 2)
    assert(!cache.isDone() && done==0)
    cache.triggerCycle() // Second request in miss queue, third requests misses
    assert(cache.requestMemoryAccess().isEmpty) // third request also misses, must wait
    assert(wasMiss == 3)
    assert(!cache.isDone() && done==0)

    // Wait for external reply on first miss
    val randLatency = 1+rand.nextInt(20)
    for(i <- 0 until randLatency) {
      assert(wasMiss == 3)
      assert(!cache.isDone() && done==0)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    assert(!cache.isDone() && done==0)
    assert(cache.requestMemoryAccess().contains((40,true,()))) // Start second miss service
    cache.triggerCycle() // One cycle for cache response, second miss starts serving
    assert(!cache.isDone() && done==1)
    assert(wasMiss == 3)

    // Wait for external reply on second miss
    for(i <- 0 until randLatency) {
      assert(wasMiss == 3)
      assert(!cache.isDone() && done==1)
      cache.triggerCycle()
    }
    cache.serveMemoryAccess(())
    assert(wasMiss == 3)
    assert(wasHitAfterMiss == 0)
    assert(cache.requestMemoryAccess().isEmpty) // Third miss should not go through (hits now)
    cache.triggerCycle() // One cycle for cache response (request 2), Third miss should now hit
    assert(!cache.isDone() && done==2)
    assert(wasMiss == 3)
    cache.triggerCycle() // One cycle for cache response (request 3)
    assert(cache.isDone() && done==3)
    assert(wasMiss == 3)
    assert(wasHitAfterMiss == 1)

  }

  test("ReadMiss after reject") {
    val traffic = new Traffic[Int] {
      var requests: Array[(Long,Boolean, Int)] = Array((20,false,0),(40,false,1))
      override def burstSize: Int = 1

      override def serveMemoryAccess(token: Int): Boolean = {true}

      override def requestMemoryAccess(): Option[(Long, Boolean, Int)] = {
        if(!requests.isEmpty) {
          val req = requests(0)
          requests = requests.drop(1)
          Some(req)
        } else {
          None
        }
      }

      override def triggerCycle(): Unit = {}
      override def isDone(): Boolean = {
        requests.isEmpty
      }
    }
    var shouldRespond = false
    var cache = new BufferedCacheTraffic(8,
      traffic,
      new SoftCache(1,1,1) {
        override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = {
          if(addr == 20 && !shouldRespond) {
            Reject
          } else {
            ReadMiss
          }
        }

        override def isHit(addr: Long): Option[(Int, Int)] = {None}

        override def printAll(): Unit = {}

        override def evict(addr: Long): Unit = {}
      },
      (_, _) => {}
    )

    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()

    assert(cache.requestMemoryAccess().contains((40,true,()))) // Second access is a miss

  }

  test("ReadMiss after reject 2") {
    val traffic = new Traffic[Int] {
      var requests: Array[(Long,Boolean, Int)] = Array((20,false,0),(20,false,1),(40,false,2))
      override def burstSize: Int = 1

      override def serveMemoryAccess(token: Int): Boolean = {true}

      override def requestMemoryAccess(): Option[(Long, Boolean, Int)] = {
        if(!requests.isEmpty) {
          val req = requests(0)
          requests = requests.drop(1)
          Some(req)
        } else {
          None
        }
      }

      override def triggerCycle(): Unit = {}
      override def isDone(): Boolean = {
        requests.isEmpty
      }
    }
    var cache = new BufferedCacheTraffic(8,
      traffic,
      new SoftCache(1,1,1) {
        override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = {
          if(addr == 20) {
            Reject
          } else {
            ReadMiss
          }
        }

        override def isHit(addr: Long): Option[(Int, Int)] = {None}

        override def printAll(): Unit = {}

        override def evict(addr: Long): Unit = {}
      },
      (_, _) => {}
    )

    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()
    assert(cache.requestMemoryAccess().isEmpty) // second access is rejected
    cache.triggerCycle()

    assert(cache.requestMemoryAccess().contains((40,true,()))) // third access is a miss
  }

  test("ReadHit after reject") {
    var done = 0
    var responseCache= new TriggerResponseCache(20, ReadHit, Reject)
    val traffic = new RoundRobinArbiter(
      1,1,
      Array(
        new ArrayTrafficUnit(Array((20,false)), ()=> ()),
        new ArrayTrafficUnit(Array((40,false)), ()=> ()),
      ),
      (_) => {
        done += 1
        None
      },
      true
    )
    var cache = new BufferedCacheTraffic(8,
      traffic,
      responseCache,
      (_, _) => {}
    )

    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()
    assert(done == 0)

    assert(cache.requestMemoryAccess().isEmpty) // second access is accepted
    cache.triggerCycle()
    assert(done == 0)

    assert(cache.requestMemoryAccess().isEmpty) // one cycle for latency
    cache.triggerCycle()
    assert(done == 1)

    responseCache.shouldRespond = true

    assert(cache.requestMemoryAccess().isEmpty) // First access is now a hit
    cache.triggerCycle()// one cycle for response
    assert(cache.requestMemoryAccess().isEmpty) // First access is now a hit
    cache.triggerCycle()// one cycle for latency
    assert(done == 2)
  }

  test("ReadHit after reject 2") {
    var done = 0
    var responseCache= new TriggerResponseCache(20, ReadHit, Reject)
    val traffic = new RoundRobinArbiter(
      1,1,
      Array(
        new ArrayTrafficUnit(Array((20,false)), ()=> ()),
        new ArrayTrafficUnit(Array((20,false)), ()=> ()),
        new ArrayTrafficUnit(Array((40,false)), ()=> ()),
      ),
      (_) => {
        done += 1
        None
      },
      true
    )
    var cache = new BufferedCacheTraffic(8,
      traffic,
      responseCache,
      (_, _) => {}
    )

    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()
    assert(done == 0)

    assert(cache.requestMemoryAccess().isEmpty) // second access is rejected
    cache.triggerCycle()
    assert(done == 0)

    assert(cache.requestMemoryAccess().isEmpty) // second access is Accepted
    cache.triggerCycle()
    cache.triggerCycle()// one cycle for latency
    assert(done == 1)

    responseCache.shouldRespond = true

    assert(cache.requestMemoryAccess().isEmpty) // First access is now a hit
    cache.triggerCycle()// one cycle for response
    cache.triggerCycle()// one cycle for latency
    assert(done == 2)

    assert(cache.requestMemoryAccess().isEmpty) // second access is now a hit
    cache.triggerCycle()// one cycle for response
    cache.triggerCycle()// one cycle for latency
    assert(done == 3)
  }

  test("Miss becomes hit after Reject") {
    var done1 = false
    var done2 = false
    var done3 = false
    var responseCache= new MapCache
    val traffic = new RoundRobinArbiter(
      1,1,
      Array(
        new ArrayTrafficUnit(Array((20,false)), ()=> done1 = true),
        new ArrayTrafficUnit(Array((40,false)), ()=> done2 = true),
        new ArrayTrafficUnit(Array((60,false)), ()=> done3 = true),
      ),
      (_) => {
        None
      },
      true
    )
    var cache = new BufferedCacheTraffic(8,
      traffic,
      responseCache,
      (_, _) => {}
    )

    responseCache.map += (20L -> Reject)
    responseCache.map += (40L -> ReadMiss)
    responseCache.map += (60L -> ReadMiss)
    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3)

    assert(cache.requestMemoryAccess().contains((40,true,()))) // second access is Miss
    cache.triggerCycle()
    assert(!done1 && !done2 && !done2)

    assert(cache.requestMemoryAccess().isEmpty) // third access is Miss
    cache.triggerCycle()
    assert(!done1 && !done2 && !done2)

    cache.serveMemoryAccess(()) // Serve second access
    responseCache.map += (60L -> ReadHit) // Make third access now a hit

    assert(cache.requestMemoryAccess().isEmpty) // response latency for second access
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3)

    assert(cache.requestMemoryAccess().isEmpty) // third access is hit
    cache.triggerCycle()
    assert(!done1 && done2 && !done3)

    assert(cache.requestMemoryAccess().isEmpty) // third access latency
    cache.triggerCycle()
    assert(!done1 && done2 && !done3)

    assert(cache.requestMemoryAccess().isEmpty) // third access latency
    cache.triggerCycle()
    assert(!done1 && done2 && done3)

    responseCache.map += (20L -> ReadHit) // First access now a hit

    assert(cache.requestMemoryAccess().isEmpty) // first access latency
    cache.triggerCycle()
    assert(!done1 && done2 && done3)

    assert(cache.requestMemoryAccess().isEmpty) // first access latency
    cache.triggerCycle()
    assert(done1 && done2 && done3)

  }

  test("Reject becomes hit after Reject") {
    var done1 = false
    var done2 = false
    var done3 = false
    var responseCache= new MapCache
    val traffic = new RoundRobinArbiter(
      1,1,
      Array(
        new ArrayTrafficUnit(Array((20,false)), ()=> done1 = true),
        new ArrayTrafficUnit(Array((40,false)), ()=> done2 = true),
        new ArrayTrafficUnit(Array((60,false)), ()=> done3 = true),
      ),
      (_) => {
        None
      },
      true
    )
    var cache = new BufferedCacheTraffic(8,
      traffic,
      responseCache,
      (_, _) => {}
    )

    responseCache.map += (20L -> Reject)
    responseCache.map += (40L -> ReadMiss)
    responseCache.map += (60L -> Reject)
    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3)

    assert(cache.requestMemoryAccess().contains((40,true,()))) // second access is Miss
    cache.triggerCycle()
    assert(!done1 && !done2 && !done2)

    assert(cache.requestMemoryAccess().isEmpty) // third access is Miss
    cache.triggerCycle()
    assert(!done1 && !done2 && !done2)

    cache.serveMemoryAccess(()) // Serve second access
    responseCache.map += (60L -> ReadHit) // Make third access now a hit

    assert(cache.requestMemoryAccess().isEmpty) // response latency for second access
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3)

    assert(cache.requestMemoryAccess().isEmpty) // third access is hit
    cache.triggerCycle()
    assert(!done1 && done2 && !done3)

    assert(cache.requestMemoryAccess().isEmpty) // third access latency
    cache.triggerCycle()
    assert(!done1 && done2 && !done3)

    assert(cache.requestMemoryAccess().isEmpty) // third access latency
    cache.triggerCycle()
    assert(!done1 && done2 && done3)

    responseCache.map += (20L -> ReadHit) // First access now a hit

    assert(cache.requestMemoryAccess().isEmpty) // first access latency
    cache.triggerCycle()
    assert(!done1 && done2 && done3)

    assert(cache.requestMemoryAccess().isEmpty) // first access latency
    cache.triggerCycle()
    assert(done1 && done2 && done3)
  }

  test("Reject becomes hit after Reject 2") {
    var done1 = false
    var done2 = false
    var done3 = false
    var done4 = false
    var responseCache= new MapCache
    val traffic = new RoundRobinArbiter(
      1,1,
      Array(
        new ArrayTrafficUnit(Array((20,false)), ()=> done1 = true),
        new ArrayTrafficUnit(Array((40,false)), ()=> done2 = true),
        new ArrayTrafficUnit(Array((60,false)), ()=> done3 = true),
        new ArrayTrafficUnit(Array((80,false)), ()=> done4 = true),
      ),
      (_) => {
        None
      },
      true
    )
    var cache = new BufferedCacheTraffic(8,
      traffic,
      responseCache,
      (_, _) => {}
    )

    responseCache.map += (20L -> Reject)
    responseCache.map += (40L -> ReadMiss)
    responseCache.map += (60L -> Reject)
    responseCache.map += (80L -> Reject)
    assert(cache.requestMemoryAccess().isEmpty) // First access is rejected
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3 && !done4)

    assert(cache.requestMemoryAccess().contains((40,true,()))) // second access is Miss
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3 && !done4)

    assert(cache.requestMemoryAccess().isEmpty) // third access is Miss
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3 && !done4)

    cache.serveMemoryAccess(()) // Serve second access

    assert(cache.requestMemoryAccess().isEmpty) // response latency for second access
    cache.triggerCycle()
    assert(!done1 && !done2 && !done3 && !done4)

    responseCache.map += (60L -> ReadHit) // Make third access now a hit
    responseCache.map += (80L -> ReadHit) // Make fourth access now a hit

    assert(cache.requestMemoryAccess().isEmpty) // third access is hit
    cache.triggerCycle()
    assert(!done1 && done2 && !done3 && !done4)

    assert(cache.requestMemoryAccess().isEmpty) // third access latency and fourth hit
    cache.triggerCycle()
    assert(!done1 && done2 && done3 && !done4)

    assert(cache.requestMemoryAccess().isEmpty) // fourth access latency
    cache.triggerCycle()
    assert(!done1 && done2 && done3 && done4)

    responseCache.map += (20L -> ReadHit) // First access now a hit

    assert(cache.requestMemoryAccess().isEmpty) // first access latency
    cache.triggerCycle()
    assert(!done1 && done2 && done3 && done4)

    assert(cache.requestMemoryAccess().isEmpty) // first access latency
    cache.triggerCycle()
    assert(done1 && done2 && done3 && done4)
  }

  test("Triggers cycles") {
    var cacheTickCount = 0;
    var trafTicks = 0;
    var cache = new BufferedCacheTraffic(8,
      new Traffic[Int] {
        override def burstSize: Int = 1

        override def serveMemoryAccess(token: Int): Boolean = false

        override def requestMemoryAccess(): Option[(Long, Boolean, Int)] = None

        override def triggerCycle(): Unit = {trafTicks += 1}

        override def isDone(): Boolean = false
      },
      new SoftCache(1,1,1) {
        override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = ReadMiss;

        override def isHit(addr: Long): Option[(Int, Int)] = None

        override def printAll(): Unit = {
        }

        override def evict(addr: Long): Unit = {}

        override def advanceCycle(): Unit = {
          cacheTickCount += 1
        }
      },
      (_,_) => (),
    )

    val triggerCount = rand.nextInt(10)
    for (_ <- 0 until triggerCount) {
      assert(cache.requestMemoryAccess().isEmpty)
      cache.triggerCycle()
    }
    assert(triggerCount == cacheTickCount)
  }

  test("Write results in write-back on evict") {
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(new TraceTraffic(1, Array(
          new MemAccess(0,false, 0, 0),
          new MemAccess(0,false, 4, 0),
          new MemAccess(0,false, 8, 0),
        ).toIterator,(_) => ())),
        (_) => None,
        true
      ),
      new LruCache(2,2,2),
      (_, _) => {},
      true
    )

    assert(cache.requestMemoryAccess().contains((0,true,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(cache.requestMemoryAccess().contains((4,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(cache.requestMemoryAccess().contains((0,false,()))) // Third write needs to evict first
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    assert(cache.requestMemoryAccess().contains((8,true,()))) // Third write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response
  }

  test("Write-back evicts line after write") {
    var lruCache = new LruCache(2,2,2);
    // Fill set 1 with dirty lines
    lruCache.performAccess(0,0,false,true)
    lruCache.performAccess(4,0,false,true)
    var traf1 = new TriggerTraffic;
    var traf2 = new TriggerTraffic;
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(traf1,traf2),
        (_) => None,
        true
      ),
      lruCache,
      (_, _) => {},
      true
    )

    traf1.nextRequest = Some(8,true,())
    assert(cache.requestMemoryAccess().contains((0,false,()))) // traf1 access needs to evict first
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    traf2.nextRequest = Some(0,true,()) // traf2 requests line that was just evicted, should miss

    assert(cache.requestMemoryAccess().contains((8,true,()))) // traf1 access needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().contains((4,false,()))) // Traf2 access must evict line2
    cache.triggerCycle() // One cycle for bus response to traf1

    cache.triggerCycle() // One cycle for traf2 request
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    cache.triggerCycle() // One cycles to check that traf1 does not issue request

    assert(cache.requestMemoryAccess().contains((0,true,()))) // Traf2 access needs to load
  }

  test("Concurrent write-backs") {
    var lruCache = new LruCache(2,2,2);
    // Fill set 1 with dirty lines
    lruCache.performAccess(0,0,false,true)
    lruCache.performAccess(4,0,false,true)
    var traf1 = new TriggerTraffic;
    var traf2 = new TriggerTraffic;
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(traf1,traf2),
        (_) => None,
        true
      ),
      lruCache,
      (_, _) => {},
      true
    )

    // Both accesses will need to write-back
    traf1.nextRequest = Some(80,true,())
    traf2.nextRequest = Some(120,true,())

    assert(cache.requestMemoryAccess().contains((0,false,()))) // traf1 access needs to evict first
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    assert(cache.requestMemoryAccess().contains((80,true,()))) // traf1 access needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().contains((4,false,()))) // Traf2 access must evict line2
    cache.triggerCycle() // One cycle for bus response to traf1

    cache.triggerCycle() // One cycle for request of traf2
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    cache.triggerCycle() // One cycles to check that traf1 does not issue request

    assert(cache.requestMemoryAccess().contains((120,true,()))) // Traf2 access needs to load
  }

  test("Concurrent write-backs 2") {
    var lruCache = new LruCache(2,2,2);
    // Fill set 1 with dirty lines
    lruCache.performAccess(0,0,false,true)
    lruCache.performAccess(4,0,false,true)
    var traf1 = new TriggerTraffic;
    var traf2 = new TriggerTraffic;
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(traf1,traf2),
        (_) => None,
        true
      ),
      lruCache,
      (_, _) => {},
      true
    )

    // Both accesses will need to write-back
    traf1.nextRequest = Some(80,true,())
    traf2.nextRequest = Some(120,true,())

    assert(cache.requestMemoryAccess().contains((0,false,()))) // traf1 access needs to evict first
    cache.triggerCycle() // One cycle for request
    // Simulate high latency
    for(_ <- 0 until 3) {
      assert(cache.requestMemoryAccess().isEmpty)
      cache.triggerCycle()
    }
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    assert(cache.requestMemoryAccess().contains((80,true,()))) // traf1 access needs to load
    cache.triggerCycle() // One cycle for request
    // Simulate high latency
    for(_ <- 0 until 3) {
      assert(cache.requestMemoryAccess().isEmpty)
      cache.triggerCycle()
    }
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().contains((4,false,()))) // Traf2 access must evict line2
    cache.triggerCycle() // One cycle for bus response to traf1

    cache.triggerCycle() // One cycle for request of traf2
    assert(cache.serveMemoryAccess(()))
    // No need for a bus latency (to internal), so should continue servicing the access

    cache.triggerCycle() // One cycles to check that traf1 does not issue request

    assert(cache.requestMemoryAccess().contains((120,true,()))) // Traf2 access needs to load
  }

  test("Write-Back only report once") {
    var reportCount= 0
    var missCount = 0
    var lruCache = new LruCache(2, 2, 2)
    lruCache.performAccess(0,0,false,true)
    lruCache.performAccess(4,0,false,true)
    var cache = new BufferedCacheTraffic(8,
      new RoundRobinArbiter(2,1,
        Array(new TraceTraffic(1, Array(
          new MemAccess(0,false, 8, 0),
          new MemAccess(0,false, 12, 0),
          new MemAccess(0,false, 16, 0),
        ).toIterator,(_) => ())),
        (_) => None,
      ),
      lruCache,
      (_, isHit) => {
        reportCount+=1
        if(isHit.isMiss()) missCount += 1
      },
      true
    )

    assert(cache.requestMemoryAccess().contains((0,false,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))

    assert(cache.requestMemoryAccess().contains((8,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(reportCount == 1)
    assert(missCount == 1)

    assert(cache.requestMemoryAccess().contains((4,false,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))

    assert(cache.requestMemoryAccess().contains((12,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(reportCount == 2)
    assert(missCount == 2)

    assert(cache.requestMemoryAccess().contains((8,false,()))) // First write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))

    assert(cache.requestMemoryAccess().contains((16,true,()))) // second write needs to load
    cache.triggerCycle() // One cycle for request
    assert(cache.serveMemoryAccess(()))
    assert(cache.requestMemoryAccess().isEmpty)
    cache.triggerCycle() // One cycle for bus response

    assert(reportCount == 3)
    assert(missCount == 3)
  }
}