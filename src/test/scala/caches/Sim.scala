package caches

import org.scalatest.funsuite.AnyFunSuite

class CacheArbiterTests extends AnyFunSuite {
  test("No Accesses") {
    var arbiter = new CacheArbiter(1, new LruCache(1,1,1,1,2),
      Array.fill(2){new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {};

        def requestMemoryAccess(): Option[(Long)] = {
          None
        };

        def serveMemoryAccess() = {};
      }}
    )

    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
    assert(arbiter.trigger().isEmpty)
  }

  test("Cache busy") {
    var arbiter = new CacheArbiter(1, new LruCache(1,1,1,1,2),
      Array.fill(2){new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {};

        def requestMemoryAccess(): Option[Long] = {
          Some(0)
        };

        def serveMemoryAccess() = {};
      }}
    )

    assert(arbiter.trigger().contains((0,false))) // First core access with miss
    assert(Range.apply(0,2).forall(_ => arbiter.trigger().isEmpty)) // busy servicing miss
    assert(arbiter.trigger().isDefined) // ready to service next access
  }

  test("Cache busy 2") {
    var arbiter = new CacheArbiter(1, new LruCache(1,1,1,1,20),
      Array.fill(2){new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {};

        def requestMemoryAccess(): Option[Long] = {
          Some(0)
        };

        def serveMemoryAccess() = {};
      }}
    )

    assert(arbiter.trigger().contains((0,false))) // First core access with miss
    assert(Range.apply(0,20).forall(_ => arbiter.trigger().isEmpty)) // busy servicing miss
    assert(arbiter.trigger().isDefined) // ready to service next access
  }

  test("Cache busy 3") {
    var arbiter = new CacheArbiter(1, new LruCache(1,1,1,4,8),
      Array.fill(2){new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {};

        def requestMemoryAccess(): Option[Long] = {
          Some(0)
        };

        def serveMemoryAccess() = {};
      }}
    )

    assert(arbiter.trigger().filter(res => res._2 == false).isDefined) // First core access with miss
    assert(Range.apply(0,8).forall(_ => arbiter.trigger().isEmpty)) // busy servicing miss
    assert(arbiter.trigger().filter(res => res._2 == true).isDefined) // ready to service next access, which is a hit
    assert(Range.apply(0,4).forall(_ => arbiter.trigger().isEmpty)) // busy servicing miss
    assert(arbiter.trigger().isDefined) // ready to service next access
  }

  test("Triggers device cycles") {
    var counts = Array.fill(4){0}
    var arbiter = new CacheArbiter(1,
      new SoftCache(1,1,1,6,12) {
        override def getCacheLine(addr: Long, core: Int): Int = {
          if(math.random < 0.5) shortLatency else longLatency
        }

        override def isHit(addr: Long): Option[(Int, Int)] = {
          if(math.random < 0.5) Some((0,0)) else None
        }

        override def printAll(): Unit = {}

        override def advanceCycle(): Unit = {counts(0) += 1}
      },
      Array.range(1,4).map(idx => new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {counts(idx) += 1};

        def requestMemoryAccess(): Option[Long] = {
          if(math.random < 0.5) Some(0) else None
        };

        def serveMemoryAccess() = {};
      })
    )

    val triggerCount = math.random.toInt % 256
    Range.apply(0, triggerCount).foreach(_=> arbiter.trigger())
    assert(counts.forall(c => c == triggerCount))
  }

  test("Serves after latency") {
    var latency = math.random.toInt%25
    var countedLatency = 0;
    var serveLatency = 0;
    var arbiter = new CacheArbiter(1,
      new SoftCache(1,1,1,6,latency) {
        override def getCacheLine(addr: Long, core: Int): Int = {
          longLatency
        }

        override def isHit(addr: Long): Option[(Int, Int)] = {
          None
        }

        override def printAll(): Unit = {}

        override def advanceCycle(): Unit = {}
      },
      Array.range(0,1).map(idx => new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {
          countedLatency += 1};

        def requestMemoryAccess(): Option[Long] = {
          Some(0)
        };

        def serveMemoryAccess() = {
          serveLatency = countedLatency
        };
      })
    )

    assert(arbiter.trigger().isDefined) // start a miss
    assert(Range.apply(0,latency).forall(_ => arbiter.trigger().isEmpty)) // Advance cycles to after the serve
    assert(arbiter.trigger().isDefined) // After busy is empty, may service another
    assert(serveLatency == latency+1) // Should trigger the previous access end before next cycle
  }
}

class TraceTrafficTests extends AnyFunSuite {
  test("Empty") {
    var traf = new TraceTraffic(1, Array.empty.toIterator)

    assert(Range.apply(0,100).forall(_ => traf.requestMemoryAccess().isEmpty))
  }

  test("Single") {
    var traf = new TraceTraffic(1, Array(new MemAccess(0,true, 0, 0)).toIterator)

    assert(traf.requestMemoryAccess().isDefined)
    traf.serveMemoryAccess()
    assert(Range.apply(0,100).forall(_ => traf.requestMemoryAccess().isEmpty))
  }

  test("Single Double Size") {
    var traf = new TraceTraffic(1, Array(new MemAccess(1,true, 0, 0)).toIterator)

    assert(traf.requestMemoryAccess().contains(0))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(1))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Single Double Size 2") {
    var traf = new TraceTraffic(2, Array(new MemAccess(2,true, 10, 0)).toIterator)

    assert(traf.requestMemoryAccess().contains(10))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(12))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Single Quad Size") {
    var traf = new TraceTraffic(2, Array(new MemAccess(3,true, 20, 0)).toIterator)

    assert(traf.requestMemoryAccess().contains(20))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(22))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(24))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(26))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().isEmpty)
  }


}