package caches

import org.scalatest.funsuite.AnyFunSuite

class ArbiterTests extends AnyFunSuite {
  var rand: scala.util.Random = new scala.util.Random;
  test("No Accesses") {
    var arbiter = new Arbiter(1,8, new LruCache(1, 1, 1), 1,
      Array.fill(2) {
        new Traffic {
          def burstSize: Int = 1;

          def triggerCycle() = {};

          def requestMemoryAccess(): Option[(Long)] = {
            None
          };

          def serveMemoryAccess() = {};
        }
      }
    )

    for (_ <- 0 until 10) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }
  }

  test("Serve after latency") {
    val latency = rand.nextInt(25)
    var arbiter = new Arbiter(1,8, new SoftCache(1,1,1) {
      override def getCacheLine(addr: Long, core: Int): Boolean = {
        false // Never hit
      }

      override def isHit(addr: Long): Option[(Int, Int)] = {
        None
      }

      override def printAll(): Unit = {}

      override def advanceCycle(): Unit = {}
    },latency,
      Array.range(0,3).map(idx => new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {};

        def requestMemoryAccess(): Option[Long] = {
          Some(idx)
        };

        def serveMemoryAccess() = {};
      })
    )

    assert(arbiter.requestMemoryAccess().contains(0)) // First core access
    // Insert arbitrary wait for serve
    for(_ <- 0 until rand.nextInt(30)) arbiter.triggerCycle();
    arbiter.serveMemoryAccess()
    // busy servicing access
    for(_ <- 0 until latency+1) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }

    assert(arbiter.requestMemoryAccess().contains(1)) // ready to service next access
    // Insert arbitrary wait for serve
    for(_ <- 0 until rand.nextInt(30)) arbiter.triggerCycle();
    arbiter.serveMemoryAccess()
    // busy servicing
    for(_ <- 0 until latency+1) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }

    assert(arbiter.requestMemoryAccess().contains(2)) // ready to service next access
  }

  test("Triggers device cycles") {
    var counts = Array.fill(4){0}
    var arbiter = new Arbiter(1,8,
      new SoftCache(1,1,1) {
        override def getCacheLine(addr: Long, core: Int): Boolean = {
          if(math.random < 0.5) true else false
        }

        override def isHit(addr: Long): Option[(Int, Int)] = {
          if(math.random < 0.5) Some((0,0)) else None
        }

        override def printAll(): Unit = {}

        override def advanceCycle(): Unit = {counts(0) += 1}
      },
      6,
      Array.range(1,4).map(idx => new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {counts(idx) += 1};

        def requestMemoryAccess(): Option[Long] = {
          if(math.random < 0.5) Some(0) else None
        };

        def serveMemoryAccess() = {};
      })
    )

    val triggerCount = rand.nextInt(256)
    Range.apply(0, triggerCount).foreach(_=> arbiter.triggerCycle())
    assert(counts.forall(c => c == triggerCount))
  }

  test("Service even without parent request") {
    var latency = 2+rand.nextInt(25)
    var servicedAccesses = 0;
    var arbiter = new Arbiter(1,8,
      new SoftCache(1,1,1) {
        override def getCacheLine(addr: Long, core: Int): Boolean = {
          true // always hit
        }

        override def isHit(addr: Long): Option[(Int, Int)] = {
          None
        }

        override def printAll(): Unit = {}

        override def advanceCycle(): Unit = {}
      },
      latency,
      Array.range(0,1).map(idx => new Traffic {
        def burstSize: Int = 1;

        def triggerCycle() = {};

        def requestMemoryAccess(): Option[Long] = {
          Some(0)
        };

        def serveMemoryAccess() = {
          servicedAccesses += 1
        };
      })
    )

    val expectedServings = 1+rand.nextInt(25)
    for(_ <- 0 until (latency*expectedServings)+expectedServings) {
      arbiter.triggerCycle()
    }
    assert(servicedAccesses == expectedServings )
  }
}

class TraceTrafficTests extends AnyFunSuite {

  var rand: scala.util.Random = new scala.util.Random;

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

  test("Two accesses") {
    var traf = new TraceTraffic(4, Array(
      new MemAccess(0,true, 20, 0),
      new MemAccess(0,true, 56, 0)
    ).toIterator)

    assert(traf.requestMemoryAccess().contains(20))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(56))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Double size before access") {
    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 20, 0),
      new MemAccess(0,true, 56, 0)
    ).toIterator)

    assert(traf.requestMemoryAccess().contains(20))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(22))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(56))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Twice oversize") {
    var traf = new TraceTraffic(1, Array(
      new MemAccess(2,true, 16, 0),
      new MemAccess(1,true, 90, 0)
    ).toIterator)

    assert(traf.requestMemoryAccess().contains(16))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(17))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(18))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(19))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(90))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(91))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Delayed") {
    var delay = rand.nextInt(300)
    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true, 16, delay)
    ).toIterator)

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains(16))
    traf.serveMemoryAccess()
    traf.triggerCycle()
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Delayed 2") {
    var delay = rand.nextInt(300)
    var delay2 = delay + rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true, 16, delay),
      new MemAccess(0,true, 74, delay2)
    ).toIterator)

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains(16))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess()
    assert(Range.apply(0, delay2-delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains(74))
  }

  test("Delayed with Double size") {
    var delay = rand.nextInt(300)
    var delay2 = delay + rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(1,true, 16, delay),
      new MemAccess(0,true, 74, delay2)
    ).toIterator)

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains(16))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess()
    assert(traf.requestMemoryAccess().contains(17))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess()
    assert(Range.apply(0, delay2-delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains(74))
  }

}

