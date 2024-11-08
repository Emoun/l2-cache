package caches

import org.scalatest.funsuite.AnyFunSuite

class TraceTrafficTests2 extends AnyFunSuite {

  var rand: scala.util.Random = new scala.util.Random;

  test("Empty") {
    var traf = new TraceTraffic(1, Array.empty.toIterator, (_) => ())

    assert(Range.apply(0,100).forall(_ => traf.requestMemoryAccess().isEmpty))
  }

  test("Single") {
    var traf = new TraceTraffic(1, Array(new MemAccess(0,true, 0, 0)).toIterator, (_) => ())

    assert(traf.requestMemoryAccess().isDefined)
    traf.serveMemoryAccess(())
    assert(Range.apply(0,100).forall(_ => traf.requestMemoryAccess().isEmpty))
  }

  test("Single Double Size") {
    var traf = new TraceTraffic(1, Array(new MemAccess(1,true, 0, 0)).toIterator, (_) => ())

    assert(traf.requestMemoryAccess().contains((0, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((1, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Single Double Size 2") {
    var traf = new TraceTraffic(2, Array(new MemAccess(2,true, 10, 0)).toIterator, (_) => ())

    assert(traf.requestMemoryAccess().contains((10, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((12, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Single Quad Size") {
    var traf = new TraceTraffic(2, Array(new MemAccess(3,true, 20, 0)).toIterator, (_) => ())

    assert(traf.requestMemoryAccess().contains((20, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((22, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((24, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((26, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Two accesses") {
    var traf = new TraceTraffic(4, Array(
      new MemAccess(0,true, 20, 0),
      new MemAccess(0,true, 56, 0)
    ).toIterator,(_) => ())

    assert(traf.requestMemoryAccess().contains((20, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((56, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Double size before access") {
    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 20, 0),
      new MemAccess(0,true, 56, 0)
    ).toIterator,(_) => ())

    assert(traf.requestMemoryAccess().contains((20, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((22, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((56, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Twice oversize") {
    var traf = new TraceTraffic(1, Array(
      new MemAccess(2,true, 16, 0),
      new MemAccess(1,true, 90, 0)
    ).toIterator,(_) => ())

    assert(traf.requestMemoryAccess().contains((16, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((17, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((18, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((19, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((90, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((91, ())))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Delayed") {
    var delay = rand.nextInt(300)
    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true, 16, delay)
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((16, ())))
    traf.serveMemoryAccess(())
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
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((16, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(Range.apply(0, delay2-delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((74, ())))
  }

  test("Delayed with Double size") {
    var delay = rand.nextInt(300)
    var delay2 = delay + rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(1,true, 16, delay),
      new MemAccess(0,true, 74, delay2)
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((16, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((17, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(Range.apply(0, delay2-delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((74, ())))
  }

  test("Unaligned burst-2 data-2") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(1,true, 1, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((2, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Unaligned burst-2 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 1, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((2, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((4, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Aligned burst-2 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(2,true, 6, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((6, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((8, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Unaligned burst-2 data-8") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(2, Array(
      new MemAccess(3,true, 11, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((10, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((12, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((14, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((16, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((18, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Burst-unaligned burst-8 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(8, Array(
      new MemAccess(2,true, 4, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Burst-unaligned data-unaligned burst-8 data-4") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(8, Array(
      new MemAccess(2,true,3, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Burst-boundary crossing access") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(8, Array(
      new MemAccess(2,true,5, delay),
    ).toIterator,(_) => ())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    assert(traf.requestMemoryAccess().contains((0, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().contains((8, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.requestMemoryAccess().isEmpty)
  }

  test("Report done") {
    var delay = rand.nextInt(300)
    var latency = rand.nextInt(30)

    var traf = new TraceTraffic(1, Array(
      new MemAccess(0,true,1, delay),
      new MemAccess(0,true,2, delay),
      new MemAccess(0,true,3, delay),
    ).toIterator,(_) => ())

    assert(!traf.isDone())

    assert(Range.apply(0, delay).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    assert(traf.requestMemoryAccess().contains((1, ())))
    assert(!traf.isDone())
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    traf.serveMemoryAccess(())
    assert(!traf.isDone())
    assert(traf.requestMemoryAccess().contains((2, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    traf.serveMemoryAccess(())
    assert(!traf.isDone())
    assert(traf.requestMemoryAccess().contains((3, ())))
    assert(Range.apply(0, latency).forall(_=> {
      val result = traf.requestMemoryAccess().isEmpty
      traf.triggerCycle()
      assert(!traf.isDone())
      result
    }))
    traf.serveMemoryAccess(())
    assert(traf.isDone())
  }

}


class NoTraffic(burst: Int) extends Traffic[Unit] {
  override def burstSize: Int = burst

  override def serveMemoryAccess(token: Unit): Boolean = true

  override def requestMemoryAccess(): Option[(Long, Unit)] = None;

  override def triggerCycle(): Unit = {}

  override def isDone(): Boolean = false;
}

class SingleTraffic(burst: Int, addr: Long) extends Traffic[Unit] {
  override def burstSize: Int = burst

  override def serveMemoryAccess(token: Unit): Boolean = true

  override def requestMemoryAccess(): Option[(Long, Unit)] = Some((addr, ()));

  override def triggerCycle(): Unit = {}

  override def isDone(): Boolean = false;
}

class RoundRobinArbiterTests extends AnyFunSuite {
  var rand: scala.util.Random = new scala.util.Random;
  test("No Accesses") {
    var arbiter = new RoundRobinArbiter(8, 1,
      Array.fill(2) {new NoTraffic(1)},(_) => None,
    )

    for (_ <- 0 until 10) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }
  }

  test("Serve after latency") {
    val latency = 1+rand.nextInt(25)
    var arbiter = new RoundRobinArbiter(8,latency,
      Array.range(0,3).map(idx => new SingleTraffic(1, idx)),(_) => None,
    )

    assert(arbiter.requestMemoryAccess().contains((0,0))) // First core access
    // Insert arbitrary wait for serve
    for(_ <- 0 until rand.nextInt(30)) arbiter.triggerCycle();
    arbiter.serveMemoryAccess(0)
    // busy servicing access
    for(_ <- 0 until latency) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }

    assert(arbiter.requestMemoryAccess().contains((1,1))) // ready to service next access
    // Insert arbitrary wait for serve
    for(_ <- 0 until rand.nextInt(30)) arbiter.triggerCycle();
    arbiter.serveMemoryAccess(1)
    // busy servicing
    for(_ <- 0 until latency) {
      assert(arbiter.requestMemoryAccess().isEmpty)
      arbiter.triggerCycle()
    }

    assert(arbiter.requestMemoryAccess().contains((2,2))) // ready to service next access
  }

  test("Triggers device cycles") {
    var counts = Array.fill(4){0}
    var arbiter = new RoundRobinArbiter(8,
      6,
      Array.range(0,4).map(idx => new Traffic[Unit] {
        def burstSize: Int = 1;

        def triggerCycle() = {counts(idx) += 1};

        def requestMemoryAccess(): Option[(Long, Unit)] = {
          if(math.random < 0.5) Some((0, ())) else None
        };

        def serveMemoryAccess(token: Unit):Boolean = true;

        override def isDone(): Boolean = false;
      }), (_) => None,
    )

    val triggerCount = rand.nextInt(256)
    Range.apply(0, triggerCount).foreach(_=> arbiter.triggerCycle())
    counts.foreach(c => assert(c == triggerCount))
  }

  test("Report done") {
    var latency = 2+rand.nextInt(25)
    var done = false;
    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(new TraceTraffic(1, Array(
        new MemAccess(0,true, 20, 0)
      ).toIterator,(_) => ())),
      (_) => {
        done = !done
        None
      },
    )

    assert(!arbiter.isDone())
    assert(!done)
    assert(arbiter.requestMemoryAccess().contains((20,0)))
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.triggerCycle()
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until latency+1) {
      assert(!arbiter.isDone())
      assert(!done)
      arbiter.triggerCycle()
    }
    assert(done)
  }

  test("Report delayed done") {
    var latency = 1+rand.nextInt(25)
    var done = false;
    val doneLatency = 1+rand.nextInt(10)
    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(new Traffic[Unit] {
        var countdown:Option[Int] = None
        var requested = false
        override def burstSize: Int = 1

        override def serveMemoryAccess(token: Unit): Boolean = {
          assert(countdown.isEmpty)
          countdown = Some(doneLatency)
          true
        }

        override def requestMemoryAccess(): Option[(Long, Unit)] = {
          if(!requested) {
            requested = true
            Some((0, ()))
          } else {
            None
          }
        }

        override def triggerCycle(): Unit = {
          if(countdown.isDefined && countdown.get >0){
            countdown = Some(countdown.get - 1)
          }
        }

        override def isDone():Boolean = {
          countdown.isDefined && (countdown.get == 0) && requested
        }
      }),
      (_) => {
        done = !done
        None
      },
    )

    assert(arbiter.requestMemoryAccess().contains((0,0)))
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.triggerCycle()
    assert(!arbiter.isDone())
    assert(!done)
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until 1+latency+doneLatency) {
      assert(!arbiter.isDone())
      assert(!done)
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone())
    assert(done)
  }

  test("Change On Done") {
    var latency = 2+rand.nextInt(25)
    val newTraffic = () => new TraceTraffic(1, Array(
      new MemAccess(0, true, 20, 0)
    ).toIterator, (_) => ())


    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic()),
      (_) => {
        Some(newTraffic())
      },
    )

    assert(arbiter.requestMemoryAccess().contains((20,0)))
    arbiter.triggerCycle()
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone())
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone())

    assert(arbiter.requestMemoryAccess().contains((20,0)))
    arbiter.triggerCycle()
    arbiter.serveMemoryAccess(0)
    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone())
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone())
  }

  test("Allow multiple outstanding requests") {
    var latency = 2+rand.nextInt(25)
    var done = 0
    val newTraffic = (addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).toIterator, (_) => done += 1)

    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(20), newTraffic(40)),
      (_) => None,
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,1))) // Second request issued while waiting for the first

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(0)) // Respond to first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && done == 1) // First request done

    // wait some random amount of time before second external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && done == 1)
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(1)) // Respond to second

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && done == 1)
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone() && done == 2) // Second request done
  }

  test("Multi-request external serve while internal serve") {
    var latency = 2+rand.nextInt(25)
    var done = 0
    val newTraffic = (addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).toIterator, (_) => done += 1)

    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(20), newTraffic(40)),
      (_) => None,
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,1))) // Second request issued while waiting for the first

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(0)) // Respond to first
    for(i <- 0 until 1+latency) {
      assert(!arbiter.serveMemoryAccess(1)) // Disallow serve of second while servicing first
      assert(!arbiter.isDone() && done == 0)
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && done == 1) // First request done
    assert(arbiter.serveMemoryAccess(1)) // Respond to second

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && done == 1)
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone() && done == 2) // Second request done
  }

  test("Multi-request Reply out of order") {
    var latency = 2+rand.nextInt(25)
    var done = Array.fill(3){false}
    val newTraffic = (coreId: Int, addr: Int) => new TraceTraffic(1, Array(
      new MemAccess(0, true, addr, 0)
    ).toIterator, (_) => done(coreId) = true)
    val doneArray = (d1:Boolean, d2:Boolean, d3:Boolean) => {
      done(0) == d1 && done(1) == d2 && done(2) == d3
    }


    var arbiter = new RoundRobinArbiter(8,
      latency,
      Array(newTraffic(0,20), newTraffic(1,40), newTraffic(2,60)),
      (_) => None,
      true // Allow multiple requests
    )

    assert(arbiter.requestMemoryAccess().contains((20,0)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((40,1)))
    arbiter.triggerCycle()
    assert(arbiter.requestMemoryAccess().contains((60,2)))

    // wait some random amount of time before external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(false,false,false))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(2)) // Respond to last first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(false,false,false))
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && doneArray(false,false,true)) // last request done

    // wait some random amount of time before second external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(false,false,true))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(1)) // Respond to second

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(false,false,true))
      arbiter.triggerCycle()
    }
    assert(!arbiter.isDone() && doneArray(false,true,true)) // Second request done

    // wait some random amount of time before third external response
    for(i <- 0 until 1+rand.nextInt(25)) {
      assert(!arbiter.isDone() && doneArray(false,true,true))
      arbiter.triggerCycle()
    }
    assert(arbiter.serveMemoryAccess(0)) // Respond to first

    for(i <- 0 until 1+latency) {
      assert(!arbiter.isDone() && doneArray(false,true,true))
      arbiter.triggerCycle()
    }
    assert(arbiter.isDone() && doneArray(true,true,true)) // First request done
  }
  
}