package caches

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex



// Define case classes for DAW and DAR
sealed trait LogEntry
case class DAW(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry
case class DAR(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry
case class DAWS(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry
case class DARS(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry

class MemAccess (dataType:Int, r:Boolean, addr: Long, t:Long ){
  /**
   * Size of the access in bytes
   */
  val accessSize: Int = scala.math.pow(2, dataType).intValue()

  val isRead:Boolean = r
  val address: Long = addr
  val time:Long = t


  def prettyPrint(): Unit = {
    printf("{%d, %s, 0x%s, %d}", accessSize, if(isRead) "Read" else "Write", addr.toHexString, time)
  }

}

trait Traffic {

  def burstSize: Int;
  require(isPowerOfTwo(burstSize))

  def serveMemoryAccess(): Unit;

  def requestMemoryAccess(): Option[Long];

  def triggerCycle();

  def isPowerOfTwo(x: Int): Boolean =
  {
    (x & (x - 1)) == 0
  }

  def isDone(): Boolean = { false }

}

class Arbiter(
  burstIn: Int,
  burstOut: Int,
  cache: SoftCache,
  latency:Int,
  cores: Array[Traffic],
  // Takes core and cache, returns whether  the core needs to be changed to the given
  onDone: (Int, SoftCache) => Option[Traffic],
  reportHits: (Int,Boolean) => Unit
)
  extends Traffic
{
  require(cache.lineLength>=burstIn)
  require(cache.lineLength%burstIn == 0)
  require(cores.forall( c => c.burstSize == burstIn))

  override def burstSize: Int = burstOut

  /**
   * Which core that should access memory next.
   */
  var nextAccess: Int = 0

  var coresDone: Array[Boolean] = Array.fill(cores.length){false}

  /**
   * Used for tracking if the cache is busy. First comes the cores being serviced, then how long is left.
   * When at 0, it means the cache is ready to service
   * When the cache returns a latency, the busy will be set to it and reduced each cycle trigger
   * While busy > 0 the cache should not service any access.
   * The third element is whether we are waiting for the request with the parent.
   * The fourth element is wether we have yet to issue the request to parent.
   * If some, then the address still needs to be requested
   */
  var busy: Option[(Int,Int,Boolean,Option[Long])] = None

  def checkDone(coreId: Int): Unit = {
    if(cores(coreId).isDone() && !coresDone(coreId)) {
      val shouldSubstitute = onDone(coreId, cache)
      if(shouldSubstitute.isDefined) {
        cores(coreId) = shouldSubstitute.get
        coresDone(coreId) = false
      } else {
        coresDone(coreId) = true
      }
    }
  }

  def checkAndServe(): Unit = {
    if (busy.isDefined && busy.get._2 == 0 && !busy.get._3) {
      cores(busy.get._1).serveMemoryAccess()
      checkDone(busy.get._1)

      busy = None
    }
  }

  override def serveMemoryAccess(): Unit = {
    if(busy.isDefined) {
      assert(!(busy.get._3 && busy.get._4.isDefined)) //
      busy = Some((busy.get._1,busy.get._2,false,busy.get._4))
    }

    checkAndServe()
  }

  def checkCoreReq(): Unit = {
    if(busy.isDefined) {
      // Already waiting for issued request
    } else {
      // check if the next core has a request
      val addr = cores(nextAccess).requestMemoryAccess()

      val result = if(addr.isDefined) {

        if(!cache.getCacheLine(addr.get, nextAccess)) {
          // Miss
          busy = Some((nextAccess, latency,true, Some(addr.get)))
        } else {
          // Hit
          busy = Some((nextAccess, latency,false, None))
        }
        reportHits(nextAccess, !busy.get._3)
      } else {
        // No request from core
      }
    }
  }

  override def requestMemoryAccess(): Option[Long] = {
    checkCoreReq()
    if(busy.isDefined && busy.get._3 && busy.get._4.isDefined) {
      // Need to issue a request
      val addr = busy.get._4.get
      busy = Some((busy.get._1, busy.get._2, true, None))
      Some(addr)
    } else {
      None
    }
  }

  override def triggerCycle(): Unit = {
    checkCoreReq()
    if(busy.isDefined && busy.get._3) {
      // While waiting for parent to respond, stall arbitration
    } else {
      if(busy.isDefined && busy.get._2>0 && !busy.get._3) {
        busy = Some((busy.get._1, busy.get._2-1, busy.get._3, busy.get._4))
      } else {
        checkAndServe()
        nextAccess = (nextAccess + 1) % cores.length
      }
    }

    cache.advanceCycle()
    cores.zipWithIndex.foreach(coreIdx => {
      checkDone(coreIdx._2)
      coreIdx._1.triggerCycle()
    })
  }

  override def isDone(): Boolean = {
    busy.isEmpty && cores.zipWithIndex.forall(coreIdx => coreIdx._1.isDone() && coresDone(coreIdx._2))
  }
}

class TraceTraffic(s: Int, source: Iterator[MemAccess], reportLatency: (Int) => Unit) extends Traffic {

  override def burstSize: Int = s

  var nextAccess: Array[MemAccess] = Array.empty

  /**
   * Tracks the current clock cycle
   */
  var clock: Long = 0;

  /**
   * Tracks how many clock cycles were spent stalled, waiting for memory
   */
  var stalls: Long = 0;

  /**
   * Whether we are waiting to bet served and how long we've been waiting
   */
  var waitingForServe: Option[Int] = None;

  def log2(x: Int): Int = {
    (math.log(x) / math.log(2)).toInt
  }

  /**
   * Returns whether an access with the given timestamp should be issued
   * @param time
   * @return
   */
  def shouldIssue(time: Long): Boolean = {
    time <= clock - stalls
  }

  override def serveMemoryAccess(): Unit = {
    assert(waitingForServe.isDefined)
    reportLatency(waitingForServe.get)
    waitingForServe = None;

  }

  def isBurstAligned(address: Long): Boolean = {
    address % burstSize == 0
  }
  def closestBurstAlignedAddress(address: Long): Long = {
    address - (address % burstSize)
  }
  def accessesNeeded(size: Int, address: Long): Int = {
    val aligned = isBurstAligned(address);

    if(aligned) {
      Math.ceil(size.toDouble / burstSize).toInt
    } else {
      val initialBurstSize = burstSize - (address % burstSize)
      val remainingDataSize = size - initialBurstSize
      Math.ceil(remainingDataSize.toDouble / burstSize).toInt + 1
    }
  }

  override def requestMemoryAccess(): Option[Long] = {
    if(waitingForServe.isDefined) return None;

    if(nextAccess.length>0) {
      val access = nextAccess(0)

      if(shouldIssue(access.time)) {

        nextAccess = nextAccess.drop(1)

        // Check for alignment with burstSize
        val startAddr = closestBurstAlignedAddress(access.address)
        val nrAccessesNeeded= accessesNeeded(access.accessSize, access.address)
        if (nrAccessesNeeded > 1) {
          // We need to split the access into chunks
          Range.apply(0, nrAccessesNeeded).foreach(idx => {
            nextAccess = nextAccess :+ new MemAccess(log2(burstSize), access.isRead, startAddr + (burstSize * idx), access.time)
          })

          // Run again on the split accesses
          requestMemoryAccess()
        } else {
          waitingForServe = Some(0)
          Some(startAddr)
        }
      } else {
        // The next access is not ready
        None
      }
    } else {
      if(source.hasNext) {
        nextAccess = nextAccess :+ source.next()
        requestMemoryAccess()
      } else {
        // We are done
        None
      }
    }
  }

  override def triggerCycle(): Unit = {
    clock += 1
    if(waitingForServe.isDefined) {
      stalls += 1
      waitingForServe = Some(waitingForServe.get+1)
    };
  }

  override def isDone(): Boolean = {
    nextAccess.isEmpty && waitingForServe.isEmpty && !source.hasNext
  }
}

/**
 *
 * @param s Burst size to caller
 * @param sCache Internal burst size between the given cache and the source
 * @param cache
 * @param source
 */
//class CachedTraceTraffic(s: Int, sCache:Int, cache: SoftCache, source: Iterator[MemAccess]) extends Traffic {
//
//  var internArbiter = new CacheArbiter(sCache, cache, Array(new TraceTraffic(sCache, source)))
//
//  override def burstSize: Int = s
//
//  override def triggerCycle(): Unit = {
//
//
//  }
//
//  override def requestMemoryAccess(): Option[Long] = {
//    var req = traffic.requestMemoryAccess()
//    if(req.isDefined) {
//      var latency = cache.getCacheLine(req.get, 0)
//    } else {
//      None
//    }
//  }
//
//  override def serveMemoryAccess(): Unit = {
//
//
//  }
//}

// Can be run using the command: sbt "runMain Sim"
object Sim {
  private val DAWPattern: Regex = """DAW \[ dsz=(\d+), uaddr=(0x[0-9a-f]+), faddr=(0x[0-9a-f]+), faddr_valid=(\d+), icnt=(0x[0-9a-f]+), tstamp=(\d+), iaddr=(0x[0-9a-f]+), iaddr_valid=(\d+), absolute_tstamp=(\d+), abtst_valid=(\d+)  \]""".r
  private val DARPattern: Regex = """DAR \[ dsz=(\d+), uaddr=(0x[0-9a-f]+), faddr=(0x[0-9a-f]+), faddr_valid=(\d+), icnt=(0x[0-9a-f]+), tstamp=(\d+), iaddr=(0x[0-9a-f]+), iaddr_valid=(\d+), absolute_tstamp=(\d+), abtst_valid=(\d+)  \]""".r
  private val DAWSPattern: Regex = """DAWS \[ dsz=(\d+), faddr=(0x[0-9a-f]+), icnt=(0x[0-9a-f]+), iaddr=(0x[0-9a-f]+), iaddr_valid=(\d+), absolute_tstamp=(\d+), abtst_valid=(\d+)  \]""".r
  private val DARSPattern: Regex = """DARS \[ dsz=(\d+), faddr=(0x[0-9a-f]+), icnt=(0x[0-9a-f]+), iaddr=(0x[0-9a-f]+), iaddr_valid=(\d+), absolute_tstamp=(\d+), abtst_valid=(\d+)  \]""".r

  def sanitizeDsz(dsz: Int): Int = {
    if(0 <= dsz || dsz <= 6) {
      dsz
    } else {
      throw new IllegalArgumentException(s"Invalid dsz: $dsz")
    }
  }
  def sanitizeAddr(faddr: Long, faddrValid: Int): Long = {
    if(faddrValid == 1) {
      faddr
    } else {
      throw new IllegalArgumentException(s"Invalid faddr_valid: $faddrValid")
    }
  }
  def sanitizeTstamp(absoluteTstamp: Long, abtstValid: Int, lastTime: Long): Long = {

    var time = absoluteTstamp
    if(abtstValid != 1) {
      throw new IllegalArgumentException(s"Invalid abtst_valid : $abtstValid")
    } else if (lastTime > absoluteTstamp) {
      throw new IllegalArgumentException(s"Invalid time (last>current): $lastTime > $absoluteTstamp")
    } else {
      time
    }
  }

  def loadAccesses(path: String): Iterator[MemAccess] = {
    val source = Source.fromFile(path)

    var lastTime:Long = 0
    source.getLines().map(line => {
      line match {
        case DAWPattern(dsz, uaddr, faddr, faddr_valid, icnt, tstamp, iaddr, iaddr_valid, absolute_tstamp, abtst_valid) =>
          Some(new MemAccess(
            sanitizeDsz(dsz.toInt),
            false,
            sanitizeAddr(java.lang.Long.parseLong(faddr.drop(2), 16), faddr_valid.toInt),
            sanitizeTstamp(absolute_tstamp.toLong, abtst_valid.toInt, lastTime)
          ))
        case DARPattern(dsz, uaddr, faddr, faddr_valid, icnt, tstamp, iaddr, iaddr_valid, absolute_tstamp, abtst_valid) =>
          Some(new MemAccess(
            sanitizeDsz(dsz.toInt),
            true,
            sanitizeAddr(java.lang.Long.parseLong(faddr.drop(2), 16), faddr_valid.toInt),
            sanitizeTstamp(absolute_tstamp.toLong, abtst_valid.toInt, lastTime)
          ))
        case DAWSPattern(dsz, faddr, icnt, iaddr, iaddr_valid, absolute_tstamp, abtst_valid) =>
          Some(new MemAccess(
            sanitizeDsz(dsz.toInt),
            false,
            java.lang.Long.parseLong(faddr.drop(2), 16),
            sanitizeTstamp(absolute_tstamp.toLong, abtst_valid.toInt, lastTime)
          ))
        case DARSPattern(dsz, faddr, icnt, iaddr, iaddr_valid, absolute_tstamp, abtst_valid) =>
          Some(new MemAccess(
            sanitizeDsz(dsz.toInt),
            true,
            java.lang.Long.parseLong(faddr.drop(2), 16),
            sanitizeTstamp(absolute_tstamp.toLong, abtst_valid.toInt, lastTime)
          ))
        case _ => None
      }
    }).filter(a => a.isDefined).map(a => {
      lastTime = a.get.time
      a.get
    })
  }

  def main(args: Array[String]): Unit = {
    println("Running Simulator")


    val traceFiles = Array(
//      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_64.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_65.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_66.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_67.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_68.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_69.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_70.txt",
      "2024-05-21-Trace/Trace_dtu/dtrace_71.txt",
    )

    var coreAccesses: Array[Int] = Array.fill(traceFiles.length){0}
    var cumulativeLatencies: Array[Int] = Array.fill(traceFiles.length){0}
    var hits: Array[Int] = Array.fill(traceFiles.length){0}
    var l2Hits: Array[Int] = Array.fill(traceFiles.length){0}

    val l1Latency = 1
    val l2Latency = 8
    val memLatency = 16
    val l1BurstSize = 4
    val l2BurstSize = 16
    val memBurstSize = 32

    var l1Arbs = traceFiles.zipWithIndex.map(pathIdx => {
      val l1Cache = new LruCache(16,4,16) // 16B line, 4-way set associative 1KiB cache
      new Arbiter(
        l1BurstSize, l2BurstSize,
        l1Cache, l1Latency,
        Array(new TraceTraffic(l1BurstSize, loadAccesses(pathIdx._1), latency=> {
          cumulativeLatencies(pathIdx._2) += latency
          coreAccesses(pathIdx._2) += 1
          printf("{ %d, %d }\n", pathIdx._2, latency)
        })),
        (_,_) => {
          None
        },
        (_, isHit) => {
          if(isHit) hits(pathIdx._2)+=1;
        }
      )
    })

//    var l2Cache = new LruCache(64, 8, 16) // 64B line, 8-way set associative 8KB cache
//    val l2OnDone  = (_:Int,_:SoftCache) => None;

//    var l2Cache = new PartitionedCache(64, 8, 16) // 64B line, 8-way set associative 8KB cache
//    l2Cache.assignWay(0,0)
//    l2Cache.assignWay(0,1)
//    l2Cache.assignWay(0,2)
//    l2Cache.assignWay(0,3)
//    l2Cache.assignWay(1,4)
//    l2Cache.assignWay(1,5)
//    l2Cache.assignWay(1,6)
//    l2Cache.assignWay(1,7)
//    val l2OnDone  = (coreId:Int,cache:SoftCache) => {
//      cache match {
//        case parCache: PartitionedCache => {
//          parCache.unassignCore(coreId)
//          None
//        }
//        case _ => {
//          assert(false)
//          None
//        }
//      }
//    };

//    var l2Cache = new ContentionCache(64, 8, 16, memLatency) // 64B line, 8-way set associative 8KB cache
//    l2Cache.setCriticality(0, 1000)
//    l2Cache.setCriticality(1, 1000)
//    val l2OnDone  = (coreId:Int,cache:SoftCache) => {
//      cache match {
//        case conCache: ContentionCache => {
//          conCache.unassign(coreId)
//          None
//        }
//        case _ => {
//          assert(false)
//          None
//        }
//      }
//    };

    var l2Cache = new ContentionPartCache(64, 8, 16, memLatency) // 64B line, 8-way set associative 8KB cache
    l2Cache.setCriticality(0, 1000)
    l2Cache.setCriticality(1, 1000)
    l2Cache.assignWay(0,0)
    l2Cache.assignWay(0,1)
    l2Cache.assignWay(0,2)
    l2Cache.assignWay(0,3)
    l2Cache.assignWay(1,4)
    l2Cache.assignWay(1,5)
    l2Cache.assignWay(1,6)
    l2Cache.assignWay(1,7)
    val l2OnDone  = (coreId:Int,cache:SoftCache) => {
      cache match {
        case conCache: ContentionPartCache => {
          conCache.unassign(coreId)
          None
        }
        case _ => {
          assert(false)
          None
        }
      }
    };

//    var l2Cache = new TimeoutCache(64, 8, 16, 100000) // 64B line, 8-way set associative 8KB cache
//    l2Cache.setPriority(0,0)
//    l2Cache.setPriority(0,1)
//    l2Cache.setPriority(0,2)
//    l2Cache.setPriority(0,3)
//    l2Cache.setPriority(1,4)
//    l2Cache.setPriority(1,5)
//    l2Cache.setPriority(1,6)
//    l2Cache.setPriority(1,7)
//    val l2OnDone  = (coreId:Int,cache:SoftCache) => {
//      cache match {
//        case timeCache: TimeoutCache => {
//          timeCache.removePriority(coreId)
//          None
//        }
//        case _ => {
//          assert(false)
//          None
//        }
//      }
//    };

    var l2Arb = new Arbiter(l2BurstSize,memBurstSize,
      l2Cache, l2Latency,
      l1Arbs.toArray,
      l2OnDone,
      (coreId, isHit) => {
        if(isHit) l2Hits(coreId)+=1;
      }
    )
    var memArb = new Arbiter(
      memBurstSize,memBurstSize,
      // Main memory always hits
      new SoftCache(memBurstSize*2,1,1) {
        override def getCacheLine(addr: Long, core: Int): Boolean = true;
        override def isHit(addr: Long): Option[(Int, Int)] = { None }
        override def printAll(): Unit = {}
      },
      memLatency,
      Array(l2Arb),
      (_,_) => None,
      (_,_) => Unit
    )

    while(!memArb.isDone()) {
      val trig = memArb.triggerCycle()
    }

    for(i <- 0 until traceFiles.length) {
      printf("Count: %d, L1 Hits: %d, L1 Hit Pct: %f, L2 Hits: %d, L2 Hit Pct: %f, Avg. Latency: %f\n",
        coreAccesses(i),
        hits(i),
        hits(i).toDouble/coreAccesses(i).toDouble,
        l2Hits(i),
        l2Hits(i)/(coreAccesses(i)-hits(i)).toDouble,
        (cumulativeLatencies(i).toDouble)/
          (coreAccesses(i).toDouble) )
    }
  }

}
