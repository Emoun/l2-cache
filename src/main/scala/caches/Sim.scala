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

}

class Arbiter(burstIn: Int, burstOut: Int, cache: SoftCache, latency:Int, cores: Array[Traffic])
  extends Traffic
{
  require(cache.lineLength>=burstIn)
  require(cache.lineLength%burstIn == 0)
  require(cores.forall( t => t.burstSize == burstIn))

  override def burstSize: Int = burstOut

  /**
   * Which core that should access memory next.
   */
  var nextAccess: Int = 0

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

  override def serveMemoryAccess(): Unit = {
    if(busy.isDefined) {
      assert(!(busy.get._3 && busy.get._4.isDefined)) //
      busy = Some((busy.get._1,busy.get._2,false,busy.get._4))
    }

    if(busy.isDefined && busy.get._2 == 0 && !busy.get._3) {
      cores(busy.get._1).serveMemoryAccess()
      busy = None
    }
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
          println(s"{ $nextAccess, $addr.get, Miss }")
          busy = Some((nextAccess, latency,true, Some(addr.get)))
        } else {
          // Hit
          println(s"{ $nextAccess, $addr.get, Hit }")
          busy = Some((nextAccess, latency,false, None))
        }
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
        if (busy.isDefined && busy.get._2 == 0 && !busy.get._3) {
          cores(busy.get._1).serveMemoryAccess()
          busy = None
        }
        nextAccess = (nextAccess + 1) % cores.length
      }
    }

    cache.advanceCycle()
    cores.foreach(core => core.triggerCycle())
  }
}

class TraceTraffic(s: Int, source: Iterator[MemAccess]) extends Traffic {

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

  var waitingForServe = false;

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
    waitingForServe = false;
  }
  override def requestMemoryAccess(): Option[Long] = {
    if(waitingForServe) return None;

    if(nextAccess.length>0) {
      val access = nextAccess(0)

      if(shouldIssue(access.time)) {

        nextAccess = nextAccess.drop(1)

        if (access.accessSize > burstSize) {
          // We need to split the access into chunks
          val chunks = access.accessSize / burstSize
          Range.apply(0, chunks).foreach(idx => {
            nextAccess = nextAccess :+ new MemAccess(log2(burstSize), access.isRead, access.address + (burstSize * idx), access.time)
          })

          // Run again on the split accesses
          requestMemoryAccess()
        } else {
          waitingForServe = true
          Some(access.address)
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
    if(waitingForServe) stalls += 1;
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

    var cache = new LruCache(32, 8, 16)
    val l2BurstSize = 16
    val memBurstSize = 32

    val traceFiles = Array(
      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_64.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_65.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_66.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_67.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_68.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_69.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_70.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_71.txt",
    )

    var l2Arb = new Arbiter(l2BurstSize,memBurstSize,
      cache, 4,
      traceFiles.map(path => new TraceTraffic(l2BurstSize, loadAccesses(path)))
    )
    var memArb = new Arbiter(
      memBurstSize,memBurstSize,
      // Main memory always hits
      new SoftCache(memBurstSize*2,1,1) {
        override def getCacheLine(addr: Long, core: Int): Boolean = true;
        override def isHit(addr: Long): Option[(Int, Int)] = { None }
        override def printAll(): Unit = {}
      },
      16,
      Array(l2Arb)
    )

    var counts = Array.fill(traceFiles.length){0}
    var hits = Array.fill(traceFiles.length){0}
    for(i <- 0 until 100//72056550*2
    ) {
      val trig = memArb.triggerCycle()
    }

    for(i <- 0 until counts.length) {
      printf("Count: %d, Hits: %d, Misses, %d, Hit perc.: %f\n", counts(i), hits(i), counts(i)-hits(i), (hits(i)).toDouble/((counts(i)).toDouble) )
    }
  }

}
