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

  def triggerCycle();

  def requestMemoryAccess(): Option[Long];

  def serveMemoryAccess();

  def isPowerOfTwo(x: Int): Boolean =
  {
    (x & (x - 1)) == 0
  }

}

class CacheArbiter(burstSize: Int, cache: SoftCache, cores: Array[Traffic]) {
  require(cache.lineLength>=burstSize)
  require(cache.lineLength%burstSize == 0)
  require(cores.forall( t => t.burstSize == burstSize))

  /**
   * Which core that should access memory next.
   */
  var nextAccess: Int = 0

  /**
   * Used for tracking if the cache is busy. First comes the cores being serviced, then how long is left.
   * When at 0, it means the cache is ready to service
   * When the cache returns a latency, the busy will be set to it and reduced each cycle trigger
   * While busy > 0 the cache should not service any access
   */
  var busy: Option[(Int,Int)] = None

  /**
   * Triggers a "round" of memory access.
   * A round start by allowing 1 core to access memory (if cache not busy), then it triggers a cycle.
   * @return The core that accesses the cache and whether it was a hit
   */
  def trigger(): Option[(Int, Boolean)]   = {

    if(busy.isDefined && busy.get._2 == 0) {
      cores(busy.get._1).serveMemoryAccess()
      busy = None
    }

    val result = if(busy.isDefined && busy.get._2>0) {
      busy = Some((busy.get._1, busy.get._2-1))
      None
    } else {
      val result = cores(nextAccess).requestMemoryAccess().map( addr => {
        cache.getCacheLine(addr, nextAccess)
      }).map(latency => {
        busy = Some((nextAccess, latency))
        (nextAccess, latency == cache.shortLatency)
      })

      nextAccess = (nextAccess+1)%cores.length
      result
    }
    cache.advanceCycle()
    cores.foreach(core => core.triggerCycle())

    result
  }
}

class TraceTraffic(s: Int, source: Iterator[MemAccess]) extends Traffic {

  override def burstSize: Int = s

  var nextAccess: Array[MemAccess] = Array.empty

  override def triggerCycle(): Unit = {


  }


  def log2(x: Int): Int = {
    (math.log(x) / math.log(2)).toInt
  }

  override def requestMemoryAccess(): Option[Long] = {
    if(nextAccess.length>0) {
      val access = nextAccess(0)
      nextAccess = nextAccess.drop(1)

      if(access.accessSize>burstSize) {
        // We need to split the access into chunks
        val chunks = access.accessSize/burstSize
        Range.apply(0, chunks).foreach(idx => {
          nextAccess = nextAccess :+ new MemAccess(log2(burstSize),access.isRead, access.address + (burstSize*idx), access.time)
        })

        // Run again on the split accesses
        requestMemoryAccess()
      } else {
        Some(access.address)
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

  override def serveMemoryAccess(): Unit = {


  }
}

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

    if((lastTime %10) == 0) {
      var time = absoluteTstamp * 10
      if (abtstValid == 1) {
        if (time == (lastTime % 10)) {
          time = lastTime + 1
        }
        time
      } else {
        throw new IllegalArgumentException(s"Invalid abtst_valid: $abtstValid")
      }
    }  else {
      throw new IllegalArgumentException(s"More than 10 identical times: $absoluteTstamp")
    }

  }

  def main(args: Array[String]): Unit = {
    println("Running Simulator")

    var cache = new LruCache(32, 8, 16, 4, 12)


    val source = Source.fromFile("2024-05-21-Trace/Trace_dtu/dtrace_64.txt")
    var count = 0
    var lastTime:Long = 0
    var hits = 0
    var misses = 0
    source.getLines().foreach( line => {
      val obj = line match {
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

      obj.foreach(a => {
        lastTime = a.time
        count += 1
        a.prettyPrint()
        val latency = cache.getCacheLine(a.address, 0)
        if(latency == 4) hits +=1 else misses += 1
        printf(": %d\n", cache.getCacheLine(a.address, 0))
      })
    })
    printf("Count: %d\n", count)
    printf("Hits: %d\n", hits)
    printf("Misses: %d\n", misses)
    printf("Hit percentage: %f\n", (hits).toDouble/((hits+misses).toDouble))
  }

}
