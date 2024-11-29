package caches

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import java.nio.file.{DirectoryStream, Files, Paths}
import scala.jdk.CollectionConverters._

// Define case classes for DAW and DAR
sealed trait LogEntry
case class DAW(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry
case class DAR(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry
case class DAWS(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry
case class DARS(dsz: Int, addr: Long, timeStamp: Long) extends LogEntry

sealed trait RequestState
// Request is ready but has yet to be issued
case object Waiting extends RequestState
// Request has been issued but has yet to be replied to
case object Issued extends RequestState
// Request has been issued and replied to and is currently being service with the given latency left
case class Servicing(latency: Int) extends RequestState


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

/**
 * Trait for traffic generating object.
 *
 * Objects are prompted for memory requests with 'requestMemoryAccess', which may return the address
 * being requested and a token identifying the address.
 * When the request has been serviced, 'serveMemoryAccess' is called with the previously-mentioned token.
 *
 * @tparam S Type of request token
 */
trait Traffic[S] {

  def burstSize: Int;
  require(isPowerOfTwo(burstSize))

  /**
   * Exernal services a previously issued request with the given token.
   * Returns whether the serve is accepted. If not, external must try again.
   * @param token
   * @return
   */
  def serveMemoryAccess(token: S): Boolean;

  /**
   * Request a memory access from external.
   * If none, no memory access is requested.
   * If Some, first is the address requested, the second is whether the access is a read,
   * and the third isa token identifying the request.
   * @return
   */
  def requestMemoryAccess(): Option[(Long, Boolean, S)];

  def triggerCycle();

  def isPowerOfTwo(x: Int): Boolean =
  {
    (x & (x - 1)) == 0
  }

  def isDone(): Boolean;

}

/**
 * Generates traffic from a trace file.
 *
 * Only generates one request at a time, waiting for a serve before issuing any other request.
 * Therefore, no request token is needed to identify the request.
 *
 * @param s
 * @param source
 * @param reportLatency
 */
class TraceTraffic(s: Int, source: Iterator[MemAccess], reportLatency: (Int) => Unit) extends Traffic[Unit] {

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

  override def serveMemoryAccess(token: Unit): Boolean = {
    assert(waitingForServe.isDefined)
    reportLatency(waitingForServe.get)
    waitingForServe = None;
    true
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

  override def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = {
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
          Some((startAddr, access.isRead, ()))
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

class RoundRobinArbiter[C<:Traffic[Unit]](
  burstOut: Int,
  latency:Int,
  cores: Array[C],
  // Takes coreId, returns whether  the core needs to be changed to the given traffic
  onDone: (Int) => Option[C],
  allowMultiple: Boolean = false,
)
  // request token is coreId
  extends Traffic[Int]
{
  require(cores.length > 0)
  require(cores.forall( c => c.burstSize == cores(0).burstSize))

  override def burstSize: Int = burstOut
  require(burstSize >= cores(0).burstSize)

  /**
   * Which core that should access memory next.
   */
  var nextAccess: Int = 0

  var coresDone: Array[Boolean] = Array.fill(cores.length){false}

  /**
   * Used for tracking the service of a core.
   * If none, no core is waiting for the arbiter
   * The first is the core being serviced
   * The second is the state
   * The third is the address
   * The fourth is whether request is a read
   */
  var requestQueue: Array[(Int,RequestState, Long, Boolean)] = Array.empty

  def checkDone(coreId: Int): Unit = {
    if(cores(coreId).isDone() && !coresDone(coreId)) {
      val shouldSubstitute = onDone(coreId)
      if(shouldSubstitute.isDefined) {
        cores(coreId) = shouldSubstitute.get
        coresDone(coreId) = false
      } else {
        coresDone(coreId) = true
      }
    }
  }

  def isBusy(): Boolean = {
    getServicingIdx().isDefined
  }

  def getServicingIdx(): Option[Int] = {
    if(!requestQueue.isEmpty) {
      var beingServicedIdx = requestQueue.zipWithIndex.filter (elem => {
        val (_, state, _, _) = elem._1
        state match {
          case Servicing (_) => true
          case _ => false
        }
      }).map(elem => elem._2)

      assert(beingServicedIdx.length <=1)

      if(beingServicedIdx.isEmpty) {
        None
      } else {
        Some(beingServicedIdx(0))
      }
    } else {
      None
    }
  }

  var internServed = false
  def checkAndServe(): Unit = {
    getServicingIdx() match {
      case Some(idx) if requestQueue(idx)._2 == Servicing(0)=> {
        assert(!internServed)
        cores(requestQueue(idx)._1).serveMemoryAccess(())
        checkDone(requestQueue(idx)._1)

        // Remove the served element in the queue
        requestQueue = requestQueue.zipWithIndex.filter(elem => elem._2 != idx).map(elem => elem._1)

        internServed = true
      }
      case _ => ()
    }
  }

  override def serveMemoryAccess(token: Int): Boolean = {
    assert(!requestQueue.isEmpty)

    if(getServicingIdx().isEmpty){
      val reqIdx = requestQueue.zipWithIndex.filter(elem => {
        val coreId = elem._1._1
        coreId == token
      }).map(elem => elem._2)

      assert(reqIdx.length == 1)
      assert(requestQueue(reqIdx(0))._2 == Issued)

      requestQueue.update(reqIdx(0), (requestQueue(reqIdx(0))._1, Servicing(latency), requestQueue(reqIdx(0))._3, requestQueue(reqIdx(0))._4))
      true
    } else {
      false
    }
  }

  var internRequested = false

  def checkCoreReq(): Unit = {
    if(!requestQueue.isEmpty && !allowMultiple) {
      // Already waiting to issued request
    } else if(!internRequested && requestQueue.find(elem => elem._1 ==nextAccess).isEmpty){
      // check if the next core has a request
      val req = cores(nextAccess).requestMemoryAccess()

      if(req.isDefined) {
        requestQueue = requestQueue :+ (nextAccess, Waiting, req.get._1, req.get._2)
      } else {
        // No request from core
      }
      nextAccess = (nextAccess + 1) % cores.length
      internRequested = true
    }
  }

  override def requestMemoryAccess(): Option[(Long, Boolean, Int)] = {
    checkAndServe()
    checkCoreReq()

    val waitingInQueue = requestQueue.zipWithIndex.filter(elem => {
      val (_, state, _, _) = elem._1
      state==Waiting
    }).map(elem => elem._2)

    if(!waitingInQueue.isEmpty) {
      val waitIdx = waitingInQueue(0)
      requestQueue(waitIdx) = (requestQueue(waitIdx)._1, Issued, requestQueue(waitIdx)._3, requestQueue(waitIdx)._4)
      Some((requestQueue(waitIdx)._3, requestQueue(waitIdx)._4, requestQueue(waitIdx)._1))
    } else {
      None
    }
  }

  override def triggerCycle(): Unit = {
    checkAndServe()
    checkCoreReq()

    val servIdx = getServicingIdx()
    if(servIdx.isDefined) {
      requestQueue(servIdx.get) match {
        case (_,Servicing(l),_,_) => {
          assert(l>0)
          requestQueue(servIdx.get) = (requestQueue(servIdx.get)._1, Servicing(l-1), requestQueue(servIdx.get)._3, requestQueue(servIdx.get)._4)
        }
        case _ => assert(false) // unreachable
      }
    }

    cores.zipWithIndex.foreach(coreIdx => {
      checkDone(coreIdx._2)
      coreIdx._1.triggerCycle()
    })
    internRequested = false
    internServed = false
  }

  override def isDone(): Boolean = {
    requestQueue.isEmpty && cores.zipWithIndex.forall(coreIdx => coreIdx._1.isDone() && coresDone(coreIdx._2))
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

  def emptyDirectory(dir: java.nio.file.Path): Unit = {
    val stream: DirectoryStream[java.nio.file.Path] = Files.newDirectoryStream(dir)
    try {
      for (entry <- stream.asScala) {
        if (Files.isRegularFile(entry)) {
          Files.delete(entry)
        }
      }
    } finally {
      stream.close()
    }
  }

  def runConfig(
    configId: String,
    newL1Cache: () => SoftCache,
    l2Cache: SoftCache,
    l2OnDone: () => Unit,
    l1Latency: Int,
    l2Latency: Int,
    mainMemLatency: Int,
    accessTimesFile: BufferedWriter,
    coreStatsFile: BufferedWriter,
    globalStatsFile: BufferedWriter,
  ): Unit = {
    val traceFiles = Array(
//      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
//      "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
//            "2024-05-21-Trace/Trace_dtu/dtrace_test.txt",
            "2024-05-21-Trace/Trace_dtu/dtrace_64.txt",
            "2024-05-21-Trace/Trace_dtu/dtrace_65.txt",
      //      "2024-05-21-Trace/Trace_dtu/dtrace_66.txt",
      //      "2024-05-21-Trace/Trace_dtu/dtrace_67.txt",
      //      "2024-05-21-Trace/Trace_dtu/dtrace_68.txt",
      //      "2024-05-21-Trace/Trace_dtu/dtrace_69.txt",
      //      "2024-05-21-Trace/Trace_dtu/dtrace_70.txt",
      //      "2024-05-21-Trace/Trace_dtu/dtrace_71.txt",
    )

    val l1BurstSize = 4

    var clock: Long = 0;
    var coreAccesses: Array[Int] = Array.fill(traceFiles.length){0}
    var l2Accesses: Array[Int] = Array.fill(traceFiles.length){0}
    var cumulativeLatencies: Array[Int] = Array.fill(traceFiles.length){0}
    var hits: Array[Int] = Array.fill(traceFiles.length){0}
    var l2Hits: Array[Int] = Array.fill(traceFiles.length){0}
    var l2HitsAfterMiss: Array[Int] = Array.fill(traceFiles.length){0}
    var mainMemAccesses: Int = 0

    var l1Cache = newL1Cache()
    val l2BurstSize = l1Cache.lineLength
    var l1CacheTraffic = traceFiles.zipWithIndex.map(pathIdx => {
      new CacheTraffic(
        l2BurstSize,
        new RoundRobinArbiter(
          l1BurstSize,
          l1Latency,
          Array(new TraceTraffic(l1BurstSize, loadAccesses(pathIdx._1), latency => {
            cumulativeLatencies(pathIdx._2) += latency
            coreAccesses(pathIdx._2) += 1
            printf("Clock: (%d), Core: (%d), Latency: (%d)\n", clock, pathIdx._2, latency)
            accessTimesFile.write(f"${pathIdx._2},$clock,$latency\n")
          })),
          (_) => {
            None
          }
        ),
        l1Cache,
        (_, hitType) => {
          if(hitType.isHit()) hits(pathIdx._2)+=1;
        }
      )
    })

    var l2CacheTraffic = new BufferedCacheTraffic(
      l2Cache.lineLength,
      new RoundRobinArbiter(
        l2BurstSize,
        l2Latency,
        l1CacheTraffic,
        (_:Int) => {
          l2OnDone()
          None
        },
        true
      ),
      l2Cache, // 64B line, 8-way set associative 8KB cache
      (coreId, hitType) => {
        l2Accesses(coreId) += 1
        hitType match {
          case Hit => l2Hits(coreId)+=1
          case HitAfterMiss => {
            l2HitsAfterMiss(coreId)+=1
            l2Accesses(coreId) -= 1 //Already counted during the miss
          }
          case _ => ()
        }
      }
    )

    var MainMemTraffic = new CacheTraffic(
      l2Cache.lineLength,
      new RoundRobinArbiter(
        l2Cache.lineLength,
        mainMemLatency,
        Array(l2CacheTraffic),
        (_) => {
          None
        }
      ),
      new MainMemory(l2Cache.lineLength),
      (_,isHit) => {
        assert(isHit.isHit())
        mainMemAccesses += 1
      }
    )

    while(!MainMemTraffic.isDone()) {
      MainMemTraffic.triggerCycle()
      clock += 1
    }

    for(i <- 0 until traceFiles.length) {
      val l1Misses = coreAccesses(i) - hits(i)
      val l1WriteBack = l2Accesses(i) - l1Misses
      printf("Count: %d, L1 Hits: %d, L1 Hit Pct: %f, L1 Write-Backs: %d, L2 Accesses: %d, L2 Hits: %d(%d), L2 Hit Pct: %f(%f), Avg. Latency: %f\n",
        coreAccesses(i),
        hits(i),
        hits(i).toDouble/coreAccesses(i).toDouble,
        l1WriteBack,
        l2Accesses(i),
        l2Hits(i),
        l2HitsAfterMiss(i),
        l2Hits(i)/(l2Accesses(i)).toDouble,
        (l2Hits(i)+l2HitsAfterMiss(i))/(l2Accesses(i)).toDouble,
        (cumulativeLatencies(i).toDouble)/
          (coreAccesses(i).toDouble) )
      coreStatsFile.write(f"$i,${coreAccesses(i)},${hits(i)},${l1WriteBack},${l2Accesses(i)},${l2Hits(i)+l2HitsAfterMiss(i)}\n")
    }
    val l2Misses = l2Accesses.sum - l2Hits.sum - l2HitsAfterMiss.sum
    printf("L2 Write-Backs: %d\n", mainMemAccesses - l2Misses)
    globalStatsFile.write(f"l2WriteBack,${mainMemAccesses - l2Misses}\n")
  }

  def main(args: Array[String]): Unit = {
    println("Running Simulator")

    val path = Paths.get(args(0))
    if (!Files.isDirectory(path)) {
      println(s"${args(0)} is not a directory.")
      return
    }
    emptyDirectory(path)

    ////////////////////////// Huawei settings /////////////////////////
//    val l1Latency = 1
//    val l2Latency = 15
//    val memLatency = 60
//    val l1BurstSize = 4
//    val l2BurstSize = 64
//    val memBurstSize = 64

    // 32KB L1 cache
//    val l1LineSize=l2BurstSize
//    val l1Ways = 4
//    val l1Sets = 128

    // 1MB L2 cache
//    val l2LineSize=memBurstSize
//    val l2Ways = 8
//    val l2Sets = 2048

    //////////////////////// Original Setting//////////////////////////
    val l1Latency = 1
    val l2Latency = 8
    val memLatency = 20
    val l1BurstSize = 4
    val l2BurstSize = 16
    val memBurstSize = 64

    // 1KB L1 cache
    val l1LineSize=l2BurstSize
    val l1Ways = 4
    val l1Sets = 16

    // 8KB L2 cache
    val l2LineSize=memBurstSize
    val l2Ways = 8
    val l2Sets = 16

    var l2Cache = new LruCache(l2LineSize, l2Ways, l2Sets)
    val l2OnDone  = () => {};

//    var l2Cache = new PartitionedCache(l2LineSize, l2Ways, l2Sets)
//    for(i <- 0 until l2Ways/2) {
//      l2Cache.assignWay(0,i)
//      l2Cache.assignWay(1,(l2Ways/2)+i)
//    }
//    val l2OnDone  = (coreId:Int) => {
//      l2Cache.unassignCore(coreId)
//      None
//    };

//    var l2Cache = new ContentionCache(l2LineSize, l2Ways, l2Sets, 2*memLatency)
//    l2Cache.setCriticality(0, 500)
//    l2Cache.setCriticality(1, 500)
//    val l2OnDone  = (coreId:Int) => {
//      l2Cache.unassign(coreId)
//      None
//    };

//    var l2Cache = new ContentionPartCache(l2LineSize, l2Ways, l2Sets, memLatency)
//    l2Cache.setCriticality(0, 500)
//    l2Cache.setCriticality(1, 500)
//    for(i <- 0 until l2Ways/2) {
//      l2Cache.assignWay(0,i)
//      l2Cache.assignWay(1,(l2Ways/2)+i)
//    }
//    val l2OnDone  = (coreId:Int) => {
//      l2Cache.unassign(coreId)
//      None
//    };

//    var l2Cache = new TimeoutCache(l2LineSize, l2Ways, l2Sets, 1000)
//    for(i <- 0 until l2Ways/2) {
//      l2Cache.setPriority(0,i)
//      l2Cache.setPriority(1,(l2Ways/2)+i)
//    }
//    val l2OnDone  = (coreId:Int) => {
//      l2Cache.removePriority(coreId)
//      None
//    };

    val newResultFile = (name:String) => {
      new BufferedWriter(new FileWriter(new File(s"$path/$name")))
    }

    val accessTimesFile = newResultFile("access_times.csv")
    val coreStatsFile = newResultFile("core_stats.csv")
    val globalStatsFile = newResultFile("global_stats.csv")

    try {
      accessTimesFile.write("coreNr,clockFinish,latency\n")
      coreStatsFile.write("coreNr,accessCount,l1Hits,l1WriteBacks,l2Accesses,l2Hits\n")

      runConfig(
        "0",
        () => {new LruCache(l1LineSize, l1Ways, l1Sets)},
        l2Cache,
        l2OnDone,
        l1Latency,
        l2Latency,
        memLatency,
        accessTimesFile,
        coreStatsFile,
        globalStatsFile
      )

    } finally {
      accessTimesFile.close()
      coreStatsFile.close()
      globalStatsFile.close()
    }
  }

}
