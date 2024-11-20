package caches

import scala.collection.mutable

sealed trait CacheResponse{
  def isReadHit(): Boolean = {
    this == ReadHit
  }
  def isReadMiss(): Boolean = {
    this == ReadMiss
  }
  def isWriteBack(): Boolean = {
    this match{
      case WriteBack(_) => true;
      case _ => false;
    }
  }
}
// The cache access is already in the cache and can be services immediately.
case object ReadHit extends CacheResponse
// The cache access must load a cache line before it can be serviced.
case object ReadMiss extends CacheResponse
// The cache access must evict a cache line, then a cache line must be loaded, before the access is serviced.
// The given address is for the evicted cache line.
case class WriteBack(addr:Long) extends CacheResponse

abstract class SoftCache(l: Int, w: Int, s: Int)  {

  val lineLength: Int = l;
  val ways: Int = w;
  val sets: Int = s;

  /**
   * The total capacity (in bytes) of the cache.
   */
  val capacity: Int = lineLength*ways*sets;

  /**
   *
   * @param addr
   * @return The index of the set that should cache the given address
   */
  def setIdxForAddr(addr: Long): Int = {
    ((addr / lineLength)%sets).toInt
  }

  /**
   *
   * @param addr
   * @return The lowest address in the cache line containing the given address
   */
  def lineHeadForAddr(addr: Long): Long = {
    (addr/lineLength) * lineLength
  }

  /**
   * Performs an access
   * @param addr Address of the access
   * @param core The core that performs the access
   * @param isRead Whether the access is a read (if not, it's a write)
   * @param withRefill Whether any changes should be done to the cache from the access.
   *                   If false, no cache lines are evicted or LRU counts updated.
   *                   I.e., becomes just a check for what the response would be, had this been true.
   * @return The response of the cache.
   */
  def performAccess(addr: Long, core: Int, isRead:Boolean,withRefill:Boolean): CacheResponse;

  def advanceCycle() = {}

  /**
   * Returns if the given address is cached and which set and way it is in.
   * @param addr
   * @return
   */
  def isHit(addr: Long): Option[(Int, Int)];

  def printAll();

  /**
   * Evict the cache line containing this address, wherever it may reside.
   * @param addr
   */
  def evict(addr: Long): Unit;
}

abstract class TrackingCache[T](l: Int, w: Int, s: Int) extends
  SoftCache(l, w, s)
{
  /**
   * Each set contains some ways.
   * Each way tracks the address its caching, some payload, and whether the cache line is dirty (has been written to)
   */
  var _setArr: Array[Array[Option[(Long, T, Boolean)]]] = {
    Array.fill(sets){Array.fill(ways){None}}
  };

  def setForAddr(addr: Long): Array[Option[(Long, T, Boolean)]] =
  {
    _setArr(setIdxForAddr(addr))
  }

  override def printAll(): Unit = {
    printf("{\n");
    for(set <- _setArr) {
      printf("\tSet {\n");
      for(way <- set) {
        printf("\t\tWay{ ");
        if(way.isDefined) {
          printf("%d, ", way.get._1);
          print(way.get._2);
          print(", ")
          print(way.get._3);
        } else {
          printf("None")
        };
        printf(" }\n");
      }
      printf("\t}\n");
    }
    printf("}\n");
  }

  /**
   * Returns the set and way index the cache line containing the given address (or none)
   * @param addr
   * @return
   */
  override def isHit(addr: Long): Option[(Int, Int)] = {
    val set = setForAddr(addr);
    val headAddr = lineHeadForAddr(addr);
    for(i <- 0 until set.length) {
      var way = set(i);
      if(way.map(_._1).contains(headAddr)) {
        // Address is cached
        return Some((setIdxForAddr(addr), i));
      }
    }
    None
  }

  /**
   * An action that will be performed upon a hit (after the reset of the way's use count)
   * @param setIdx index of set that hit
   * @param wayIdx index of way that hit
   */
  def onHit(coreId: Int, setIdx: Int, wayIdx: Int,withRefill:Boolean);

  /**
   * An action that should be performed upon a miss, returning the way index that should
   * be replaced and the payload that should be tracked (if any).
   * @return
   */
  def onMiss(coreId: Int, setIdx: Int,withRefill:Boolean): Option[(Int, T)];

  override def performAccess(addr: Long, core: Int, isRead:Boolean,withRefill:Boolean): CacheResponse = {
    val set = setForAddr(addr);
    val headAddr = lineHeadForAddr(addr);

    isHit(addr) match {
      // Hit
      case Some((setIdx, wayIdx)) => {
        onHit(core, setIdx, wayIdx, withRefill);
        if(withRefill && !isRead) {
          set(wayIdx) = Some((set(wayIdx).get._1, set(wayIdx).get._2, true))
        };
        ReadHit
      }
      // Miss
      case None => {
        onMiss(core, setIdxForAddr(addr), withRefill) match {
          case Some((wayIdx, payload)) => {
            val result = if(set(wayIdx).isDefined && set(wayIdx).get._3) {
              val evictAddr = set(wayIdx).get._1
              WriteBack(evictAddr)
            } else {
              ReadMiss
            };
            if(withRefill) {
              var isDirty = !isRead
              if(set(wayIdx).isDefined) {
                isDirty = isDirty || set(wayIdx).get._3;
              }
              set(wayIdx) = Some((headAddr, payload, isDirty))
            };
            result
          }
          case None => ReadMiss;
        }
      }
    }
  }

  def evict(addr: Long): Unit = {
    var set = setForAddr(addr)
    val found = set.zipWithIndex.find(entry => {
      entry._1.isDefined && entry._1.get._1 == addr
    })

    if(found.isEmpty) {
      assert(false)
    }

    set(found.get._2) = None
  }
}

trait ReplacementPolicy[T]  {self: TrackingCache[T] =>

  def sortValidWays(coreId: Int, setIdx: Int, toSort: Array[Int]): Array[Int];

  /**
   * Get valid list of ways in the given set that the given core can evict
   * @param coreId
   * @param setIdx
   * @return
   */
  def getValidWays(coreId: Int, setIdx: Int): Array[Int];

  def defaultPayload(coreId: Int, setIdx: Int, wayIdx: Int):T;

  override def onMiss(coreId: Int, setIdx: Int, withRefill: Boolean): Option[(Int, T)] = {
    val valids = getValidWays(coreId, setIdx);

    if( valids.length > 0 ) {
      val wayIdx = sortValidWays(coreId, setIdx, valids)(0)
      Some((wayIdx, defaultPayload(coreId, setIdx, wayIdx)))
    } else {
      None
    }
  }

}

trait LRUReplacement[T] extends ReplacementPolicy[(Int, T)] {self: TrackingCache[(Int, T)] =>
  def increaseUse(setIdx: Int) = {
    var set = _setArr(setIdx)
    for(i <- 0 until ways) {
      val way = set(i);
      if(way.isDefined) {
        val (address, (useCount, payload), isDirty) = way.get;
        _setArr(setIdx)(i) = Some((address, (useCount+1, payload), isDirty))
      }
    }
  }

  def resetUse(setIdx: Int, wayIdx: Int) = {
    val line = _setArr(setIdx)(wayIdx);
    if(line.isDefined) {
      val (address, (_, payload), isDirty) = line.get;
      _setArr(setIdx)(wayIdx) = Some((address, (0, payload), isDirty));
    }
  }

  def sortValidWays(coreId: Int, setIdx: Int, toSort: Array[Int]): Array[Int] = {
    toSort.sortBy( wayIdx => {
      getUseCount(setIdx, wayIdx) match {
        case None => (0,0)
        case Some(count) => (1, -count)
      }
    })
  }

  def getUseCount(setIdx: Int, wayIdx: Int): Option[Int] = {
    _setArr(setIdx)(wayIdx).map{
      case (_, (count, _),_) => count
    }
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int, withRefill: Boolean) = {
    increaseUse(setIdx);
    if(withRefill) resetUse(setIdx, wayIdx);
  }

  override def onMiss(coreId: Int, setIdx: Int, withRefill: Boolean): Option[(Int, (Int, T))] = {
    increaseUse(setIdx);
    super.onMiss(coreId, setIdx,withRefill)
  }
}

trait PartitionedReplacement[T] extends ReplacementPolicy[T] {self: TrackingCache[T] =>

  var _partitions: Array[Option[Int]] = Array.fill(ways){None};

  /**
   * Assign the given core to the given way
   * @param coreId
   * @param wayIdx
   */
  def assignWay(coreId: Int, wayIdx: Int): Unit = {
    _partitions(wayIdx) = Some(coreId);
  }

  /**
   * Unassign the given core from any ways
   * @param coreId
   */
  def unassignCore(coreId: Int): Unit = {
    for(i <- 0 until ways) {
      if(_partitions(i).isDefined && _partitions(i).get == coreId) {
        _partitions(i) = None;
      }
    }
  }

  /**
   * Returns the ways, if any, that are assigned to the given core
   */
  def getPartition(coreId: Int): Array[Int] = {
    _partitions.zipWithIndex.filter(entry => {
      if(entry._1.isDefined){
        entry._1.contains(coreId)
      } else {
        false
      }
    }).map(entry => entry._2)
  }

  def getUnpartitioned(): Array[Int] = {
    _partitions.zipWithIndex.filter(entry => {
      entry._1.isEmpty
    }).map(entry => entry._2)
  }

  override def getValidWays(coreId: Int, setIdx: Int): Array[Int] = {

    if(applyPartitioning(coreId)) {
      _partitions.zipWithIndex.filter(entry => {
        if(entry._1.isDefined){
          entry._1.contains(coreId)
        } else {
          true
        }
      }).map(entry => entry._2)
    } else {
      Array.range(0, ways)
    }
  }

  /**
   * Returns whether the given core should be bound by partitioning. If not, it may evict anything.
   * @param coreId
   * @return
   */
  def applyPartitioning(coreId: Int): Boolean;

}

class LruCache(lineLength: Int, ways: Int, sets: Int) extends
  TrackingCache[(Int,Unit)](lineLength, ways, sets) with LRUReplacement[Unit]
{
  override def getValidWays(coreId: Int, setIdx: Int): Array[Int] = {
    Array.range(0, ways)
  }

  override def defaultPayload(coreId: Int, setIdx: Int, wayIdx: Int): (Int, Unit) = {
    (0, Unit)

  }
}

class PartitionedCache(lineLength: Int, ways: Int, sets: Int) extends
  TrackingCache[(Int,Unit)](lineLength, ways, sets) with LRUReplacement[Unit] with PartitionedReplacement[(Int,Unit)]
{
  override def defaultPayload(coreId: Int, setIdx: Int, wayIdx: Int): (Int, Unit) = {
    (0, Unit)
  }

  override def applyPartitioning(coreId: Int): Boolean = {
    true
  }
}

class ContentionCache(lineLength: Int, ways: Int, sets: Int, contentionCost: Int)
// T is the ID of the core that loaded the line
  extends TrackingCache[(Int,Int)](lineLength, ways, sets) with LRUReplacement[Int]
{

  /**
   * Tracks contention for each core.
   * Tracks the available amount of contention
   * When is reaches zero, no more contention is allowed
   */
  private var _contention: mutable.Map[Int, Int] = mutable.Map.empty;

  // Reduces the contention count for the given core (assuming it is high-criticality)
  def triggerContention(coreId: Int): Unit = {
    assert(_contention.contains(coreId));
    _contention(coreId) -= contentionCost;
  }

  def setCriticality(coreId: Int, contentionLimit: Int): Unit = {
    _contention(coreId) = contentionLimit;
  }

  def unassign(coreId: Int): Unit = {
    _contention.remove(coreId)
  }

  override def defaultPayload(coreId: Int, setIdx: Int, wayIdx: Int): (Int, Int) = {
    (0, coreId)
  }

  override def getValidWays(coreId: Int, setIdx: Int): Array[Int] = {
    val notLimited = Array.range(0, ways).filter(wayIdx => {
      _setArr(setIdx)(wayIdx) match {
        case Some((_, (_, cId),_)) => !_contention.contains(cId) || (_contention(cId) >=contentionCost);
        case None => true
      }
    });

    if (notLimited.length > 0) {
      // There are some that are not limited, choose from them
      notLimited
    } else {
      // All lines are occupied by limited cores

      if (_contention.contains(coreId)) {
        // Request is also from critical core, so choose any
        Array.range(0, ways)
      } else {
        // Request is not critical, no eviction is allowed
        Array.empty
      }
    }
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int, withRefill:Boolean): Unit = {
    super.onHit(coreId, setIdx, wayIdx, withRefill);
    if(withRefill && _contention.contains(coreId)) {
      val line = _setArr(setIdx)(wayIdx).get;
      val hitOwnerId = line._2._2;
      if(hitOwnerId != coreId && !_contention.contains(hitOwnerId) ) {
        // The core got a free hit, so increase its contention limit to match
        _contention(coreId) += contentionCost;
        // Assign the line to the core
        _setArr(setIdx)(wayIdx) = Some(line._1, (line._2._1, coreId), line._3);
      }
    }
  }

  override def onMiss(coreId: Int, setIdx: Int, withRefill:Boolean): Option[(Int, (Int, Int))] = {
    val miss = super.onMiss(coreId, setIdx, withRefill);

    if(withRefill && miss.isDefined) {
      val (evictedWayIdx, _) = miss.get;

      val evictedWay = _setArr(setIdx)(evictedWayIdx);
      if(evictedWay.isDefined) {
        val evictedCoreId = evictedWay.get._2._2;

        if(_contention.contains(evictedCoreId)) {
          // Evicted a critical core, check contention

          // If another way contains a non-critical line
          val existOtherNonCritWays = Array.range(0, ways).find( wayIdx =>{
            _setArr(setIdx)(wayIdx) match {
              case Some((_,(_,c),_)) => !_contention.contains(c);
              case None => false;
            }
          });

          if(!_contention.contains(coreId) || existOtherNonCritWays.isDefined) {
            assert(_contention(evictedCoreId) > 0);
            triggerContention(evictedCoreId);
          }
        }
      }
    }
    miss
  }

  override def printAll() {
    super.printAll()
    print("{ ")
    for( entry <- _contention) {
      printf("(%d: %d) ", entry._1, entry._2)
    }
    println("}")
  }
}

class TimeoutCache(lineLength: Int, ways: Int, sets: Int, timeout: Int)
// T is the ID of the core that loaded the line
  extends TrackingCache[(Int,Int)](lineLength, ways, sets) with LRUReplacement[Int]
{
  // For each way, element contains the core with the priority to this line.
  private var _priorities: Array[Option[Int]] = Array.fill(ways){None};

  /**
   * Returns whether the given way in the given set has non-zero timer.
   * @param setIdx
   * @param wayIdx
   * @return
   */
  private def hasPriority(setIdx: Int, wayIdx: Int): Boolean = {
    val line = _setArr(setIdx)(wayIdx)
    line.isDefined && line.get._2._2 != 0
  }

  override def advanceCycle() = {
    for(setIdx <- 0 until sets) {
      for(wayIdx <- 0 until ways) {
        val line = _setArr(setIdx)(wayIdx);
        if(line.isDefined && line.get._2._2 > 0) {
          _setArr(setIdx)(wayIdx) = Some((line.get._1, (line.get._2._1, line.get._2._2-1), line.get._3))
        }
      }
    }
  }

  def setPriority(coreId: Int, wayIdx: Int) = {
    assert(wayIdx < ways);
    _priorities(wayIdx) = Some(coreId);
  }

  def removePriority(coreId: Int) = {
    for(i <- 0 until ways) {
      if(_priorities(i).isDefined && _priorities(i).get == coreId) {
        _priorities(i) = None;
      }
    }
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int, withRefill:Boolean): Unit = {
    super.onHit(coreId, setIdx, wayIdx, withRefill)
    // Refresh timer if prioritized
    if(withRefill && _priorities(wayIdx).contains(coreId)){
      val line = _setArr(setIdx)(wayIdx).get;
      _setArr(setIdx)(wayIdx) = Some((line._1, (line._2._1, timeout), line._3));
    }
  }

  override def getValidWays(coreId: Int, setIdx: Int): Array[Int] = {
    Array.range(0, ways).filter((wayIdx) => {
      val line = _setArr(setIdx)(wayIdx);
      line.isEmpty ||
        // Choose from any ways that have an expired timer
        !hasPriority(setIdx, wayIdx) ||
        // Choose from any ways that have the given core as priority
        _priorities(wayIdx).contains(coreId)
    })
  }

  override def defaultPayload(coreId: Int, setIdx: Int, wayIdx: Int): (Int, Int) = {
    if(_priorities(wayIdx).contains(coreId)) {
      // Core has priority in this way, so add timeout
      (0,timeout)
    } else {
      // Core does not have priority, no timeout
      (0,0)
    }

  }
}

class ContentionPartCache(lineLength: Int, ways: Int, sets: Int, contentionCost: Int)
// T is the LRU counter and ID of the core that loaded the line
  extends TrackingCache[(Int,Int)](lineLength, ways, sets) with LRUReplacement[Int] with PartitionedReplacement[(Int,Int)]
{

  /**
   * Tracks contention for each core.
   * Tracks the available amount of contention
   * When is reaches zero, no more contention is allowed
   */
  private var _contention: mutable.Map[Int, Int] = mutable.Map.empty;

  // Reduces the contention count for the given core (assuming it is high-criticality)
  def triggerContention(coreId: Int): Unit = {
    assert(_contention.contains(coreId));
    assert(_contention(coreId) >= contentionCost);
    _contention(coreId) -= contentionCost;
  }

  def setCriticality(coreId: Int, contentionLimit: Int): Unit = {
    _contention(coreId) = contentionLimit;
  }

  def unassign(coreId: Int): Unit = {
    _contention.remove(coreId)
    super.unassignCore(coreId)
  }

  override def defaultPayload(coreId: Int, setIdx: Int, wayIdx: Int): (Int, Int) = {
    (0, coreId)
  }

  override def getValidWays(coreId: Int, setIdx: Int): Array[Int] = {
    val notLimited = super.getValidWays(coreId, setIdx).filter(wayIdx => {
      _setArr(setIdx)(wayIdx) match {
        case Some((_, (_, cId),_)) => !_contention.contains(cId) || (_contention(cId) >= contentionCost);
        case None => true
      }
    });

    if (notLimited.length > 0) {
      // There are some that are not limited, choose from them
      notLimited
    } else {
      // All lines are occupied by limited cores
      if (_contention.contains(coreId)) {
        // Request is also from critical core, so choose any
        super.getValidWays(coreId, setIdx)
      } else {
        // Request is not critical, no eviction is allowed
        Array.empty
      }
    }
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int, withRefill:Boolean): Unit = {
    super.onHit(coreId, setIdx, wayIdx, withRefill);
    if(withRefill && _contention.contains(coreId)) {
      val line = _setArr(setIdx)(wayIdx).get;
      val hitOwnerId = line._2._2;
      if(hitOwnerId != coreId && !_contention.contains(hitOwnerId) ) {
        // The core got a free hit, so increase its contention limit to match
        _contention(coreId) += contentionCost;
        // Assign the line to the core
        _setArr(setIdx)(wayIdx) = Some(line._1, (line._2._1, coreId),line._3);
      }
    }
  }

  def triggerContentionOrSuggestOther(
     coreId: Int, setIdx: Int, eviction: (Int, (Int, Int)), blockList: Array[Int], withRefill:Boolean
  ): Option[(Int, (Int, Int))] =
  {
    val (evictedWayIdx, (lruCount, ownerCoreId)) = eviction;

    val evictedWay = _setArr(setIdx)(evictedWayIdx);
    if(evictedWay.isDefined) {
      val evictedCoreId = evictedWay.get._2._2;

      if(_contention.contains(evictedCoreId)) {
        // Evicted a critical core, check contention

        // Trigger if critical is evicted by non-critical
        val evictedByNonCrit = !_contention.contains(coreId);

        // If another way in the partition (or non-partitioned) contains a non-critical line

        val nonCritWaysInPart = sortValidWays(0, setIdx,
          (getPartition(evictedCoreId).filter(wayIdx => {
            _setArr(setIdx)(wayIdx) match {
              case Some((_,(_,c),_)) => c != evictedCoreId;
              case None => false;
            }
          }) ++ getUnpartitioned())
          .filter(wayIdx => !blockList.contains(wayIdx))
          .filter(wayIdx => evictedWayIdx != wayIdx)
        )

        val limited = !(_contention(evictedCoreId) >= contentionCost)

        if(!limited) {
          if(evictedByNonCrit || nonCritWaysInPart.length>0) {
            if(withRefill) triggerContention(evictedCoreId);
          }
          return Some(eviction)
        } else {
          if(nonCritWaysInPart.length>0) {
            val newBlockList = blockList :+ evictedWayIdx;
            return triggerContentionOrSuggestOther(coreId, setIdx, (nonCritWaysInPart(0),(lruCount, ownerCoreId)), newBlockList, withRefill)
          } else if(evictedByNonCrit) {
            return None
          } else {
            return Some(eviction)
          }
        }
      }
    }
    Some(eviction)
  }

  override def onMiss(coreId: Int, setIdx: Int, withRefill:Boolean): Option[(Int, (Int, Int))] = {
    val miss = super.onMiss(coreId, setIdx, withRefill);

    if(miss.isDefined) {
      return triggerContentionOrSuggestOther(coreId, setIdx, miss.get, Array.empty, withRefill)
    }
    miss
  }

  override def printAll() {
    super.printAll()
    print("{ ")
    for( entry <- _contention) {
      printf("(%d: %d) ", entry._1, entry._2)
    }
    println("}")
  }

  override def applyPartitioning(coreId: Int): Boolean = {
    _contention.contains(coreId) || getPartition(coreId).length > 0
  }

  override def sortValidWays(coreId: Int, setIdx: Int, toSort: Array[Int]): Array[Int] = {
    val firstSort = super.sortValidWays(coreId, setIdx, toSort)

    val coreIsCritical = _contention.contains(coreId) && _contention(coreId)<contentionCost
    firstSort.sortBy(wayIdx => {
      val lineNonCrit = _setArr(setIdx)(wayIdx).isEmpty || !_contention.contains(_setArr(setIdx)(wayIdx).get._2._2);
      if(coreIsCritical && lineNonCrit) {
        0 // Prefer non-critical lines to evict
      } else {
        1
      }
    })
  }
}

class MainMemory(lineSize:Int) extends SoftCache(lineSize,1,1) {
  override def isHit(addr: Long): Option[(Int, Int)] = { None }
  override def printAll(): Unit = {}
  override def performAccess(addr: Long, core: Int, isRead: Boolean, withRefill: Boolean): CacheResponse = ReadHit;
  override def evict(addr: Long): Unit = {}
}

/**
 * Defines various types of cache accesses. E.g. normal hits and misses, but also others.
 */
sealed trait HitType{
  def isHit(): Boolean = {
    this == Hit
  }
  def isMiss(): Boolean = {
    this == Miss
  }
  def isHitAfterMiss(): Boolean = {
    this == HitAfterMiss
  }
}
object HitType {
  def fromBool(isHit: Boolean): HitType = {
    if(isHit) Hit else Miss
  }
}
// A cache miss
case object Miss extends HitType
// A cache hit
case object Hit extends HitType
// Initially it missed, but before the external was prompted for the address, it was loaded by another request
case object HitAfterMiss extends HitType

class CacheTraffic(
  burst: Int,
  traf: Traffic[Int],
  cache: SoftCache,

  /**
* For each access, takes the core requesting it and whether it was a hit or miss
*/
  reportHits: (Int, HitType) => Unit,
) extends Traffic[Unit] {

  override def burstSize: Int = burst
  require(burstSize >= traf.burstSize)

  /**
   * Whether we are waiting for being served by external
   * None means not waiting
   * First is the address needed from external
   * The second is request state
   * Third is the request token
   * Fourth is whether the request to extern is a read
   * Fifth is whether the request from traf is a read
   */
  var busy: Option[(Long, RequestState, Int, Boolean, Boolean)] = None
  /**
   * In case we are writing back, this stores the address of the original write that should be loded af the write-back
   */
  var writeAddr: Option[Long] = None

  def waitingToIssue(): Boolean = {
    busy.isDefined && busy.get._2 == Waiting
  }
  def waitingForExternal(): Boolean = {
    busy.isDefined && busy.get._2 == Issued
  }
  def waitingForInternal(): Boolean = {
    busy match {
      case Some((_, Servicing(0),_,true,_)) => true
      case _ => false
    }
  }
  def waitingForWrite(): Boolean = {
    busy match {
      case Some((_, Servicing(0),_,false,_)) => {
        assert(writeAddr.isDefined)
        true
      }
      case _ => false
    }
  }

  def checkServe(): Unit = {
    if(waitingForInternal() && traf.serveMemoryAccess(busy.get._3)) {
      val response = cache.performAccess(busy.get._1, busy.get._3, busy.get._5,true)
      assert(!response.isWriteBack())
      busy = None
    }
    if(waitingForWrite()) {
      // On a write, we must also reload the cache line
      cache.evict(busy.get._1)
      busy = Some((writeAddr.get,  Waiting, busy.get._3, true, busy.get._5))
      writeAddr = None
    }
  }

  def checkTraf():Unit = {
    if(busy.isEmpty ) {
      traf.requestMemoryAccess() match {
        case Some((addr, isRead, coreId)) => {
          val isHit = cache.performAccess(addr, coreId,isRead,false) match {
            case ReadHit => {
              // Cache hit, service next cycle
              busy = Some((addr, Servicing(1), coreId, true, isRead))
              true
            }
            case ReadMiss => {
              // Cache read miss
              busy = Some((addr, Waiting, coreId, true, isRead))
              false
            }
            case WriteBack(writeBackAddr) => {
              // Cache write back
              busy = Some((writeBackAddr, Waiting, coreId, false, isRead))
              writeAddr = Some(addr)
              false
            }
          }
          reportHits(coreId, HitType.fromBool(isHit))
        }
        case _ =>
      }
    }
  }

  override def serveMemoryAccess(token: Unit): Boolean = {
    assert(!waitingToIssue())
    if(waitingForInternal()) {
      false
    } else if(waitingForExternal()) {
      busy = Some(busy.get._1, Servicing(0), busy.get._3, busy.get._4, busy.get._5)
      true
    } else {
      assert(false) // Unreachable
      false
    }
  }

  override def requestMemoryAccess(): Option[(Long,Boolean,Unit)] = {
    checkServe()
    checkTraf()

    if(waitingToIssue()) {
      busy = Some((busy.get._1, Issued, busy.get._3, busy.get._4, busy.get._5))
      return Some((busy.get._1, busy.get._4, ()))
    }

    None
  }

  override def triggerCycle(): Unit = {
    checkServe()
    checkTraf()

    busy match {
      case Some((addr, Servicing(l),token, isExtRead, isIntRead)) if l>0 => {
        busy = Some((addr, Servicing(l-1), token, isExtRead, isIntRead))
      }
      case _ =>
    }

    traf.triggerCycle()
    cache.advanceCycle()
  }

  override def isDone(): Boolean = {
    traf.isDone()
  }
}

class BufferedCacheTraffic(
  burst: Int,
  traf: Traffic[Int],
  cache: SoftCache,

  /**
   * For each access, takes the core requesting it and whether it was a hit or miss
   */
  reportHits: (Int, HitType) => Unit,
) extends Traffic[Unit] {

  override def burstSize: Int = burst

  /**
   * Must be the next to be sent to internal. Contains the request token
   */
  var interPrio: Option[Int] = None

  /**
   * Whether we have served itern this cycle
   */
  var internServed = false;
  /**
   * Whether we have accepted a requenst from internal this cycles
   */
  var internRequested = false;

  /**
   * Ordered queue of cache misses that are waiting for external.
   * First is the address requested, second is the request token.
   * Third is whether it's an extern read and if so, whether it is then an intern read too
   * (on extern writes, there is no intern response, as a read will come after)
   */
  var externQueue: Array[(Long,Int,Option[Boolean])] = Array.empty
  /**
   * The status of the head of the extern queue.
   * If false, has yet to issue the request to extern
   * If true, waiting for extern to service the requenst
   */
  var externStatus: RequestState = Waiting

  def checkServe(): Unit = {
    if(!internServed && interPrio.isDefined) {
      if(traf.serveMemoryAccess(interPrio.get)) {
        interPrio = None
        internServed = true
      }
    }

    if(interPrio.isEmpty && externStatus == Servicing(0)) {
      assert(externQueue.length>0)

      if(externQueue(0)._3.isDefined){
        // Update cache
        cache.performAccess(externQueue(0)._1, externQueue(0)._2,externQueue(0)._3.get,true)
        // Only schedule reads for core servicing
        interPrio = Some(externQueue(0)._2)
      } else {
        // Evict cache line
        cache.evict(externQueue(0)._1)
        // Writes don't service the core because the following read will
      }

      externQueue = externQueue.drop(1)
      externStatus = Waiting
      checkServe() // Potentially serve immediately
    } else if(interPrio.isEmpty && externStatus == Waiting && externQueue.length>0) {
      if(externQueueHeadHits()) {
        // This miss is now a hit
        interPrio = Some(externQueue(0)._2)
        reportHits(externQueue(0)._2, HitAfterMiss)
        externQueue = externQueue.drop(1)
        checkServe() // Potentially serve immediately
      }
    }
  }

  def checkReq(): Unit = {
    if(!internRequested && interPrio.isEmpty) {
      traf.requestMemoryAccess() match {
        case Some((addr, isRead, coreId)) => {
          internRequested = true

          val wasHit = cache.performAccess(addr, coreId,isRead,false) match {
            case ReadHit => {
              interPrio = Some(coreId)
              true
            }
            case _ => {
              externQueue = externQueue :+ (addr, coreId, Some(isRead))
              false
            }
          }
          reportHits(coreId, if(wasHit) Hit else Miss)
        }
        case None =>
      }
    }
  }

  def externQueueHeadHits(): Boolean = {
    externQueue.length>0 && externQueue(0)._3.isDefined &&
      cache.performAccess(externQueue(0)._1, externQueue(0)._2,true,false).isReadHit()
  }

  override def serveMemoryAccess(token: Unit): Boolean = {
    assert(externStatus == Issued)
    assert(externQueue.length > 0)

    externStatus = Servicing(0)

    true
  }

  override def requestMemoryAccess(): Option[(Long, Boolean, Unit)] = {
    checkServe()
    checkReq()

    if(externStatus == Waiting && externQueue.length>0 && !externQueueHeadHits()) {

      externStatus = Issued
      cache.performAccess(
          externQueue(0)._1, externQueue(0)._2, externQueue(0)._3.isDefined, false
      ) match {
        case WriteBack(writeAddr) => {
          // This external read has not evicted yet, evict first
          externQueue = Array((writeAddr, externQueue(0)._2, None)) ++ externQueue
          Some((writeAddr, false, ()))
        }
        case ReadMiss => {
          Some((externQueue(0)._1, true, ()))
        }
        case ReadHit => {
          assert(false)
          None
        };
      }

    } else {
      None
    }
  }
  override def triggerCycle(): Unit = {
    checkServe()
    checkReq()

    traf.triggerCycle()
    cache.advanceCycle()

    internServed = false
    internRequested = false
  }
  override def isDone(): Boolean = {
    traf.isDone()
  }
}