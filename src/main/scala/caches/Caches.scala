package caches

import scala.collection.mutable

abstract class SoftCache(l: Int, w: Int, s: Int) {

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
   * Takes a cache line requests and returns if it is a hit or a miss
   *
   * @return Whether it is a cache hit
   */
  def getCacheLine(addr: Long, core: Int): Boolean

  def advanceCycle() = {}

  /**
   * Returns if the given address is cached and which set and way it is in.
   * @param addr
   * @return
   */
  def isHit(addr: Long): Option[(Int, Int)];

  def printAll();
}

abstract class TrackingCache[T](l: Int, w: Int, s: Int) extends
  SoftCache(l, w, s)
{
  /**
   * Each set contains some ways.
   * Each way tracks the address its caching and how many times is has not been used.
   * The highest "use" is the least recently used
   */
  var _setArr: Array[Array[Option[(Long, T)]]] = {
    Array.fill(sets){Array.fill(ways){None}}
  };

  def setForAddr(addr: Long): Array[Option[(Long, T)]] =
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
  def onHit(coreId: Int, setIdx: Int, wayIdx: Int);

  /**
   * An action that should be performed upon a miss, returning the way index that should
   * be replaced and the payload that should be tracked (if any).
   * @return
   */
  def onMiss(coreId: Int, setIdx: Int): Option[(Int, T)];

  override def getCacheLine(addr: Long, core: Int): Boolean =
  {
    val set = setForAddr(addr);
    val headAddr = lineHeadForAddr(addr);

    isHit(addr) match {
      // Hit
      case Some((setIdx, wayIdx)) => {
        onHit(core, setIdx, wayIdx);
        true
      }
      // Miss
      case None => {
        onMiss(core, setIdxForAddr(addr)) match {
          case Some((wayIdx, payload)) => {
            set(wayIdx) = Some((headAddr, payload));
          }
          case None => ;
        }
        false
      }
    }
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

  override def onMiss(coreId: Int, setIdx: Int): Option[(Int, T)] = {
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
        val (address, (useCount, payload)) = way.get;
        _setArr(setIdx)(i) = Some((address, (useCount+1, payload)))
      }
    }
  }

  def resetUse(setIdx: Int, wayIdx: Int) = {
    val line = _setArr(setIdx)(wayIdx);
    if(line.isDefined) {
      val (address, (_, payload)) = line.get;
      _setArr(setIdx)(wayIdx) = Some((address, (0, payload)));
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
      case (_, (count, _)) => count
    }
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int) = {
    increaseUse(setIdx);
    resetUse(setIdx, wayIdx);
  }

  override def onMiss(coreId: Int, setIdx: Int): Option[(Int, (Int, T))] = {
    increaseUse(setIdx);
    super.onMiss(coreId, setIdx)
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
        case Some((_, (_, cId))) => !_contention.contains(cId) || (_contention(cId) >=contentionCost);
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

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int): Unit = {
    super.onHit(coreId, setIdx, wayIdx);
    if(_contention.contains(coreId)) {
      val line = _setArr(setIdx)(wayIdx).get;
      val hitOwnerId = line._2._2;
      if(hitOwnerId != coreId && !_contention.contains(hitOwnerId) ) {
        // The core got a free hit, so increase its contention limit to match
        _contention(coreId) += contentionCost;
        // Assign the line to the core
        _setArr(setIdx)(wayIdx) = Some(line._1, (line._2._1, coreId));
      }
    }
  }

  override def onMiss(coreId: Int, setIdx: Int): Option[(Int, (Int, Int))] = {
    val miss = super.onMiss(coreId, setIdx);

    if(miss.isDefined) {
      val (evictedWayIdx, _) = miss.get;

      val evictedWay = _setArr(setIdx)(evictedWayIdx);
      if(evictedWay.isDefined) {
        val evictedCoreId = evictedWay.get._2._2;

        if(_contention.contains(evictedCoreId)) {
          // Evicted a critical core, check contention

          // Trigger if critical is evicted by non-critical
          val evictedByNonCrit = !_contention.contains(coreId);

          // If another way contains a non-critical line
          val existOtherNonCritWays = Array.range(0, ways).find( wayIdx =>{
            _setArr(setIdx)(wayIdx) match {
              case Some((_,(_,c))) => !_contention.contains(c);
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

  private def hasPriority(setIdx: Int, wayIdx: Int): Boolean = {
    val line = _setArr(setIdx)(wayIdx)
    line.isDefined && line.get._2._2 != 0
  }

  override def advanceCycle() = {
    for(setIdx <- 0 until sets) {
      for(wayIdx <- 0 until ways) {
        val line = _setArr(setIdx)(wayIdx);
        if(line.isDefined && line.get._2._2 > 0) {
          _setArr(setIdx)(wayIdx) = Some((line.get._1, (line.get._2._1, line.get._2._2-1)))
        }
      }
    }
  }

  def setPriority(coreId: Int, wayIdx: Int) = {
    assert(wayIdx < ways);
    _priorities(wayIdx) = Some(coreId);
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int): Unit = {
    super.onHit(coreId, setIdx, wayIdx)
    // Refresh timer if prioritized
    if(_priorities(wayIdx).contains(coreId)){
      val line = _setArr(setIdx)(wayIdx).get;
      _setArr(setIdx)(wayIdx) = Some((line._1, (line._2._1, timeout)));
    }
  }

  override def getValidWays(coreId: Int, setIdx: Int): Array[Int] = {
    val coreHasPrio = _priorities.zipWithIndex.filter((prio)=> {
      val (prioId, _) = prio;
      prioId.contains(coreId)
    });

    if(coreHasPrio.length>0) {
      // First, look for any priority ways that have expired
      val expired = coreHasPrio.filter((prio) => {
        val wayIdx = prio._2;
        val line = _setArr(setIdx)(wayIdx);
        line.isEmpty || line.get._2._2 == 0
      });

      if(expired.length>0) {
        // Evict from prioritized ways that have expired
        return expired.map(prio => prio._2 );
      } else {
        // No prioritized way has expired. Just use default behavior then
      }
    } else {
      // Core does not have any prioritized ways, use default behavior
    }

    // Choose from any ways that do not have a non-expired timeout
    Array.range(0, ways).filter((wayIdx) => {
      val line = _setArr(setIdx)(wayIdx);
      line.isEmpty || !hasPriority(setIdx, wayIdx)
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
// T is the ID of the core that loaded the line
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
    val notLimited = super.getValidWays(coreId, setIdx).filter(wayIdx => {
      _setArr(setIdx)(wayIdx) match {
        case Some((_, (_, cId))) => !_contention.contains(cId) || (_contention(cId) >= contentionCost);
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

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int): Unit = {
    super.onHit(coreId, setIdx, wayIdx);
    if(_contention.contains(coreId)) {
      val line = _setArr(setIdx)(wayIdx).get;
      val hitOwnerId = line._2._2;
      if(hitOwnerId != coreId && !_contention.contains(hitOwnerId) ) {
        // The core got a free hit, so increase its contention limit to match
        _contention(coreId) += contentionCost;
        // Assign the line to the core
        _setArr(setIdx)(wayIdx) = Some(line._1, (line._2._1, coreId));
      }
    }
  }

  override def onMiss(coreId: Int, setIdx: Int): Option[(Int, (Int, Int))] = {
    val miss = super.onMiss(coreId, setIdx);

    if(miss.isDefined) {
      val (evictedWayIdx, _) = miss.get;

      val evictedWay = _setArr(setIdx)(evictedWayIdx);
      if(evictedWay.isDefined) {
        val evictedCoreId = evictedWay.get._2._2;

        if(_contention.contains(evictedCoreId)) {
          // Evicted a critical core, check contention

          // Trigger if critical is evicted by non-critical
          val evictedByNonCrit = !_contention.contains(coreId);

          // If another way contains a non-critical line
          val existOtherNonCritWays = Array.range(0, ways).find( wayIdx =>{
            _setArr(setIdx)(wayIdx) match {
              case Some((_,(_,c))) => !_contention.contains(c);
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

  override def applyPartitioning(coreId: Int): Boolean = {
    _contention.contains(coreId)
  }
}