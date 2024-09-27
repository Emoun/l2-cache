package caches

import scala.collection.mutable

abstract class SoftCache(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int) {

  /**
   * The total capacity (in bytes) of the cache.
   */
  val capacity: Int = lineLength*ways*sets;

  /**
   *
   * @param addr
   * @return The index of the set that should cache the given address
   */
  def setIdxForAddr(addr: Int): Int = {
    (addr / lineLength)%sets
  }

  /**
   *
   * @param addr
   * @return The lowest address in the cache line containing the given address
   */
  def lineHeadForAddr(addr: Int): Int = {
    (addr/lineLength) * lineLength
  }

  /**
   * Takes a cache line requests and returns how many cycles later the line
   * will be bursted
   *
   * @return how many cycles later the response begins
   */
  def getCacheLine(addr: Int, core: Int): Int
}

abstract class TrackingLruCache[T](lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int) extends
  SoftCache(lineLength, ways, sets, shortLatency, longLatency)
{
  /**
   * Each set contains some ways.
   * Each way tracks the address its caching and how many times is has not been used.
   * The highest "use" is the least recently used
   */
  var _setArr: Array[Array[Option[(Int, Int, T)]]] = {
    Array.fill(sets){Array.fill(ways){None}}
  };

  def setForAddr(addr: Int): Array[Option[(Int, Int, T)]] =
  {
    _setArr(setIdxForAddr(addr))
  }

  def increaseUse(set: Array[Option[(Int, Int, T)]]) = {
    for(i <- 0 until set.length) {
      val way = set(i);
      if(way.isDefined) set(i) = Some((way.get._1, way.get._2+1, way.get._3));
    }
  }

  /**
   * Returns a sorted list of indices into the given array where the 'None's are first,
   * then the elements with the highest use count (second element in tuple).
   * @param set
   * @return
   */
  def getLeastRecentryUseOrder(set: Array[Option[(Int, Int, T)]]): Array[Int] = {
    set.zipWithIndex.sortBy {
      case (None, _) => (0, 0)
      case (Some((_, useCount, _)), _) => (1, -useCount)
    }.map(_._2)
  }

  /**
   * Returns the index of the way with the highest use count (or the first None)
   * @param set
   * @return
   */
  def getLeastRecentlyUse(set: Array[Option[(Int, Int, T)]]): Int = {
    getLeastRecentryUseOrder(set)(0)
  }

  def printAll(): Unit = {
    printf("{\n");
    for(set <- _setArr) {
      printf("\tSet {\n");
      for(way <- set) {
        printf("\t\tWay{ ");
        if(way.isDefined) {
          printf("%d, %d, ", way.get._1, way.get._2);
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
  def isHit(addr: Int): Option[(Int, Int)] = {
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
  def onMiss(coreId: Int, setIdx: Int): Option[(Int, T)]

  override def getCacheLine(addr: Int, core: Int): Int =
  {
    val set = setForAddr(addr);
    // Increase use of all ways, such that none of them have a use count of 0
    // which will be the new use count of the address requested
    increaseUse(set);
    val headAddr = lineHeadForAddr(addr);

    isHit(addr) match {
      // Hit
      case Some((setIdx, wayIdx)) => {
        // Reset this way's use
        _setArr(setIdx)(wayIdx) = Some((_setArr(setIdx)(wayIdx).get._1, 0, _setArr(setIdx)(wayIdx).get._3));
        onHit(core, setIdx, wayIdx);
        shortLatency
      }
      // Miss
      case None => {
        onMiss(core, setIdxForAddr(addr)) match {
          case Some((wayIdx, payload)) => {
            set(wayIdx) = Some((headAddr, 0, payload));
          }
          case None => ;
        }
        longLatency
      }
    }
  }
}

class LruCache(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int) extends
  TrackingLruCache[Unit](lineLength, ways, sets, shortLatency, longLatency)
{
  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int): Unit = {}

  override def onMiss(coreId: Int, setIdx: Int): Option[(Int, Unit)] = {
    Some((getLeastRecentlyUse(_setArr(setIdx)), Unit))
  }
}

class ContentionCache(lineLength: Int, ways: Int, sets: Int, shortLatency: Int, longLatency: Int)
  // T is the ID of the core that loaded the line
  extends TrackingLruCache[Int](lineLength, ways, sets, shortLatency, longLatency)
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
    _contention(coreId) -= longLatency - shortLatency;
  }

  override def onHit(coreId: Int, setIdx: Int, wayIdx: Int): Unit = {
    if(_contention.contains(coreId)) {
      if(_setArr(setIdx)(wayIdx).get._3 != coreId ) {
        // The core got a free hit, so increase its contention limit to match
        _contention(coreId) += longLatency-shortLatency;
      }
    }
  }

  override def onMiss(coreId: Int, setIdx: Int): Option[(Int, Int)] = {
    val set = _setArr(setIdx);

    val lruList = getLeastRecentryUseOrder(set);

    if(set(lruList(0)).isEmpty) {
      // Found empty way, fill it
      return Some((lruList(0), coreId));
    }

    // No empty ways

    // First, try to find a way that is not occupied by a high-criticality core
    val nonCrit = lruList.find(wayIdx => {
      set(wayIdx) match {
        case Some((_,_,cId)) => !_contention.contains(cId);
        case None => false
      }
    });

    // Find lines occupied by non-limited
    val notLimited = lruList.find(wayIdx => {
      set(wayIdx) match {
        case Some((_,_,cId)) => !_contention.contains(cId) || (_contention(cId) != 0);
        case None => false
      }
    });

    if(_contention.contains(coreId)) {
      // Request is critical
      if(notLimited.isDefined) {
        val evictedCoreId = set(notLimited.get).get._3;
        if(_contention.contains(evictedCoreId) && nonCrit.isDefined){
          // Evicting high-criticality line instead of low-criticality line.
          // This results in contention
          triggerContention(evictedCoreId);
        }
        Some((notLimited.get, coreId))
      } else {
        // All ways contain limited lines, just take the LRU
        Some((lruList(0), set(lruList(0)).get._3))
      }
    } else {
      // Request is not critical, get first non-critical LRU
      if(nonCrit.isDefined) {
        Some((nonCrit.get, coreId))
      } else {
        if(notLimited.isDefined) {
          // Found a core that has not reached the limit

          triggerContention(notLimited.get);

          // Allow evicting it
          Some((notLimited.get, coreId))
        } else {
          // No ways are from cores that have not reached the limit.
          // No caching can be done
          None
        }
      }
    }
  }

  def setCriticality(coreId: Int, contentionLimit: Int): Unit = {
    _contention(coreId) = contentionLimit;
  }
}
