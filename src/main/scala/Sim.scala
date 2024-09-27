import caches.LruCache

// Can be run using the command: sbt "runMain Sim"
object Sim {
  def main(args: Array[String]): Unit = {
    println("Running Simulator")

    //val trace_path = args(0)
    var cache = new LruCache(2,2,2,14,130);
    cache.getCacheLine(0,0) // set 0, way 0
    cache.getCacheLine(2,0) // set 1, way 0
    cache.getCacheLine(4,0) // set 0, way 1
    cache.getCacheLine(6,0) // set 1, way 1

    // Set 0 is full, with '0' being least recently used
    println(cache.getCacheLine(8,0))
    println(cache.getCacheLine(5,0))

  }

}
