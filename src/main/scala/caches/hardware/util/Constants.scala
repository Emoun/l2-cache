package caches.hardware.util

object Constants {
  val ADDRESS_WIDTH = 32

  val CORE_REQUEST_ID_WIDTH = 16

  val BYTES_PER_SUB_BLOCK_L2 = 16

  val BYTES_PER_BLOCK_L2 = 64

  val CONTENTION_LIMIT_WIDTH = 11 // If the maximum limit is 100k and assuming a single L2 cache miss latency is 60 cc, then we need only 11 bits to represent the maximum contention limit count

  val TIMEOUT_LIMIT_WIDTH = 10 // Assuming one sets timers are ticked down each cycle, and default size includes 512 sets, each timer is decremented every 512 cycles, giving a max timer at 500k using 10 bits

}
