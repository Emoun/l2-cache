package caches.hardware.util

object Constants {
  val ADDRESS_WIDTH = 32

  val CORE_REQUEST_ID_WIDTH = 16

  val BYTES_PER_WORD_L1 = 4

  val BYTES_PER_WORD_L2 = 16

  val BYTES_PER_BLOCK_L1 = 16

  val BYTES_PER_BLOCK_L2 = 64

  // TODO: Compute this in the contention cache instead, if we know the cache miss latency and the maximum contention limit we can compute the maximum contention limit width
  val CONTENTION_LIMIT_WIDTH = 11 // If the maximum limit is 100k and assuming a single L2 cache miss latency is 60 cc, then we need only 11 bits to represent the maximum contention limit count

  val CONTENTION_LIMIT = 0
}
