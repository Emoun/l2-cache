# L2 Cache for Real-Time Systems

This repo serves for the H. project. The plan is to simulate and implement
a L2 cache for time-critical (real-time) and high-performance tasks.
The real-time tasks are called *high priority* tasks here.

Initial idea: the tag memory has an additional field marking if the line
is owned by a high-priority task. In that case it cannot be evicted by
a low priority task. We will use n-way associativity to reserve some
ways where high-priority can allocate lines. To ensure that the other
tasks will not starve, some ways need to be allocated for low-priotity
tasks only.

