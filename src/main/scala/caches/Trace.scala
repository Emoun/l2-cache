package caches

import caches.sim._
import caches.sim.Sim.emptyDirectory

import java.io.{BufferedWriter, File, FileWriter, OutputStreamWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.matching.Regex

/**
 * Tracks all the allocated memory blocks in the trace.
 * Each block has a name (key), its starting address, and size.
 * @param blocks
 */
class MemBlocks(blocks: HashMap[String, (Long, Int)], absolutes: HashSet[String])  {
  val memBlocks = blocks
  val absoluteBlocks = absolutes

  /**
   * Returns the effective address of the access from the given memory block with the given offset.
   * @param block
   * @param offset
   * @return
   */
  def getAddr(graphId: Int, block: String, offset: Long): Long = {

    if(absoluteBlocks.contains(block)) {
      offset
    } else {
      val mem = if(memBlocks.contains(block)) {
        memBlocks.get(block).get
      } else {
        assert(memBlocks.contains(MemBlocks.getExtendedName(graphId, block)), s"Did not find block for graph $graphId: $block")
        memBlocks.get(MemBlocks.getExtendedName(graphId, block)).get
      }

      assert(mem._2 > offset, s"Request outside block. Block: $block, Size: ${mem._2}, offset: $offset")

      mem._1 + offset
    }
  }

  def isPrivate(graphId: Int, addr: Long): Boolean = {
    for(block <- memBlocks) {
      if(block._2._1 <= addr && addr < (block._2._1+block._2._2)) {
        // Found block
        return block._1.startsWith(s"Graph_${graphId}")
      }
    }
    // Address is not in any block, meaning its absolute, which we treat as shared
    false
  }
}

object MemBlocks {
  def getExtendedName(graphId: Int, block: String): String = {
    s"Graph_${graphId}_$block"
  }
}

class TaskGraph(s: Array[Int], e: Array[Int], deps: HashMap[Int, Array[Int]]) {
  val dependencies: HashMap[Int, Array[Int]] = deps

  val starts: Array[Int] = s
  val ends: Array[Int] = e

  def revDeps(job: Int): Array[Int] = {
    var revs: Array[Int] = Array.empty
    for (key <- dependencies.keys) {
      if (dependencies(key).contains(job)) {
        revs = revs :+ key
      }
    }
    revs
  }

  def getDeps(job: Int): Array[Int] = {
    if(dependencies.contains(job)) {
      dependencies(job)
    } else {
      Array.empty
    }
  }

  def getAllJobs(): HashSet[Int] = {
    var jobs = HashSet.empty[Int]

    for(key <- dependencies.keys ++ starts ++ ends) {
      jobs += key
    }

    jobs
  }

  def prettyPrint(): Unit = {
    println(s"Starts: ${starts.mkString(", ")}")
    println("Dependencies {")
    for ((key, array) <- dependencies) {
      val values = array.mkString(", ")
      println(s"  $key -> [ $values ]")
    }
    println("}")
    println(s"Ends: ${ends.mkString(", ")}")
  }
}

class TaskGraphRun(graph: TaskGraph) {
  var taskGraph = graph
  var executing: HashSet[Int] = HashSet.empty
  var done: HashSet[Int] = HashSet.empty

  def next(): Option[Int]  = {
    val start_job = graph.starts.find(job => !done.union(executing).contains(job))
    if(start_job.isDefined) {
      executing += start_job.get
      Some(start_job.get)
    } else {

      for (job <- done) {
        // For each finished job, look dependent jobs that have all their dependents done
        val ready = graph.getDeps(job).find(dep => {
          !done.union(executing).contains(dep) && !graph.revDeps(dep).exists(rev => !done.contains(rev))
        })

        if(ready.isDefined) {
          executing += ready.get
          return Some(ready.get)
        }
      }

      None
    }
  }

  def jobFinished(job: Int): Unit = {
    assert(executing.contains(job))
    done += job
    executing -= job
  }

  def isDone(): Boolean = {
    !graph.ends.exists(job => !done.contains(job))
  }
}

class CoreRunner (graphRun: TaskGraphRun, accesses: HashMap[Int, Array[Array[MemAccess]]]) {

  var taskRun = graphRun
  var currentJob: Option[Int] = None
  var waitingJobAccesses: Array[Array[MemAccess]] = Array.empty

  /**
   * Retruns the accesses of the next instance of the current job
   * @return
   */
  def nextInstance(): Option[Array[MemAccess]] = {
    if(waitingJobAccesses.length > 0) {
      val head = waitingJobAccesses.head
      waitingJobAccesses = waitingJobAccesses.tail
      Some(head)
    } else {
      if(currentJob.isDefined) {
        graphRun.jobFinished(currentJob.get)
      }
      currentJob = None
      None
    }
  }

  /**
   * Initiates the next job, assuming no more instances must be executed.
   */
  def nextJob() = {
    assert(nextInstance().isEmpty)
    currentJob = graphRun.next()
    if(currentJob.isDefined) {
      waitingJobAccesses = accesses(currentJob.get)
    }
  }

  def isDone(): Boolean = {
    currentJob.isEmpty && graphRun.isDone()
  }



}

object Trace2Sim {
  val traceBaseDir = "2024-12-24 Trace/"
  val memAlocFileName = "tempAllocList.csv"
  val traceFilesDir = traceBaseDir + "DTraceSplitCSV/"


  /**
   * Loads the allocated memory blocks from the given source.
   * Skips the first line of the source, assuming it to be the header of the csv file.
   * @param source
   * @return
   */
  def loadMemBlocks(source: Source, graphIds: Array[Int]): MemBlocks = {
    val MemBlockPattern: Regex = """(\w+),(\d+),(\d+),(\d+),(\d+),(\w+)""".r
    /// Map name to (start_address, size)
    var memBlocks = HashMap[String, (Long, Int)]();
    var absoluteBlocks: HashSet[String] = HashSet.empty
    var nextAddress: Long = 0;

    for (line  <- source.getLines().drop(1)) {
      line match {
        case MemBlockPattern(name, totalSize, count, maxOffset, minOffset, alloc_type) =>
          assert(memBlocks.get(name).isEmpty && !absoluteBlocks.contains(name), s"Memory allocation duplicate: $name")

          val size = totalSize.toInt.max((maxOffset.toInt) + 128) // Add buffer to maxOffset to ensure no accesses is larger than size

          if(alloc_type != "AbsoluteAddr") {

            val blockNames: Array[String] = alloc_type match {
              case "StaticMem" => {
                Array(name)
              }
              case "Stack" | "DynamicMem" | "TempBuffer"  =>{
                graphIds.map( id => MemBlocks.getExtendedName(id, name))
              }
              case x => {
                assert(false, s"Unknown memory type: $x")
                Array.empty
              }
            }

            for(block <- blockNames) {
              val alignment: Int = 128
              var alignedSize = size
              alignedSize += (alignment - (alignedSize % alignment))
              assert((alignedSize % alignment) == 0) // Ensure size is aligned on 128 byte boundary

              memBlocks(block) = (nextAddress, size)

              nextAddress += alignedSize
            }
          } else {
            absoluteBlocks += name
          }
        case _ => {
          println(s"Unknown memory allocation: '$line'")
          assert(false)
        }
      }
    }
    new MemBlocks(memBlocks, absoluteBlocks)
  }

  def loadAccesses(source: Source, memBlocks: MemBlocks, graphIds: Array[Int]): HashMap[Int, Array[MemAccess]] = {
    val accessPattern: Regex = """(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\w+),(\d+),(\d+),(\d+),(R|W|NA),(\w+),(\d+),(\d+),(\d+),(R|W|NA)""".r
    val accessPattern2: Regex = """(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\w+),(\w+),(\d+),(\d+),(\d+),(R|W|NA),(\w+),(\d+),(\d+),(\d+),(R|W|NA)""".r
    val accessPattern3: Regex = """(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\w+),(\d+),(\d+),(\d+),(R|W|NA),(\w+),(\w+),(\d+),(\d+),(\d+),(R|W|NA)""".r
    val accessPattern4: Regex = """(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\d+),(\w+),(\w+),(\d+),(\d+),(\d+),(R|W|NA),(\w+),(\w+),(\d+),(\d+),(\d+),(R|W|NA)""".r

    var accesses: HashMap[Int, Array[MemAccess]]= HashMap.empty
    for(id <- graphIds) {
      accesses(id) = Array.empty
    }


    var ignoreStart: Option[Long] = None

    val knownHeaders = Array(
      "cycle_bus,cycle_start,cycle_end,latency,kernelId,TTiNo,jobIdx,inst_num,data_num,name_space_0,total_size_0,offset_0,Len(Byte)_0,RW_0,name_space_1,total_size_1,offset_1,Len(Byte)_1,RW_1",
      "cycle_bus,cycle_start,cycle_end,latency,kernelId,TTiNo,jobIdx,inst_num,data_num,name_space_0,mem_level_0,total_size_0,offset_0,Len(Byte)_0,RW_0,name_space_1,total_size_1,offset_1,Len(Byte)_1,RW_1",
      "cycle_bus,cycle_start,cycle_end,latency,kernelId,TTiNo,jobIdx,inst_num,data_num,name_space_0,total_size_0,offset_0,Len(Byte)_0,RW_0,name_space_1,mem_level_1,total_size_1,offset_1,Len(Byte)_1,RW_1",
      "cycle_bus,cycle_start,cycle_end,latency,kernelId,TTiNo,jobIdx,inst_num,data_num,name_space_0,mem_level_0,total_size_0,offset_0,Len(Byte)_0,RW_0,name_space_1,mem_level_1,total_size_1,offset_1,Len(Byte)_1,RW_1"
    )
    var lines = source.getLines()
    val header = lines.next()
    assert(knownHeaders.exists(h => h == header), s"Unknown trace header: $header")

    val pattern1  = header == knownHeaders(0)
    val pattern2  = header == knownHeaders(1)
    val pattern3  = header == knownHeaders(2)
    val pattern4  = header == knownHeaders(3)

    def parseLine(line: String, cycle_bus: String, cycle_start: String, data_num: String, name_space_0: String, offset_0: String, len_0: String, rw_0: String, name_space_1: String, offset_1: String, len_1: String, rw_1: String): Unit = {
      if (ignoreStart.isEmpty) {
        ignoreStart = Some(cycle_bus.toLong)
      } else if (cycle_bus.toLong == ignoreStart.get) {
        // Remove any previous accesses
        for (id <- graphIds) {
          accesses(id) = Array.empty
        }
      } else {
        // Proper accesses were found, continue normally
      }

      if (data_num.toInt > 0) {
        assert(name_space_0 != "NA" || name_space_1 != "NA", s"Did not find expected memory access in instruction: $line")
        var data_count = 0
        if (name_space_0 != "NA") {
          assert(rw_0 != "NA")
          data_count += 1

          for (id <- graphIds) {
            accesses(id) = accesses(id) :+ new MemAccess(
              (math.log(len_0.toInt) / math.log(2)).toInt,
              rw_0 == "R",
              memBlocks.getAddr(id, name_space_0, offset_0.toInt),
              cycle_start.toLong
            )
          }
        }
        if (name_space_1 != "NA") {
          assert(rw_1 != "NA")
          data_count += 1
          for (id <- graphIds) {
            accesses(id) = accesses(id) :+ new MemAccess(
              (math.log(len_1.toInt) / math.log(2)).toInt,
              rw_1 == "R",
              memBlocks.getAddr(id, name_space_1, offset_1.toInt),
              cycle_start.toLong
            )
          }
        }
        assert(data_count == data_num.toInt)
      }
    }

    for (line  <- lines) {
      line match {
        case accessPattern(
          cycle_bus,cycle_start,cycle_end,latency,kernelId,tTiNo,jobIdx,inst_num,data_num,
          name_space_0,total_size_0,offset_0,len_0,rw_0,
          name_space_1,total_size_1,offset_1,len_1,rw_1
        ) if pattern1 => parseLine(line, cycle_bus, cycle_start, data_num, name_space_0, offset_0, len_0, rw_0, name_space_1, offset_1, len_1, rw_1)
        case accessPattern2(
          cycle_bus,cycle_start,cycle_end,latency,kernelId,tTiNo,jobIdx,inst_num,data_num,
          name_space_0,mem_level_0, total_size_0,offset_0,len_0,rw_0,
        name_space_1,total_size_1,offset_1,len_1,rw_1
        ) if pattern2 => parseLine(line, cycle_bus, cycle_start, data_num, name_space_0, offset_0, len_0, rw_0, name_space_1, offset_1, len_1, rw_1)
        case accessPattern3(
          cycle_bus,cycle_start,cycle_end,latency,kernelId,tTiNo,jobIdx,inst_num,data_num,
          name_space_0,total_size_0,offset_0,len_0,rw_0,
          name_space_1,mem_level_1,total_size_1,offset_1,len_1,rw_1
        ) if pattern3 => parseLine(line, cycle_bus, cycle_start, data_num, name_space_0, offset_0, len_0, rw_0, name_space_1, offset_1, len_1, rw_1)
        case accessPattern4(
          cycle_bus,cycle_start,cycle_end,latency,kernelId,tTiNo,jobIdx,inst_num,data_num,
          name_space_0,mem_level_0,total_size_0,offset_0,len_0,rw_0,
          name_space_1,mem_level_1,total_size_1,offset_1,len_1,rw_1
        ) if pattern4 => parseLine(line, cycle_bus, cycle_start, data_num, name_space_0, offset_0, len_0, rw_0, name_space_1, offset_1, len_1, rw_1)
        case _ => {
          println(s"Unknown memory access: '$line'")
          assert(false)
        }
      }
    }

    accesses
  }

  def parseIntList(list: String): Array[Int] = {
    list.split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt)
  }

  def loadTaskGraph(source: Source): TaskGraph = {

    var lines = source.getLines()

    var start = lines.next().split(":").map(_.trim)

    assert(start.length == 2, "Start jobs list wrong")
    assert(start(0) == "start")

    var deps: HashMap[Int, Array[Int]] = HashMap.empty

    for(line <- lines) {
      val parts = line.split(":").map(_.trim)
      assert(parts.length == 2)

      if(parts(0) == "end") {
        return new TaskGraph(parseIntList(start(1)), parseIntList(parts(1)), deps)
      } else {
        val job = parts(0).toInt
        assert(!deps.contains(job))

        deps(job) = parseIntList(parts(1))
      }
    }
    assert(false, "Did not find end")

    new TaskGraph(Array.empty,Array.empty,HashMap.empty)
  }

  def getJobTraceFiles(dirPath: String, prefix: String, jobId: Int): Array[Source] = {
    val dir = new File(dirPath)

    if (dir.exists && dir.isDirectory) {
      dir.listFiles
        .filter(file => file.isFile && file.getName.startsWith(prefix + jobId))
        .map(file => Source.fromFile(file))
    } else {
      Array.empty
    }
  }

  def initializeRunners(traceMapping: HashMap[String, HashSet[Int]], memAllocs: Source, taskGraph: Source): (Array[CoreRunner], MemBlocks) = {
    var allGraphIds = HashSet.empty[Int]
    for(ids <- traceMapping) {
      assert(allGraphIds.intersect(ids._2).size == 0, "Duplicate graph ID")
      allGraphIds ++= ids._2
    }

    val memBlocks = loadMemBlocks(memAllocs, allGraphIds.toArray)

    val deps = loadTaskGraph(taskGraph)

    val jobAccesses:  HashMap[Int, HashMap[Int, Array[Array[MemAccess]]]] = HashMap.empty

    for(mapping <- traceMapping) {
      for (job <- deps.getAllJobs()) {
        val traceAcceses = getJobTraceFiles(traceFilesDir, mapping._1, job).map(trace => loadAccesses(trace, memBlocks, mapping._2.toArray))

        for (trace <- traceAcceses) {
          for (gId <- trace.keys) {
            if (!jobAccesses.contains(gId)) {
              jobAccesses(gId) = HashMap.empty
            }
            if (!jobAccesses(gId).contains(job)) {
              jobAccesses(gId)(job) = Array.empty
            }

            jobAccesses(gId)(job) = jobAccesses(gId)(job) :+ trace(gId)
          }
        }
      }
    }

    (allGraphIds.toArray.map(id => new CoreRunner(new TaskGraphRun(deps), jobAccesses(id))), memBlocks)
  }

  def runSim(
              simID: String,
              traceMapping: HashMap[String, HashSet[Int]],
              newL1Cache: () => SoftCache,
              l2CacheGen: (()=>Long, String) => (SoftCache, (Int) => Unit),
              l1Latency: Int,
              l2Latency: Int,
              memLatency: Int,
              l1BurstSize: Int,
              withWriteBack: Boolean,
              accessTimesFile: BufferedWriter,
              jobFinishTimesFile: BufferedWriter,
              coreStatsFile: BufferedWriter,
              globalStatsFile: BufferedWriter,
              log: (String) => Unit,
            ): Unit = {
    println("Running Simulation '" + simID + "'")

    val (runners, memBlocks) = initializeRunners(
      traceMapping,
      Source.fromFile(traceBaseDir + memAlocFileName),
      Source.fromFile(traceBaseDir + "task_graph.csv")
    )

    var clock: Long = 0;
    val cores = runners.length
    var coreAccesses: Array[Int] = Array.fill(cores){0}
    var l2Accesses: Array[Int] = Array.fill(cores){0}
    var cumulativeLatencies: Array[Int] = Array.fill(cores){0}
    var hits: Array[Int] = Array.fill(cores){0}
    var l2Hits: Array[Int] = Array.fill(cores){0}
    var l2HitsAfterMiss: Array[Int] = Array.fill(cores){0}
    var l2MissDuringMiss: Array[Int] = Array.fill(cores){0}
    var l2MissDuringMissLimited: Array[Int] = Array.fill(cores){0}
    var l2QueueMiss: Array[Int] = Array.fill(cores){0}
    var mainMemAccesses: Int = 0
    var maxCores = 8


    val newL1RunnerTraffic = (runnerIdx: Int) => {
      val lastJob = runners(runnerIdx).currentJob

      val nextInst = runners(runnerIdx).nextInstance()

      val inst = if(nextInst.isDefined) {
        nextInst
      } else {
        if(lastJob.isDefined){
          jobFinishTimesFile.write(s"$simID,$runnerIdx,${lastJob.get},$clock\n")
          log(s"Core $runnerIdx finished job ${lastJob.get}\n")
        }

        runners(runnerIdx).nextJob()

        if(runners(runnerIdx).currentJob.isDefined) {
          log(s"Core $runnerIdx starting job ${runners(runnerIdx).currentJob.get}\n")
          runners(runnerIdx).nextInstance()
        } else {
          None
        }
      }

      inst.map(i => {
        new TraceTraffic(l1BurstSize, i.iterator, latency => {
          cumulativeLatencies(runnerIdx) += latency
          coreAccesses(runnerIdx) += 1
          log(f"($runnerIdx, ${runners(runnerIdx).currentJob.get}) $clock: {${runnerIdx}, $latency}\n")
          accessTimesFile.write(f"$simID,${runnerIdx},${runners(runnerIdx).currentJob.get},${runners(runnerIdx).waitingJobAccesses.length},$clock,$latency\n")
        })
      })
    }
    val l2BurstSize = newL1Cache().lineLength
    var l1CacheTraffic = runners.zipWithIndex.map(pathIdx => {
      new CacheTraffic(
        l2BurstSize,
        new RoundRobinArbiter(
          l1BurstSize,
          l1Latency,
          Array(newL1RunnerTraffic(pathIdx._2).get),
          (_) => {
            newL1RunnerTraffic(pathIdx._2)
          }
        ),
        newL1Cache(),
        (_, hitType) => {
          if(hitType.isHit()) hits(pathIdx._2)+=1;
        },
        withWriteBack
      )
    })
    var l1CacheTrafficPassives: Array[CacheTraffic] = Array.empty
    // Make sure we always have the same number of cores connected to the L2 arbiter, to ensure latency is
    // the same even with inactive cores.
    while((l1CacheTraffic.length + l1CacheTrafficPassives.length) < maxCores) {
      l1CacheTrafficPassives +:= new CacheTraffic(
        l2BurstSize,
        new NoneTraffic[Int](l1BurstSize, () => {
          !l1CacheTraffic.exists(c => !c.isDone())
        }),
        newL1Cache(),
        (_, _) => {}
      )
    }

    val l2CacheGen2 = l2CacheGen(() => clock, simID)
    val l2Cache = l2CacheGen2._1
    val l2OnDone = l2CacheGen2._2
    val memBurstSize = l2Cache.lineLength

    var l2CacheTraffic = new BufferedCacheTraffic(
      memBurstSize,
      new RoundRobinArbiter(
        l2BurstSize,
        l2Latency,
        l1CacheTraffic ++ l1CacheTrafficPassives,
        (coreId:Int) => {
          l2OnDone(coreId)
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
          case MissDuringMiss => {
            l2MissDuringMiss(coreId)+=1
            l2Accesses(coreId) -= 1 //Already counted during the miss
          }
          case MissDuringMissLimited => {
            l2MissDuringMissLimited(coreId)+=1
            l2Accesses(coreId) -= 1 //Already counted during the miss
          }
          case QueueMiss => {
            l2QueueMiss(coreId)+=1
            l2Accesses(coreId) -= 1 //Already counted during the miss
          }
          case _ => ()
        }
      },
      withWriteBack
    )

    var MainMemTraffic = new CacheTraffic(
      memBurstSize,
      new RoundRobinArbiter(
        memBurstSize,
        memLatency,
        Array(l2CacheTraffic),
        (_) => {
          None
        }
      ),
      new MainMemory(memBurstSize),
      (_,isHit) => {
        assert(isHit.isHit())
        mainMemAccesses += 1
      }
    )

    while(!MainMemTraffic.isDone()) {
      val trig = MainMemTraffic.triggerCycle()
      clock+=1
    }


    for(i <- 0 until runners.length) {
      val l1Misses = coreAccesses(i) - hits(i)
      val l1WriteBack = l2Accesses(i) - l1Misses
      log(s"Count: ${coreAccesses(i)}, L1 Hits: ${hits(i)}, L1 Hit Pct: ${hits(i).toDouble/coreAccesses(i).toDouble}, " +
        s"L1 Write-Backs: $l1WriteBack, L2 Accesses: ${l2Accesses(i)}, L2 Hits: ${l2Hits(i)}(${l2HitsAfterMiss(i)}), " +
        s"L2 Hit Pct: ${l2Hits(i)/(l2Accesses(i)).toDouble}(${(l2Hits(i)+l2HitsAfterMiss(i))/(l2Accesses(i)).toDouble})," +
        s" Avg. Latency: ${(cumulativeLatencies(i).toDouble)/(coreAccesses(i).toDouble)}, Miss During Miss: ${l2MissDuringMiss(i)}\n")
      coreStatsFile.write(f"$simID,$i,${coreAccesses(i)},${hits(i)},${l1WriteBack},${l2Accesses(i)},${l2Hits(i)+l2HitsAfterMiss(i)},${l2MissDuringMiss(i)},${l2MissDuringMissLimited(i)},${l2QueueMiss(i)}\n")
    }
    val l2Misses = l2Accesses.sum - l2Hits.sum - l2HitsAfterMiss.sum
    globalStatsFile.write(f"$simID,${mainMemAccesses - l2Misses}\n")

  }

  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println(s"Expected result directory path as first argument.")
      return
    }
    val path = Paths.get(args(0))
    if (!Files.isDirectory(path)) {
      println(s"${args(0)} is not a directory.")
      return
    }
    emptyDirectory(path)
    val newResultFile = (name:String) => {
      new BufferedWriter(new FileWriter(new File(s"$path/$name")))
    }
    val simIdDescFile = newResultFile("sim_id_desc.csv")
    val accessTimesFile = newResultFile("access_times.csv")
    val jobFinishTimesFile = newResultFile("job_finish_times.csv")
    val coreStatsFile = newResultFile("core_stats.csv")
    val globalStatsFile = newResultFile("global_stats.csv")
    val contentionLimitFile = newResultFile("contention_limit_stats.csv")

    val logger = new BufferedWriter(new OutputStreamWriter(System.out))
    val log = (msg: String) => {
      // Comment the following to not log
//      logger.write(msg)
    }

    var configs: Array[(
      String, //simId
      HashMap[String, HashSet[Int]], // traces
      () => SoftCache, //L1Cache
      (()=>Long, String) => (SoftCache, (Int) => Unit), // L2Cache and L2CacheOnDone
      Int, //l1Latency
      Int, //l2Latency
      Int, //memLatency
      Int, //l1BurstSize
      Int, //l2BurstSize
      Int, //memBurstSize
      Boolean, // With write back
      String, // L1 cache name
      String, // L2 cache name
      String, //Misc info
      )] = Array.empty;

    ////////////////////////// Huawei settings /////////////////////////
    val l1Latency = 1
    val l2Latency = 15
    val memLatency = 60
    val l1BurstSize = 4
    val l2BurstSize = 64
    val memBurstSize = 64

    // 1KB L1 cache
    val l1LineSize=l2BurstSize
    val l1Ways = 4
    val l1Sets = 128

    // 8KB L2 cache
    val l2LineSize=memBurstSize
    val l2Ways = 8
    val l2Sets = 2048

    val l1LruDtuCache = () => new LruCache(l1LineSize,l1Ways,l1Sets)
    var l2ContCacheGen = (simId: String, limit:Int, sets: Int, clock: () => Long) => {
      val cache = new ContentionCache(l2LineSize, l2Ways, sets, memLatency,
        (coreId:Int) => {
          contentionLimitFile.write(f"$simId,$coreId,${clock()}\n")
        }
      )
      cache.setCriticality(0, limit)
      (cache, (coreId:Int) => {
        cache.unassign(coreId)
        contentionLimitFile.flush()
      })
    }

    var l2PartitionCacheGen = (simId: String, sets: Int, wayPart: Int) => {
      val cache = new PartitionedCache(l2LineSize, l2Ways, sets)
      for(i <- 0 until wayPart) {
        cache.assignWay(0, i)
      }
      (cache, (coreId:Int) => {
        cache.unassignCore(coreId)
      })
    }

    val defaultJobName = "Slot1_Cell0_Task0_Parallel0_Kernel"

    val cacheSizes = Array(
      (l2Sets, "Full", ""),
      (l2Sets/2, "Half", "(L2 half size)"),
      (l2Sets/4, "Quart", "(L2 quarter size)"),
    )

    for(writeBack <- Array(/*(true,"Write"),*/ (false,""))) {
      for (size <- cacheSizes) {
        configs +:= (
          "alone" + size._2 + writeBack._2,
          HashMap(defaultJobName -> HashSet[Int](0)),
          l1LruDtuCache,
          (clock: () => Long, simID: String) => l2ContCacheGen(simID, 0, size._1, clock),
          l1Latency,
          l2Latency,
          memLatency,
          l1BurstSize,
          l2BurstSize,
          memBurstSize,
          writeBack._1,
          "Lru",
          "Contention",
          "Single core alone " + size._3,
        );

        val limits = Array(
          (1000, "1k"),
          (10000, "10k"),
          (50000, "50k"),
          (100000, "100k"),
          (200000, "200k"),
          (500000, "500k"),
        )

        for (limit <- limits) {
          configs +:= (
            "mixed4Limit" + limit._2 + size._2 + writeBack._2,
            HashMap(defaultJobName -> HashSet[Int](0, 1, 2, 3)),
            l1LruDtuCache,
            (clock: () => Long, simID: String) => l2ContCacheGen(simID, limit._1, size._1, clock),
            l1Latency,
            l2Latency,
            memLatency,
            l1BurstSize,
            l2BurstSize,
            memBurstSize,
            writeBack._1,
            "Lru",
            "Contention",
            "4 cores, limit " + limit._2 + " " + size._3,
          );

          configs +:= (
            "mixed8Limit" + limit._2 + size._2 + writeBack._2,
            HashMap(defaultJobName -> HashSet[Int](0, 1, 2, 3, 4, 5, 6, 7)),
            l1LruDtuCache,
            (clock: () => Long, simID: String) => l2ContCacheGen(simID, limit._1, size._1, clock),
            l1Latency,
            l2Latency,
            memLatency,
            l1BurstSize,
            l2BurstSize,
            memBurstSize,
            writeBack._1,
            "Lru",
            "Contention",
            "8 cores, limit " + limit._2 + " " + size._3,
          );
        }

        val partitions = Array(
          (1, "1Way"),
          (2, "2Way"),
          (3, "3Way"),
          (4, "4Way"),
          (5, "5Way"),
          (6, "6Way"),
          (7, "7Way"),
          (8, "8Way"),
        )
        for (part <- partitions) {
          configs +:= (
            "mixed4Partition" + part._2 + size._2 + writeBack._2,
            HashMap(defaultJobName -> HashSet[Int](0, 1, 2, 3)),
            l1LruDtuCache,
            (clock: () => Long, simID: String) => l2PartitionCacheGen(simID, size._1, part._1),
            l1Latency,
            l2Latency,
            memLatency,
            l1BurstSize,
            l2BurstSize,
            memBurstSize,
            writeBack._1,
            "Lru",
            "Partitioned",
            "4 cores, Critical partition " + part._2 + " " + size._3,
          );

          configs +:= (
            "mixed8Partition" + part._2 + size._2 + writeBack._2,
            HashMap(defaultJobName -> HashSet[Int](0, 1, 2, 3, 4, 5, 6, 7)),
            l1LruDtuCache,
            (clock: () => Long, simID: String) => l2PartitionCacheGen(simID, size._1, part._1),
            l1Latency,
            l2Latency,
            memLatency,
            l1BurstSize,
            l2BurstSize,
            memBurstSize,
            writeBack._1,
            "Lru",
            "Partitioned",
            "8 cores, Critical partition " + part._2 + " " + size._3,
          );
        }
      }
    }



    try {
      accessTimesFile.write("simId,coreNr,jobId,instanceNr,clockFinish,latency\n")
      jobFinishTimesFile.write("simId,coreNr,jobId,instanceNr,clockFinish\n")
      coreStatsFile.write("simId,coreNr,accessCount,l1Hits,l1WriteBacks,l2Accesses,l2Hits,l2MissDuringMiss,l2MissDuringMissLimited,l2QueueMiss\n")
      globalStatsFile.write("simId,l2WriteBack\n")
      simIdDescFile.write("simId,l1Latency,l2Latency,mainMemLatency,l1BurstSize,l2BurstSize,mainMemBurstSize,l1CacheType,l2CacheType,misc\n")
      contentionLimitFile.write("simId,coreNr,limitClock\n")

      for(conf <- configs.reverse) {
        simIdDescFile.write(f"${conf._1},${conf._5},${conf._6},${conf._7},${conf._8},${conf._9},${conf._10},${conf._11},${conf._12},${conf._13},\n")
        runSim(
          conf._1,
          conf._2,
          conf._3,
          conf._4,
          conf._5,
          conf._6,
          conf._7,
          conf._8,
          conf._11,
          accessTimesFile, jobFinishTimesFile, coreStatsFile, globalStatsFile, log
        )
      }
    } finally {
      accessTimesFile.close()
      jobFinishTimesFile.close()
      coreStatsFile.close()
      globalStatsFile.close()
      simIdDescFile.close()
      contentionLimitFile.close()
      logger.close()
    }
  }
}