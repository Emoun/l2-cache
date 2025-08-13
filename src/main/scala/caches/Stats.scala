package caches

import caches.Trace2Sim.{initializeRunners, memAlocFileName, traceBaseDir}
import caches.sim.TraceTraffic

import java.io.PrintWriter
import scala.collection.mutable.{HashMap, HashSet}
import scala.io.Source

/**
 * Relevant statistics
 *  * Distance between accesses
 *  * Proportion of reads vs writes over time
 *  * Proportion of accessing shared vs private data
 *  * probability of reaccess (accessing an address again after the first time)
 *  * size of working set at each time point
 */
object Stats {


  def main(args: Array[String]): Unit = {
    val baseFileName = "accesses_"
    val defaultJobName = "Slot1_Cell0_Task0_Parallel0_Kernel"
    val (runners, memBlocks) = initializeRunners(
      HashMap(defaultJobName -> HashSet[Int](0,1,2,3)),
      Source.fromFile(traceBaseDir + memAlocFileName),
      Source.fromFile(traceBaseDir + "task_graph.csv")
    )

    val newL1RunnerTraffic = (runnerIdx: Int) => {
      val lastJob = runners(runnerIdx).currentJob

      val nextInst = runners(runnerIdx).nextInstance()

      val inst = if(nextInst.isDefined) {
        nextInst
      } else {
        if(lastJob.isDefined){
          println(s"Core $runnerIdx finished job ${lastJob.get}\n")
        }

        runners(runnerIdx).nextJob()

        if(runners(runnerIdx).currentJob.isDefined) {
          println(s"Core $runnerIdx starting job ${runners(runnerIdx).currentJob.get}\n")
          runners(runnerIdx).nextInstance()
        } else {
          None
        }
      }

      inst.map(i => {
        new TraceTraffic(4, i.iterator, _ => {})
      })
    }

    var cycle = 0
    var largestAddr: Long = 0
    var count = 1
    for(r <- 0 until 4) {
      val fileName = s"${baseFileName}${r}.txt"
      val writer = new PrintWriter(fileName)


      var runner = newL1RunnerTraffic(r)
      var openReq = false
      var reqDistance = 0
      while (runner.isDefined) {
        val run = runner.get
        while (!run.isDone()) {
          if (openReq) {
            run.serveMemoryAccess(r)
            openReq = false
          }
          val req = run.requestMemoryAccess()
          if (req.isDefined) {
            val (addr, isRead, _) = req.get

            println(s"($count, $cycle): Addr: $addr, isRead: $isRead, isPrivate: ${memBlocks.isPrivate(r, addr)}, distance: $reqDistance")
            var read_string = "w"
            if (isRead) {
              read_string = "r"
            }
            writer.write(s"$addr, $read_string, $reqDistance\n")

            openReq = true
            reqDistance = 0
            count += 1

            if (largestAddr < addr) {
              largestAddr = addr
            }
          }
          run.triggerCycle()
          cycle += 1
          if (!openReq) {
            reqDistance += 1
          }
        }
        runner = newL1RunnerTraffic(r)
      }
      writer.close()
    }
    println(s"Largest Address: $largestAddr")
  }


}
