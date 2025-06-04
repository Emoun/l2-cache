package caches.hardware.pipelined.cache

import chisel3._
import chisel3.util._

// TODO: Mimic the AXI interface here
class ReadChannel(addrWidth: Int, burstWidth: Int) extends Bundle {
  val rAddr = new DecoupledIO(UInt(addrWidth.W))
  val rData = Flipped(new DecoupledIO(UInt(burstWidth.W)))
  val rLast = Input(Bool())
}

class WriteChannel(addrWidth: Int, burstWidth: Int) extends Bundle {
  val wAddr = new DecoupledIO(UInt(addrWidth.W))
  val wData = new DecoupledIO(UInt(burstWidth.W))
  val wLast = Output(Bool())
}

class MemoryControllerIO(addrWidth: Int, burstWidth: Int) extends Bundle {
  val rChannel = new ReadChannel(addrWidth, burstWidth)
  val wChannel = new WriteChannel(addrWidth, burstWidth)
}

class MemoryInterface(nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffsetWidth: Int, blockWidth: Int, subBlockWidth: Int, burstWidth: Int) extends Module {
  private val byteOffsetWidth = log2Up(subBlockWidth / 8)
  private val addressWidth = tagWidth + indexWidth + blockOffsetWidth + byteOffsetWidth
  private val nSubBlocks = blockWidth / subBlockWidth
  private val nBursts = blockWidth / burstWidth

  val io = IO(new Bundle {
    val missFifo = new Bundle {
      val popEntry = new MissFifoEntryIO(nWays, reqIdWidth, tagWidth, indexWidth, blockOffsetWidth, subBlockWidth)
      val empty = Input(Bool())
      val pop = Output(Bool())
    }
    val wbFifo = new Bundle {
      val popEntry = new WbFifoEntryIO(tagWidth, indexWidth, blockOffsetWidth, blockWidth)
      val empty = Input(Bool())
      val pop = Output(Bool())
    }
    val updateLogic = Flipped(new CacheUpdateEntryIO(nWays, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth))
    val memController = new MemoryControllerIO(addressWidth, burstWidth)
  })

  val sIdle :: sRead :: sWrite :: sReadBurst :: sWriteBurst :: sDoneFill :: sDoneWb :: Nil = Enum(7)
  val stateReg = RegInit(sIdle)

  // Registers
  val memRwDataReg = RegInit(VecInit(Seq.fill(nBursts)(0.U(burstWidth.W))))
  val burstCounter = RegInit(0.U(log2Up(nBursts).W))
  val reqRwReg = RegInit(false.B)
  val reqWDataReg = RegInit(0.U(subBlockWidth.W))
  val reqWayReg = RegInit(0.U(log2Up(nWays).W))
  val reqBlockOffsetReg = RegInit(0.U(blockOffsetWidth.W))
  val reqIndexReg = RegInit(0.U(indexWidth.W))
  val reqTagReg = RegInit(0.U(tagWidth.W))
  val reqIdReg = RegInit(0.U(reqIdWidth.W))

  // Default signal assignments
  val memRAddrValid = WireDefault(false.B)
  val memRDataReady = WireDefault(false.B)
  val memWAddrValid = WireDefault(false.B)
  val memWDataValid = WireDefault(false.B)
  val memWData = WireDefault(0.U(burstWidth.W))
  val memWLast = WireDefault(false.B)
  val wbFifoPop = WireDefault(false.B)
  val missFifoPop = WireDefault(false.B)
  val updateLogicValid = WireDefault(false.B)
  val cacheRData = WireDefault(0.U((blockWidth).W))
  val cacheRespStatus = WireDefault(0.U(1.W))
  val memRwDataRegAsUint = memRwDataReg.asUInt

  switch(stateReg) {
    is(sIdle) {

      when(!io.wbFifo.empty) {
        // Perform a write-back
        for (i <- 0 until nBursts) {
          memRwDataReg(i) := io.wbFifo.popEntry.wbData((i * burstWidth) + (burstWidth - 1), i * burstWidth)
        }

        reqRwReg := true.B
        reqWDataReg := 0.U
        reqWayReg := 0.U
        reqBlockOffsetReg := io.wbFifo.popEntry.blockOffset
        reqIndexReg := io.wbFifo.popEntry.index
        reqTagReg := io.wbFifo.popEntry.tag

        stateReg := sWrite
      }.elsewhen(!io.missFifo.empty) {
        // Perform a read
        reqRwReg := io.missFifo.popEntry.rw
        reqWDataReg := io.missFifo.popEntry.wData
        reqWayReg := io.missFifo.popEntry.replaceWay
        reqBlockOffsetReg := io.missFifo.popEntry.blockOffset
        reqIndexReg := io.missFifo.popEntry.index
        reqTagReg := io.missFifo.popEntry.tag
        reqIdReg := io.missFifo.popEntry.reqId

        stateReg := sRead
      }
    }

    is(sRead) {
      memRAddrValid := true.B
      when(io.memController.rChannel.rAddr.ready) {
        stateReg := sReadBurst
      }
    }

    is(sWrite) {
      memWAddrValid := true.B
      when(io.memController.wChannel.wAddr.ready) {
        stateReg := sReadBurst
      }
    }

    is(sReadBurst) {
      memRDataReady := true.B
      when(io.memController.rChannel.rData.valid) {
        burstCounter := burstCounter + 1.U

        memRwDataReg(burstCounter) := io.memController.rChannel.rData.bits

        when(io.memController.rChannel.rLast) {
          burstCounter := 0.U
          stateReg := sDoneFill
        }
      }
    }

    is(sWriteBurst) {
      memWDataValid := true.B
      when(io.memController.wChannel.wAddr.ready) {
        burstCounter := burstCounter + 1.U

        memWData := memRwDataReg(burstCounter)

        when(burstCounter === (nBursts - 1).U) {
          memWLast := true.B
          burstCounter := 0.U
          stateReg := sDoneWb
        }
      }
    }

    is(sDoneFill) {
      missFifoPop := true.B
      updateLogicValid := true.B
      stateReg := sIdle
    }

    is(sDoneWb) {
      wbFifoPop := true.B
      stateReg := sIdle
    }
  }

  io.wbFifo.pop := wbFifoPop
  io.missFifo.pop := missFifoPop

  io.updateLogic.valid := updateLogicValid
  io.updateLogic.rw := reqRwReg
  io.updateLogic.wData := reqWDataReg
  io.updateLogic.wWay := reqWayReg
  io.updateLogic.blockOffset := reqBlockOffsetReg
  io.updateLogic.index := reqIndexReg
  io.updateLogic.tag := reqTagReg
  io.updateLogic.reqId := reqIdReg

  val outAddr = Cat(reqTagReg, reqIndexReg, reqBlockOffsetReg) << byteOffsetWidth.U

  for (i <- 0 until nSubBlocks) {
    io.updateLogic.memReadData(i) := memRwDataRegAsUint((subBlockWidth - 1) + (i * subBlockWidth), i * subBlockWidth)
  }

  io.memController.rChannel.rAddr.valid := memRAddrValid
  io.memController.rChannel.rAddr.bits := outAddr
  io.memController.rChannel.rData.ready := memRDataReady
  io.memController.wChannel.wAddr.valid := memWAddrValid
  io.memController.wChannel.wAddr.bits := outAddr
  io.memController.wChannel.wData.valid := memWDataValid
  io.memController.wChannel.wData.bits := memWData
  io.memController.wChannel.wLast := memWLast
}
