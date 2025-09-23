package caches.hardware.pipelined.stages

import chisel3._
import chisel3.util._
import caches.hardware.util._
import caches.hardware.reppol.ReplacementPolicyIO
import caches.hardware.pipelined.{MshrInfoIO, MshrPushIO, RejectionQueueEntry}

class RepIO(nCores: Int, nWays: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, subBlockWidth: Int) extends Bundle() {
  val coreId = Input(UInt(log2Up(nCores).W))
  val reqValid = Input(Bool())
  val reqId = Input(UInt(reqIdWidth.W))
  val reqRw = Input(Bool())
  val wData = Input(UInt(subBlockWidth.W))
  val byteEn = Input(UInt((subBlockWidth / 8).W))
  val dirtyBits = Input(Vec(nWays, Bool()))
  val validBits = Input(Vec(nWays, Bool()))
  val setTags = Input(Vec(nWays, UInt(tagWidth.W)))
  val blockOffset = Input(UInt(blockOffWidth.W))
  val index = Input(UInt(indexWidth.W))
  val repPolReadIndex = Input(UInt(indexWidth.W))
  val tag = Input(UInt(tagWidth.W))
}

class InvalidateLineIO(nWays: Int, indexWidth: Int) extends Bundle {
  val invalidate = Output(Bool())
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(indexWidth.W))
}

class Rep(nCores: Int, nSets: Int, nWays: Int, nMshrs: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockOffWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module() {
  val io = IO(new Bundle {
    val addrWidth = tagWidth + indexWidth + blockOffWidth + log2Up(subBlockWidth / 8)
    val rep = new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
    val invalidate = new InvalidateLineIO(nWays, indexWidth)
    val read = Flipped(new ReadIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, blockWidth, subBlockWidth))
    val missFifoPush = Flipped(new MshrPushIO(nCores = nCores, nMshrs = nMshrs, nWays = nWays, reqIdWidth = reqIdWidth, tagWidth = tagWidth, indexWidth = indexWidth, blockOffsetWidth = blockOffWidth, subBlockWidth = subBlockWidth))
    val isMissPushCrit = Output(Bool())
    val missNonCritInfo = Flipped(new MshrInfoIO(nCores, nMshrs, nWays, indexWidth, tagWidth))
    val missCritInfo = Flipped(new MshrInfoIO(nCores, nMshrs, nWays, indexWidth, tagWidth))
    val repPol = Flipped(new ReplacementPolicyIO(nWays = nWays, nSets = nSets, nCores = nCores))
    val halfMissCapacity = Output(Bool())
    val dirtyInvalidStall = Output(Bool())
    val dirtyCtrl = Flipped(new DirtyCtrlIO(nWays = nWays, indexWidth = indexWidth))
    val stall = Input(Bool())
    val pushReject = Output(Bool())
    val pushRejectEntry = Flipped(new RejectionQueueEntry(nCores = nCores, addrWidth = addrWidth, dataWidth = subBlockWidth, reqIdWidth = reqIdWidth))
    val setLineValid = Flipped(new SetLineValidIO(nWays = nWays, indexWidth = indexWidth, tagWidth = tagWidth))
  })

  def halfMissCheck(currIndex: UInt, currTag: UInt, checkIdxs: Vec[UInt], checkTags: Vec[UInt], checkValids: Vec[Bool]): (Bool, UInt) = {
    val missMatches = Wire(Vec(nMshrs, Bool()))

    for (mshr <- 0 until nMshrs) {
      val mshrIndex = checkIdxs(mshr)
      val mshrTag = checkTags(mshr)
      val validMshr = checkValids(mshr)

      missMatches(mshr) := mshrIndex === currIndex && mshrTag === currTag && validMshr
    }

    (missMatches.reduce((x, y) => x || y), PriorityEncoder(missMatches))
  }

  def checkIfHit(validBits: Vec[Bool], tags: Vec[UInt], currTag: UInt): (Bool, UInt) = {
    // Compare tags and check if there is a hit and where
    val hits = Wire(Vec(nWays, Bool()))
    for (wayIdx <- 0 until nWays) {
      hits(wayIdx) := validBits(wayIdx) && (currTag === tags(wayIdx))
    }

    val hit = hits.reduce((x, y) => x || y)
    val hitWay = PriorityEncoder(hits)

    (hit, hitWay)
  }

  val invalidate = WireDefault(false.B)
  val invalidateWay = WireDefault(0.U(log2Up(nWays).W))
  val invalidateIndex = WireDefault(0.U(indexWidth.W))
  val updateStageIndex = WireDefault(0.U(indexWidth.W))
  val updateStageTag = WireDefault(0.U(tagWidth.W))
  val updateStageValid = WireDefault(false.B)
  val updateStageIsHit = WireDefault(false.B)
  val updateStageIsCrit = WireDefault(false.B)

  def updateValidAndDirtyBits(inValid: Vec[Bool], inDirty: Vec[Bool], inTags: Vec[UInt], currIdx: UInt, useInvalidate: Boolean = true): (Vec[Bool], Vec[Bool], Vec[UInt]) = {
    val newValidBits = VecInit(Seq.fill(nWays)(false.B))
    val newDirtyBits = VecInit(Seq.fill(nWays)(false.B))
    val newTags = VecInit(Seq.fill(nWays)(0.U(tagWidth.W)))
    newValidBits := inValid
    newDirtyBits := inDirty
    newTags := inTags

    if (useInvalidate) {
      when(invalidate && invalidateIndex === currIdx) {
        newValidBits(io.invalidate.way) := false.B
      }.elsewhen (io.setLineValid.refill && io.setLineValid.index === currIdx) {
        newValidBits(io.setLineValid.way) := true.B
        newTags(io.setLineValid.way) := io.setLineValid.tag
      }
    } else {
      when (io.setLineValid.refill && io.setLineValid.index === currIdx) {
        newValidBits(io.setLineValid.way) := true.B
        newTags(io.setLineValid.way) := io.setLineValid.tag
      }
    }

    when(io.dirtyCtrl.set && io.dirtyCtrl.wIndex === currIdx) {
      newDirtyBits(io.dirtyCtrl.wWay) := true.B
    }.elsewhen(io.dirtyCtrl.unset && io.dirtyCtrl.wIndex === currIdx) {
      newDirtyBits(io.dirtyCtrl.wWay) := false.B
    }

    (newValidBits, newDirtyBits, newTags)
  }

  // ---------------- Compute Replace Way ----------------
  io.repPol.setIdx := io.rep.repPolReadIndex // This value is not delayed by one CC (like other io input values that come from a pipeline reg), since it is used to read the PLRU bits from the memory
  io.repPol.stall := io.stall

  val validAndDirtyBits1 = updateValidAndDirtyBits(io.rep.validBits, io.rep.dirtyBits, io.rep.setTags, io.rep.index)

  // We compute a half miss check here, since we do not need to know the replacement way either way
  val halfMissCheckInMissFifoNonCrit = halfMissCheck(io.rep.index, io.rep.tag, io.missNonCritInfo.currentIndexes, io.missNonCritInfo.currentTags, io.missNonCritInfo.validMSHRs)
  val halfMissCheckInMissFifoCrit = halfMissCheck(io.rep.index, io.rep.tag, io.missCritInfo.currentIndexes, io.missCritInfo.currentTags, io.missCritInfo.validMSHRs)
  val halfMissCheckInMissFifo = (halfMissCheckInMissFifoCrit._1 || halfMissCheckInMissFifoNonCrit._1, Mux(halfMissCheckInMissFifoCrit._1, halfMissCheckInMissFifoCrit._2, halfMissCheckInMissFifoNonCrit._2))

  val halfMissCheckInUpdateStage = io.rep.index === updateStageIndex && io.rep.tag === updateStageTag && updateStageValid && !updateStageIsHit
  val halfMissIdxInUpdateStage = Mux(updateStageIsCrit, io.missCritInfo.wrPtr, io.missNonCritInfo.wrPtr)

  val isHalfMiss = halfMissCheckInMissFifo._1 || halfMissCheckInUpdateStage
  val halfMissIdx = Mux(halfMissCheckInMissFifo._1, halfMissCheckInMissFifo._2, halfMissIdxInUpdateStage)

  val coreIdReg = PipelineReg(io.rep.coreId, 0.U, !io.stall)
  val reqValidReg = PipelineReg(io.rep.reqValid, false.B, !io.stall)
  val isHalfMissReg = PipelineReg(isHalfMiss, false.B, !io.stall)
  val halfMissIdxReg = PipelineReg(halfMissIdx, 0.U, !io.stall)
  val halfMissInCritReg = PipelineReg(updateStageIsCrit || halfMissCheckInMissFifoCrit._1, false.B, !io.stall)
  val reqIdReg = PipelineReg(io.rep.reqId, 0.U, !io.stall)
  val reqRwReg = PipelineReg(io.rep.reqRw, false.B, !io.stall)
  val wDataReg = PipelineReg(io.rep.wData, 0.U, !io.stall)
  val byteEnReg = PipelineReg(io.rep.byteEn, 0.U, !io.stall)
  val validBitsReg = PipelineReg(validAndDirtyBits1._1, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  val dirtyBitsReg = PipelineReg(validAndDirtyBits1._2, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  val setTagsReg = PipelineReg(validAndDirtyBits1._3, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall)
  val blockOffsetReg = PipelineReg(io.rep.blockOffset, 0.U, !io.stall)
  val indexReg = PipelineReg(io.rep.index, 0.U, !io.stall)
  val tagReg = PipelineReg(io.rep.tag, 0.U, !io.stall)

  val validAndDirtyBits2 = updateValidAndDirtyBits(validBitsReg, dirtyBitsReg, setTagsReg, indexReg, useInvalidate = false)
  val isHit = checkIfHit(validAndDirtyBits2._1, validAndDirtyBits2._3, tagReg)

  updateStageIndex := indexReg
  updateStageTag := tagReg
  updateStageValid := reqValidReg
  updateStageIsHit := isHit._1

  // ---------------- Update Replacement policy ----------------
  val repWay = io.repPol.replaceWay
  val repWayValid = io.repPol.isValid
  val isRepDirty = validAndDirtyBits2._2(repWay)
  val isRepLineValid = validAndDirtyBits2._1(repWay)
  val dirtyTag = validAndDirtyBits2._3(repWay)
  val evict = reqValidReg && (!isHit._1 && repWayValid && !isHalfMissReg) && !io.stall

  // If the replacement way is dirty but the line is not valid---stall the pipeline
  val stallDueToDirtyInvalid = reqValidReg && (!isHit._1 && isRepDirty && !isRepLineValid)
  io.dirtyInvalidStall := stallDueToDirtyInvalid

  // If a single mshr register is full of commands then we stall the pipeline until the line is brought in
  val cmdCapacityNonCrit = io.missNonCritInfo.fullCmds(halfMissIdxReg) && io.missNonCritInfo.validMSHRs(halfMissIdxReg) && isHalfMissReg && !isHit._1
  val cmdCapacityCrit = io.missCritInfo.fullCmds(halfMissIdxReg) && io.missCritInfo.validMSHRs(halfMissIdxReg) && isHalfMissReg && !isHit._1
  val halfMissCapacity = Mux(halfMissInCritReg, cmdCapacityCrit, cmdCapacityNonCrit)
  io.halfMissCapacity := halfMissCapacity

  invalidate := evict
  invalidateWay := repWay
  invalidateIndex := indexReg

  // Update the rejection policy
  // We update the replacement policy even on a miss, since this miss later turns into a hit anyway.
  // We prevent updating the policy if the replacement way is not valid
  io.repPol.update.valid := reqValidReg && !io.stall && (isHit._1 || repWayValid)
  io.repPol.update.bits := Mux(isHit._1, isHit._2, repWay)
  io.repPol.updateCoreId := coreIdReg
  io.repPol.evict := evict
  io.repPol.isHit := isHit._1
  io.repPol.missQueueEmpty := io.missNonCritInfo.elementCnt === 0.U
  io.repPol.missQueueCores := io.missNonCritInfo.incidentCoreIds
  io.repPol.missQueueValidCores := io.missNonCritInfo.validMSHRs

  // Push request or a command to the miss fifo
  io.isMissPushCrit := halfMissInCritReg || io.repPol.pushReqToCritQueue
  updateStageIsCrit := io.repPol.pushReqToCritQueue && reqValidReg

  io.missFifoPush.pushReq := evict
  io.missFifoPush.withCmd := evict
  io.missFifoPush.pushReqEntry.tag := tagReg
  io.missFifoPush.pushReqEntry.index := indexReg
  io.missFifoPush.pushReqEntry.byteEn := Mux(reqRwReg, byteEnReg, 0.U)
  io.missFifoPush.pushReqEntry.replaceWay := repWay
  io.missFifoPush.pushReqEntry.incidentCoreId := coreIdReg

  io.missFifoPush.pushCmd := isHalfMissReg && reqValidReg && !isHit._1 && !io.stall // Push a half miss command
  io.missFifoPush.mshrIdx := halfMissIdxReg
  io.missFifoPush.pushCmdEntry.reqId := reqIdReg
  io.missFifoPush.pushCmdEntry.coreId := coreIdReg
  io.missFifoPush.pushCmdEntry.blockOffset := blockOffsetReg

  io.missFifoPush.updateByteEn := (isHalfMissReg && reqValidReg && !isHit._1 && reqRwReg && !io.stall) // Update a byte mask if it is a write request and a half miss
  io.missFifoPush.updateByteEnVal := byteEnReg
  io.missFifoPush.updateByteEnCol := blockOffsetReg
  io.missFifoPush.updateByteEnRow := halfMissIdxReg

  // Push rejected request to the rejection queue, if it is a valid request, that is a miss but does not have a valid replacement way
  io.pushReject := reqValidReg && !isHit._1 && !repWayValid && !io.stall
  io.pushRejectEntry.coreId := coreIdReg
  io.pushRejectEntry.reqId := reqIdReg
  io.pushRejectEntry.addr := Cat(tagReg, indexReg, blockOffsetReg, 0.U(log2Up(subBlockWidth / 8).W))
  io.pushRejectEntry.rw := reqRwReg
  io.pushRejectEntry.byteEn := byteEnReg
  io.pushRejectEntry.wData := wDataReg

  val byteShift = Cat(blockOffsetReg, 0.U(log2Up(subBlockWidth / 8).W))
  val blockByteMask = (byteEnReg << byteShift).asUInt

  io.read.coreId := coreIdReg
  io.read.reqValid := reqValidReg
  io.read.reqId := reqIdReg
  io.read.reqRw := reqRwReg
  io.read.wData := wDataReg
  io.read.byteEn := blockByteMask((blockWidth / 8) -1, 0)
  io.read.repValid := repWayValid
  io.read.repWay := Mux(isHalfMissReg, Mux(halfMissInCritReg, io.missCritInfo.replacementWays(halfMissIdxReg), io.missNonCritInfo.replacementWays(halfMissIdxReg)), repWay) // If it is a half miss we give the replacement way of the full miss for a write request
  io.read.isHit := isHit._1
  io.read.hitWay := isHit._2
  io.read.isRepDirty := isRepDirty
  io.read.dirtyTag := dirtyTag
  io.read.blockOffset := blockOffsetReg
  io.read.index := indexReg
  io.read.tag := tagReg

  io.invalidate.invalidate := invalidate
  io.invalidate.way := invalidateWay
  io.invalidate.index := invalidateIndex
}
