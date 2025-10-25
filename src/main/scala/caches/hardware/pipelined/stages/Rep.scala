package caches.hardware.pipelined.stages

import chisel3._
import chisel3.util._
import caches.hardware.util._
import caches.hardware.reppol._
import caches.hardware.pipelined.{MshrInfoIO, MshrPushIO, RejectionQueueEntry}

case class IsHitResult(hit: Bool, hitIdx: UInt)

case class HalfMissCheckResult(isHalfMiss: Bool, halfMissIdx: UInt)

case class ValidDirtyBitsResult(valid: Vec[Bool], dirty: Vec[Bool], tags: Vec[UInt])

class InvalidateLineIO(nWays: Int, indexWidth: Int) extends Bundle {
  val invalidate = Output(Bool())
  val way = Output(UInt(log2Up(nWays).W))
  val index = Output(UInt(indexWidth.W))
}

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

class RepTopIO(nCores: Int, nSets: Int, nWays: Int, nMshrs: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Bundle {
  private val blockOffWidth = log2Up(blockWidth / subBlockWidth)
  private val addrWidth = tagWidth + indexWidth + blockOffWidth + log2Up(subBlockWidth / 8)

  val stall = Input(Bool())
  // Input from previous stage
  val rep = new RepIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth)
  // Info signals from miss and wb queues
  val missNonCritInfo = Flipped(new MshrInfoIO(nCores, nMshrs, nWays, indexWidth, tagWidth))
  val missCritInfo = Flipped(new MshrInfoIO(nCores, nMshrs, nWays, indexWidth, tagWidth))
  val nonCritWbPop = Input(Bool())
  val nonCritWbEntryIsCrit = Input(Bool())
  // Connection to replacement policy
  val repPolCtrl = Flipped(new ReplacementPolicyControlIO(nWays, nSets))
  val repPolInfo = Flipped(new ReplacementPolicyInfoIO(nCores, nMshrs))
  // Valid and dirty control forwarding signals
  val setLineValid = Flipped(new SetLineValidIO(nWays, indexWidth, tagWidth))
  val dirtyCtrl = Flipped(new DirtyCtrlIO(nWays, indexWidth))
  // Connection to rejection queue
  val pushReject = Output(Bool())
  val pushRejectEntry = Flipped(new RejectionQueueEntry(nCores, addrWidth, subBlockWidth, reqIdWidth))
  // Line invalidation control
  val invalidate = new InvalidateLineIO(nWays, indexWidth)
  // Miss queue control
  val missFifoPush = Flipped(new MshrPushIO(nCores, nMshrs, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, subBlockWidth))
  val isMissPushCrit = Output(Bool()) // TODO: Add this into the miss fifo push
  // Output to the next stage
  val read = Flipped(new ReadIO(nCores, nWays, reqIdWidth, tagWidth, indexWidth, blockOffWidth, blockWidth, subBlockWidth))
  val halfMissCapacity = Output(Bool())
  val dirtyInvalidStall = Output(Bool())
  val writeMissHazard = Output(Bool())
}

/**
 * Rep stage of the cache pipeline
 */
class Rep(nCores: Int, nSets: Int, nWays: Int, nMshrs: Int, reqIdWidth: Int, tagWidth: Int, indexWidth: Int, blockWidth: Int, subBlockWidth: Int) extends Module() {
  val io = IO(new RepTopIO(nCores, nSets, nWays, nMshrs, reqIdWidth, tagWidth, indexWidth, blockWidth, subBlockWidth))

  val invalidate = WireDefault(false.B)
  val invalidateWay = WireDefault(0.U(log2Up(nWays).W))
  val invalidateIndex = WireDefault(0.U(indexWidth.W))
  val updateStageIndex = WireDefault(0.U(indexWidth.W))
  val updateStageTag = WireDefault(0.U(tagWidth.W))
  val updateStageValid = WireDefault(false.B)
  val updateStageIsHit = WireDefault(false.B)
  val updateStageIsCrit = WireDefault(false.B)
  val updateStageMshrIdx = WireDefault(0.U(log2Up(nWays).W))

  def wayConflictsInMissQ(index: UInt, repWay: UInt): Bool = {
    val writeWayMatches = Wire(Vec(nMshrs, Bool()))

    for (i <- 0 until nMshrs) {
      val critConflict = io.missCritInfo.currentIndexes(i) === index && io.missNonCritInfo.replacementWays(i) === repWay
      val nonCritConflict = io.missNonCritInfo.currentIndexes(i) === index && io.missNonCritInfo.replacementWays(i) === repWay

      writeWayMatches(i) := critConflict || nonCritConflict
    }

    writeWayMatches.reduce((x, y) => y || x )
  }

  def checkIfHitSetLineValid(isHitCurr: Bool, currHitWay: UInt, currIdx: UInt, currTag: UInt): IsHitResult = {
    val hit = WireDefault(false.B)
    val hitWay = WireDefault(0.U(log2Up(nWays).W))

    when(io.setLineValid.refill && io.setLineValid.index === currIdx && io.setLineValid.tag === currTag) {
      hit := true.B
      hitWay := io.setLineValid.way
    } .otherwise {
      hit := isHitCurr
      hitWay := currHitWay
    }

    IsHitResult(hit, hitWay)
  }

  def checkIfHit(validBits: Vec[Bool], tags: Vec[UInt], currTag: UInt): IsHitResult = {
    // Compare tags and check if there is a hit and where
    val hits = Wire(Vec(nWays, Bool()))
    for (wayIdx <- 0 until nWays) {
      hits(wayIdx) := validBits(wayIdx) && (currTag === tags(wayIdx))
    }

    val hit = hits.reduce((x, y) => x || y)
    val hitWay = PriorityEncoder(hits)

    IsHitResult(hit, hitWay)
  }

  def halfMissCheck(currIndex: UInt, currTag: UInt, checkIdxs: Vec[UInt], checkTags: Vec[UInt], checkValids: Vec[Bool]): HalfMissCheckResult = {
    val missMatches = Wire(Vec(nMshrs, Bool()))

    for (mshr <- 0 until nMshrs) {
      val mshrIndex = checkIdxs(mshr)
      val mshrTag = checkTags(mshr)
      val validMshr = checkValids(mshr)

      missMatches(mshr) := mshrIndex === currIndex && mshrTag === currTag && validMshr
    }

    HalfMissCheckResult(missMatches.reduce((x, y) => x || y), PriorityEncoder(missMatches))
  }

  def updateValidAndDirtyBits(inValid: Vec[Bool], inDirty: Vec[Bool], inTags: Vec[UInt], currIdx: UInt, useInvalidate: Boolean = true): ValidDirtyBitsResult = {
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

    ValidDirtyBitsResult(newValidBits, newDirtyBits, newTags)
  }

  // ---------------- Compute Replace Way ----------------
  io.repPolCtrl.setIdx := io.rep.repPolReadIndex // This value is not delayed by one CC (like other io input values that come from a pipeline reg), since it is used to read the PLRU bits from the memory
  io.repPolCtrl.stall := io.stall

  val validAndDirtyBits1 = updateValidAndDirtyBits(io.rep.validBits, io.rep.dirtyBits, io.rep.setTags, io.rep.index)

  // We compute a half miss check here, since we do not need to know the replacement way either way
  val halfMissCheckInMissFifoNonCrit = halfMissCheck(io.rep.index, io.rep.tag, io.missNonCritInfo.currentIndexes, io.missNonCritInfo.currentTags, io.missNonCritInfo.validMSHRs)
  val halfMissCheckInMissFifoCrit = halfMissCheck(io.rep.index, io.rep.tag, io.missCritInfo.currentIndexes, io.missCritInfo.currentTags, io.missCritInfo.validMSHRs)

  val halfMissCheckInUpdateStage = io.rep.index === updateStageIndex && io.rep.tag === updateStageTag && updateStageValid && !updateStageIsHit

  val isHalfMiss = halfMissCheckInMissFifoNonCrit.isHalfMiss || halfMissCheckInMissFifoCrit.isHalfMiss || halfMissCheckInUpdateStage
  val halfMissIdx = WireDefault(0.U(log2Up(nMshrs).W))
  val isHalfMissInCrit = WireDefault(false.B)
  when(halfMissCheckInUpdateStage) {
    halfMissIdx := updateStageMshrIdx
    isHalfMissInCrit := updateStageIsCrit
  } .elsewhen (halfMissCheckInMissFifoCrit.isHalfMiss) {
   halfMissIdx := halfMissCheckInMissFifoCrit.halfMissIdx
   isHalfMissInCrit := true.B
  } .elsewhen (halfMissCheckInMissFifoNonCrit.isHalfMiss) {
   halfMissIdx := halfMissCheckInMissFifoNonCrit.halfMissIdx
   isHalfMissInCrit := false.B
  }

  val isHitCheck = checkIfHit(validAndDirtyBits1.valid, validAndDirtyBits1.tags, io.rep.tag)

  val coreIdReg = PipelineReg(io.rep.coreId, 0.U, !io.stall)
  val reqValidReg = PipelineReg(io.rep.reqValid, false.B, !io.stall)
  val isReqHit = PipelineReg(isHitCheck.hit, false.B, !io.stall)
  val reqHitWay = PipelineReg(isHitCheck.hitIdx, 0.U, !io.stall)
  val isHalfMissReg = PipelineReg(isHalfMiss, false.B, !io.stall)
  val halfMissIdxReg = PipelineReg(halfMissIdx, 0.U, !io.stall)
  val halfMissInCritReg = PipelineReg(isHalfMissInCrit, false.B, !io.stall)
  val reqIdReg = PipelineReg(io.rep.reqId, 0.U, !io.stall)
  val reqRwReg = PipelineReg(io.rep.reqRw, false.B, !io.stall)
  val wDataReg = PipelineReg(io.rep.wData, 0.U, !io.stall)
  val byteEnReg = PipelineReg(io.rep.byteEn, 0.U, !io.stall)
  val validBitsReg = PipelineReg(validAndDirtyBits1.valid, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  val dirtyBitsReg = PipelineReg(validAndDirtyBits1.dirty, VecInit(Seq.fill(nWays)(false.B)), !io.stall)
  val setTagsReg = PipelineReg(validAndDirtyBits1.tags, VecInit(Seq.fill(nWays)(0.U(tagWidth.W))), !io.stall)
  val blockOffsetReg = PipelineReg(io.rep.blockOffset, 0.U, !io.stall)
  val indexReg = PipelineReg(io.rep.index, 0.U, !io.stall)
  val tagReg = PipelineReg(io.rep.tag, 0.U, !io.stall)

  // ---------------- Update Replacement policy ----------------

  val validAndDirtyBits2 = updateValidAndDirtyBits(validBitsReg, dirtyBitsReg, setTagsReg, indexReg, useInvalidate = false)
  val isStillHitCheck = checkIfHitSetLineValid(isReqHit, reqHitWay, indexReg, tagReg)
  val isHalfMissInUpdateStage = isHalfMissReg && !(!halfMissInCritReg && io.repPolInfo.updateCoreReachedLimit) // If the core has reached limit and there is already a request in the non-critical q then we turn it into a full new request

  val repWay = io.repPolCtrl.replaceWay
  val repWayValid = io.repPolCtrl.isValid
  val isRepLineDirty = validAndDirtyBits2.dirty(repWay)
  val isRepLineValid = validAndDirtyBits2.valid(repWay)
  val dirtyTag = validAndDirtyBits2.tags(repWay)

  val evict = reqValidReg && (!isStillHitCheck.hit && repWayValid && !isHalfMissInUpdateStage) && !io.stall

  // Replacement policy connections

  // We update the replacement policy even on a miss, since this miss later turns into a hit anyway.
  // We prevent updating the policy if the replacement way is not valid
  io.repPolCtrl.update.valid := reqValidReg && !io.stall && (isStillHitCheck.hit || repWayValid)
  io.repPolCtrl.update.bits := Mux(isStillHitCheck.hit, isStillHitCheck.hitIdx, repWay)
  io.repPolCtrl.evict := evict
  io.repPolInfo.updateCoreId := coreIdReg
  io.repPolInfo.isHit := isStillHitCheck.hit
  io.repPolInfo.missQueueCores := io.missNonCritInfo.incidentCoreIds
  io.repPolInfo.missQueueValidCores := io.missNonCritInfo.validMSHRs
  io.repPolInfo.nonCritWbPop := io.nonCritWbPop
  io.repPolInfo.nonCritWbEntryIsCrit := io.nonCritWbEntryIsCrit

  // Push request or a command to the miss fifo
  io.isMissPushCrit := (isHalfMissInUpdateStage && halfMissInCritReg) || io.repPolInfo.updateCoreReachedLimit
  updateStageIsCrit := io.repPolInfo.updateCoreReachedLimit && reqValidReg

  // Miss fifo connections
  io.missFifoPush.pushReq := evict
  io.missFifoPush.withCmd := evict
  io.missFifoPush.pushReqEntry.tag := tagReg
  io.missFifoPush.pushReqEntry.index := indexReg
  io.missFifoPush.pushReqEntry.byteEn := Mux(reqRwReg, byteEnReg, 0.U)
  io.missFifoPush.pushReqEntry.replaceWay := repWay
  io.missFifoPush.pushReqEntry.incidentCoreId := coreIdReg
  io.missFifoPush.pushReqEntry.isCrit := io.repPolInfo.updateCoreIsCrit

  io.missFifoPush.pushCmd := isHalfMissInUpdateStage && reqValidReg && !isStillHitCheck.hit && !io.stall // Push a half miss command
  io.missFifoPush.mshrIdx := halfMissIdxReg
  io.missFifoPush.pushCmdEntry.reqId := reqIdReg
  io.missFifoPush.pushCmdEntry.coreId := coreIdReg
  io.missFifoPush.pushCmdEntry.blockOffset := blockOffsetReg

  io.missFifoPush.updateByteEn := (isHalfMissInUpdateStage && reqValidReg && !isStillHitCheck.hit && reqRwReg && !io.stall) // Update a byte mask if it is a write request and a half miss
  io.missFifoPush.updateByteEnVal := byteEnReg
  io.missFifoPush.updateByteEnCol := blockOffsetReg
  io.missFifoPush.updateByteEnRow := halfMissIdxReg

  // Rejection queue connections

  // Push rejected request to the rejection queue, if it is a valid request, that is a miss but does not have a valid replacement way
  io.pushReject := reqValidReg && !isStillHitCheck.hit && !repWayValid && !io.stall
  io.pushRejectEntry.coreId := coreIdReg
  io.pushRejectEntry.reqId := reqIdReg
  io.pushRejectEntry.addr := Cat(tagReg, indexReg, blockOffsetReg, 0.U(log2Up(subBlockWidth / 8).W))
  io.pushRejectEntry.rw := reqRwReg
  io.pushRejectEntry.byteEn := byteEnReg
  io.pushRejectEntry.wData := wDataReg

  // Outputs

  val byteShift = Cat(blockOffsetReg, 0.U(log2Up(subBlockWidth / 8).W))
  val blockByteMask = (byteEnReg << byteShift).asUInt

  io.read.coreId := coreIdReg
  io.read.isRepWayCrit := io.repPolInfo.isReplacementWayCrit
  io.read.repWayAtLimit := io.repPolInfo.isReplacementWayAtLimit
  io.read.reqValid := reqValidReg
  io.read.reqId := reqIdReg
  io.read.reqRw := reqRwReg
  io.read.wData := wDataReg
  io.read.byteEn := blockByteMask((blockWidth / 8) -1, 0)
  io.read.repValid := repWayValid
  io.read.repWay := Mux(isHalfMissInUpdateStage, Mux(halfMissInCritReg, io.missCritInfo.replacementWays(halfMissIdxReg), io.missNonCritInfo.replacementWays(halfMissIdxReg)), repWay) // If it is a half miss we give the replacement way of the full miss for a write request
  io.read.isHit := isStillHitCheck.hit
  io.read.hitWay := isStillHitCheck.hitIdx
  io.read.isRepDirty := isRepLineDirty
  io.read.dirtyTag := dirtyTag
  io.read.blockOffset := blockOffsetReg
  io.read.index := indexReg
  io.read.tag := tagReg

  // Cache line invalidation logic
  invalidate := evict
  invalidateWay := repWay
  invalidateIndex := indexReg

  io.invalidate.invalidate := invalidate
  io.invalidate.way := invalidateWay
  io.invalidate.index := invalidateIndex

  // Forwarding signals to the previous stage
  updateStageIndex := indexReg
  updateStageTag := tagReg
  updateStageValid := reqValidReg
  updateStageIsHit := isStillHitCheck.hit
  updateStageMshrIdx := Mux(evict, Mux(io.repPolInfo.updateCoreReachedLimit, io.missCritInfo.wrPtr, io.missNonCritInfo.wrPtr), halfMissIdxReg)

  // ---------------- Stalls due to hazards ----------------

  // If we are writing some data into some line that an outstanding request will write into
  // - not a half miss, a request in either critical or non-critical queues is attempting to write to the same line as we are
  // if we are a write request that is not a half miss (eviction), and we are evicting a line that someone else will bring some data in before us, we stall
  val missWriteHazard = false.B //reqValidReg && reqRwReg && wayConflictsInMissQ(indexReg, repWay) && repWayValid
  io.writeMissHazard := missWriteHazard

  // If the replacement way is dirty but the line is not valid---stall the pipeline
  val stallDueToDirtyInvalid = reqValidReg && (!isStillHitCheck.hit && isRepLineDirty && !isRepLineValid)
  io.dirtyInvalidStall := stallDueToDirtyInvalid

  // If a single mshr register is full of commands then we stall the pipeline until the line is brought in
  val cmdCapacityNonCrit = io.missNonCritInfo.fullCmds(halfMissIdxReg) && io.missNonCritInfo.validMSHRs(halfMissIdxReg) && isHalfMissInUpdateStage && !isStillHitCheck.hit
  val cmdCapacityCrit = io.missCritInfo.fullCmds(halfMissIdxReg) && io.missCritInfo.validMSHRs(halfMissIdxReg) && isHalfMissInUpdateStage && !isStillHitCheck.hit
  val halfMissCapacity = Mux(halfMissInCritReg, cmdCapacityCrit, cmdCapacityNonCrit)
  io.halfMissCapacity := halfMissCapacity
}
