package caches.hardware.reppol

import chisel3._
import chisel3.util._
import caches.hardware.util.Constants._

class ContentionReplacementAlgorithm(nWays: Int, nCores: Int) extends Module {
  val io = IO(new Bundle {
    val evict = Input(Bool())
    val isReqCoreCritical = Input(Bool())
    val baseCandidates = Input(Vec(nWays, UInt(log2Up(nWays).W)))
    val validLineAssignments = Input(Vec(nWays, Bool()))
    val lineAssignments = Input(Vec(nWays, UInt(log2Up(nCores).W)))
    val coreLimits = Input(Vec(nCores, UInt(CONTENTION_LIMIT_WIDTH.W)))
    val criticalCores = Input(Vec(nCores, Bool()))
    val updateCore = Output(Valid(UInt(log2Up(nCores).W)))
    val replacementWay = Output(Valid(UInt(log2Up(nWays).W)))
  })

  val replaceWay = WireDefault(0.U(log2Up(nWays).W))
  val isValidReplaceWay = WireDefault(false.B)

  val coreAssignments = VecInit(Seq.fill(nWays)(0.U(log2Up(nCores).W)))
  val criticalWays = VecInit(Seq.fill(nWays)(false.B))
  val unlimitedWays = VecInit(Seq.fill(nWays)(false.B))

  val updateCore = WireDefault(false.B)
  val updateCoreId = WireDefault(0.U(log2Up(nCores).W))

  // Determine critical and unlimited ways
  for (i <- 0 until nWays) {
    val wayIdx = io.baseCandidates(i)
    val assignedCoreIdx = io.lineAssignments(wayIdx)
    val hasValidAssignment = io.validLineAssignments(wayIdx)

    val limit = Mux(hasValidAssignment, io.coreLimits(assignedCoreIdx), 0.U(CONTENTION_LIMIT_WIDTH.W))
    val isCriticalWay = Mux(hasValidAssignment, io.criticalCores(assignedCoreIdx), false.B)

    val isUnlimited = !isCriticalWay || (isCriticalWay && limit > 0.U)

    unlimitedWays(i) := isUnlimited
    criticalWays(i) := isCriticalWay
    coreAssignments(i) := assignedCoreIdx
  }

  val firstUCSetIdx = PriorityEncoder(unlimitedWays)
  val firstUCSetWayCoreCritical = criticalWays(firstUCSetIdx)
  val firstUCSetWayCore = coreAssignments(firstUCSetIdx)
  val anyNonCriticalWays = criticalWays.map(x => !x).reduce((x, y) => x || y)

  when(unlimitedWays.reduce((x, y) => x || y)) {
    // If we encounter a contention event we need to update the contention count
    when(firstUCSetWayCoreCritical && (!io.isReqCoreCritical || anyNonCriticalWays)) {
      when(io.evict) {
        updateCore := true.B
        updateCoreId := firstUCSetWayCore
      }
    }

    replaceWay := io.baseCandidates(firstUCSetIdx)
    isValidReplaceWay := true.B
  } .elsewhen(io.isReqCoreCritical) {
    replaceWay := io.baseCandidates(0)
    isValidReplaceWay := true.B
  }

  io.updateCore.valid := updateCore
  io.updateCore.bits := updateCoreId
  io.replacementWay.valid := isValidReplaceWay
  io.replacementWay.bits := replaceWay
}
