package caches.hardware.util

import chisel3._
import chisel3.util.log2Up

/**
 * A register-based fifo, which allows for updating individual elements
 */
class RegFifoWithUpdate(dataWidth: Int, depth: Int, updateFun: (UInt, UInt) => UInt = (newData, oldData) => newData) extends Module {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val update = Input(Bool())
    val rdPtr = Input(UInt(log2Up(depth).W))
    val wrPtr = Input(UInt(log2Up(depth).W))
    val updtPtr = Input(UInt(log2Up(depth).W))
    val wrData = Input(UInt(dataWidth.W))
    val updtData = Input(UInt(dataWidth.W))
    val rdData = Output(UInt(dataWidth.W))
  })

  val regMem = RegInit(VecInit(Seq.fill(depth)(0.U(dataWidth.W))))

  when(io.push) {
    regMem(io.wrPtr) := io.wrData
  }.elsewhen(io.update) {
    regMem(io.updtPtr) := updateFun(io.updtData, regMem(io.updtPtr))
  }

  io.rdData := regMem(io.rdPtr)
}

/**
 * A register based fifo which consists of blocks of data that can be updated independently.
 */
class BlockRegFifoWithUpdate(dataWidth: Int, nBlocks: Int, depth: Int, updateFun: (UInt, UInt) => UInt = (newData, oldData) => newData) extends Module() {
  val io = IO(new Bundle {
    val push = Input(Bool())
    val update = Input(Bool())
    val rdPtr = Input(UInt(log2Up(depth).W))
    val wrPtr = Input(UInt(log2Up(depth).W))
    val wrData = Input(UInt(dataWidth.W))
    val updtPtr = Input(UInt(log2Up(depth).W))
    val updtBlockIdx = Input(UInt(log2Up(nBlocks).W))
    val updtData = Input(UInt((dataWidth / nBlocks).W))
    val rdData = Output(UInt(dataWidth.W))
  })

  val blockWidth = dataWidth / nBlocks
  val blockBasedQueue = Array.fill(nBlocks)(
    Module(new RegFifoWithUpdate(dataWidth / nBlocks, depth, updateFun = updateFun)
    ))

  val readData = Wire(Vec(nBlocks, UInt((dataWidth / nBlocks).W)))

  for (blckIdx <- 0 until nBlocks) {
    blockBasedQueue(blckIdx).io.push := io.push
    blockBasedQueue(blckIdx).io.rdPtr := io.rdPtr
    blockBasedQueue(blckIdx).io.wrPtr := io.wrPtr
    blockBasedQueue(blckIdx).io.wrData := io.wrData((blockWidth - 1) + (blckIdx * blockWidth), blckIdx * blockWidth)
    blockBasedQueue(blckIdx).io.update := io.update && (io.updtBlockIdx === blckIdx.U)
    blockBasedQueue(blckIdx).io.updtPtr := io.updtPtr
    blockBasedQueue(blckIdx).io.updtData := io.updtData

    readData(blckIdx) := blockBasedQueue(blckIdx).io.rdData
  }

  io.rdData := readData.asUInt
}