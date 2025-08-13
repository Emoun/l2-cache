package caches.hardware.pipelined

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import caches.hardware.reppol.{BitPlruReplacementPolicy, ContentionReplacementPolicy}
import ocp.{OcpCmd, OcpResp}

class OcpCacheWrapperTest extends AnyFlatSpec with ChiselScalatestTester {
  "OcpCacheWrapper" should "accept OCP burst commands and issue OCP burst commands to external memory" in {
    val nCores = 4
    val addrWidth = 32
    val coreDataWidth = 32
    val coreBurstLen = 4
    val memDataWidth = 32
    val memBurstLen = 4

    val l2Size = 1024
    val nWays = 8
    val bytesPerBlock = 32
    val l2Sets = l2Size  / (nWays * bytesPerBlock)

    val l2RepPolGen = () => new BitPlruReplacementPolicy(nWays, l2Sets, nCores)
    val l2ContPolGen = () => new ContentionReplacementPolicy(nWays, l2Sets, nCores, l2RepPolGen)

    val l2CacheGen = () => new SharedPipelinedCache(
      sizeInBytes = l2Size,
      nWays = 8,
      nCores = nCores,
      reqIdWidth = 1,
      addressWidth = addrWidth,
      bytesPerBlock = bytesPerBlock,
      bytesPerSubBlock = 16,
      memBeatSize = memDataWidth / 8,
      memBurstLen = memBurstLen,
      l2RepPolicy = l2ContPolGen
    )

    test(new OcpCacheWrapperSingleCore(
      nCores = nCores,
      addrWidth = addrWidth,
      coreDataWidth = coreDataWidth,
      coreBurstLen = coreBurstLen,
      memDataWidth = memDataWidth,
      memBurstLen = memBurstLen,
      l2Cache = l2CacheGen
    )).withAnnotations(Seq(WriteVcdAnnotation, WriteFstAnnotation)) { dut =>
      // Default assignments
      dut.io.core.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.core.M.Addr.poke(0.U)
      dut.io.core.M.Data.poke(0.U)
      dut.io.core.M.DataByteEn.poke(0.U)
      dut.io.core.M.DataValid.poke(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U)
      dut.io.mem.S.CmdAccept.poke(0.U)
      dut.io.mem.S.DataAccept.poke(0.U)

      dut.io.scheduler.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.scheduler.M.Addr.poke(0.U)
      dut.io.scheduler.M.Data.poke(0.U)

      dut.clock.step(5)

      // --------------- Set the first core as critical ---------------
      dut.io.scheduler.M.Cmd.poke(OcpCmd.WR)
      dut.io.scheduler.M.Addr.poke(1.U)
      dut.io.scheduler.M.Data.poke(2.U)

      dut.clock.step(1)

      dut.io.scheduler.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.scheduler.M.Addr.poke(0.U)
      dut.io.scheduler.M.Data.poke(0.U)

      dut.io.scheduler.S.Resp.expect(OcpResp.DVA)

      dut.clock.step(1)

      // --------------- Issue a write command to the cache ---------------
      dut.io.core.M.Cmd.poke(OcpCmd.WR)
      dut.io.core.M.Addr.poke("h08f0".U)
      dut.io.core.M.Data.poke("hdeadbeef".U)
      dut.io.core.M.DataByteEn.poke("b1110".U)
      dut.io.core.M.DataValid.poke(1.U)

      dut.io.core.S.Resp.expect(OcpResp.NULL)
      dut.io.core.S.Data.expect(0.U)
      dut.io.core.S.CmdAccept.expect(true.B)
      dut.io.core.S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.core.M.Cmd.poke(OcpCmd.IDLE)
      dut.io.core.M.Addr.poke(0.U)
      dut.io.core.M.Data.poke("hcafebabe".U)
      dut.io.core.M.DataByteEn.poke("b0110".U)
      dut.io.core.M.DataValid.poke(1.U)

      dut.io.core.S.Resp.expect(OcpResp.NULL)
      dut.io.core.S.Data.expect(0.U)
      dut.io.core.S.CmdAccept.expect(false.B)
      dut.io.core.S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.core.M.Data.poke("hbeefdead".U)
      dut.io.core.M.DataByteEn.poke("b1111".U)
      dut.io.core.M.DataValid.poke(1.U)

      dut.io.core.S.Resp.expect(OcpResp.NULL)
      dut.io.core.S.Data.expect(0.U)
      dut.io.core.S.CmdAccept.expect(false.B)
      dut.io.core.S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.core.M.Data.poke("hbabecafe".U)
      dut.io.core.M.DataByteEn.poke("b1001".U)
      dut.io.core.M.DataValid.poke(1.U)

      dut.io.core.S.Resp.expect(OcpResp.NULL)
      dut.io.core.S.Data.expect(0.U)
      dut.io.core.S.CmdAccept.expect(false.B)
      dut.io.core.S.DataAccept.expect(true.B)

      dut.clock.step(1)

      dut.io.core.M.Data.poke(0.U)
      dut.io.core.M.DataValid.poke(0.U)

      dut.io.core.S.Resp.expect(OcpResp.DVA)
      dut.io.core.S.Data.expect(0.U)
      dut.io.core.S.CmdAccept.expect(false.B)
      dut.io.core.S.DataAccept.expect(false.B)

      dut.clock.step(5)

      // --------------- Expect the cache to issue a read command ---------------
      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("h08e0".U) // TODO: Double check this address
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave not being ready
      dut.clock.step(2)

      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("h08e0".U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.CmdAccept.poke(1.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h00df00df".U)
      dut.io.mem.S.CmdAccept.poke(0.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hbadc0ffe".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hd15ea5ef".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hbaddbeef".U)

      dut.clock.step(1)

      // --------------- Expect the cache to issue a second read command ---------------
      dut.io.mem.M.Cmd.expect(OcpCmd.RD)
      dut.io.mem.M.Addr.expect("h08f0".U) // TODO: Double check this address
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.CmdAccept.poke(1.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hbadf00df".U)
      dut.io.mem.S.CmdAccept.poke(0.U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("h600dcafe".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hfaceb00c".U)

      dut.clock.step(1)

      dut.io.mem.M.Cmd.expect(OcpCmd.IDLE)
      dut.io.mem.M.Addr.expect(0.U)
      dut.io.mem.M.Data.expect(0.U)
      dut.io.mem.M.DataByteEn.expect(0.U)
      dut.io.mem.M.DataValid.expect(0.U)

      // Simulate the slave being ready now
      dut.io.mem.S.Resp.poke(OcpResp.DVA)
      dut.io.mem.S.Data.poke("hb00cb00c".U)

      dut.clock.step(1)

      dut.io.mem.S.Resp.poke(OcpResp.NULL)
      dut.io.mem.S.Data.poke(0.U)

      dut.clock.step(1)
    }
  }
}
