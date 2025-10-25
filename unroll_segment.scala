package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf


case class MagicNumber(bitwidth : Int = 32) extends Bundle
{
    val M = Input(UInt(64.W))
    val s = Input(UInt(32.W))
    val add_indicator = Input(Bool())
    val stride = Input(UInt(32.W))
}


case class UnrollSegmentIO(bitwidth : Int = 32) extends Bundle
{
    val magic = new MagicNumber(bitwidth)
    val index = Valid(Output(UInt(bitwidth.W)))
    val remainder = Output(UInt(bitwidth.W))
    val inValue = Flipped(Valid((UInt(bitwidth.W))))
    val rst = Input(Bool())
}

class UnrollSegment32(index: Int) extends Module
{
    val io = IO(new UnrollSegmentIO(32))


    val reg = RegInit(0.U(32.W))
    val remreg = RegInit(0.U(32.W))
    val vreg_1 = RegInit(false.B)
    val vreg_2 = RegInit(false.B)
    val mul = Wire(UInt(64.W))
    val int_mag_res = Wire(UInt(32.W))
    val magic_res = Reg(UInt(32.W))
    mul := io.inValue.bits * io.magic.M
    int_mag_res := (mul >> 32)
    vreg_2 := vreg_1

    when (io.magic.add_indicator)
    {
        magic_res := (int_mag_res + ((io.inValue.bits - int_mag_res) >> 1)) >> io.magic.s
    }
    .otherwise
    {
        magic_res := (int_mag_res >> io.magic.s)
    }

    when(io.rst)
    {
        reg := 0.U
        vreg_1 := false.B
        vreg_2 := false.B
        remreg := 0.U
       // SynthesizePrintf("[UnrollSegment32_%d] io.inValue.valid, bits %d\n", index.U, io.inValue.bits)
        //SynthesizePrintf("[UnrollSegment32_%d] M %d, S %d, add_indicator %d\n", index.U, io.magic.M, io.magic.s, io.magic.add_indicator)
       // SynthesizePrintf("[UnrollSegment32_%d] Reg %d, remreg %d\n", index.U, reg, remreg)
    }
    .elsewhen(io.inValue.valid)
    {
        vreg_1 := io.inValue.valid
    }


    /* 
        We break up the multiplications into two stages
    */
    when (vreg_1)
    {
        reg := magic_res
        remreg := (io.inValue.bits - (magic_res*io.magic.stride))
    }



    io.index.valid := vreg_2
    io.index.bits := reg
    io.remainder := remreg
}