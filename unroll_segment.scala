package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf


case class MagicNumber(bitwidth : Int = 32) extends Bundle
{
    val M = Input(UInt(bitwidth.W))
    val s = Input(UInt(bitwidth.W))
    val add_indicator = Input(Bool())
}


case class UnrollSegmentIO(bitwidth : Int = 32) extends Bundle
{
    val magic = new MagicNumber(bitwidth)
    val index = Valid(Output(UInt(bitwidth.W)))
    val remainder = Output(UInt(bitwidth.W))
    val inValue = Valid(Input(UInt(bitwidth.W)))
    val rst = Input(Bool())
}

class UnrollSegment32() extends Module
{
    val io = IO(new UnrollSegmentIO(32))


    val reg = RegInit(0.U(32.W))
    val remreg = RegInit(0.U(32.W))
    val vreg = RegInit(false.B)
    val mul = Wire(UInt(64.W))
    val int_mag_res = Wire(UInt(32.W))
    val magic_res = Wire(UInt(32.W))
    mul := io.inValue.bits * io.magic.M
    int_mag_res := (mul >> 32)


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
        vreg := false.B
        remreg := 0.U
    }
    .elsewhen(io.inValue.valid)
    {
        reg := magic_res
        vreg := io.inValue.valid
        remreg := (io.inValue.bits - magic_res)
    }



    io.index.valid := vreg
    io.index.bits := magic_res//reg
    io.remainder := remreg
}