package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf


case class MagicNumber(bitwidth : Int = 32) extends Bundle
{
    val M = Input(UInt(32.W))
    val s = Input(UInt(32.W))
    val add_indicator = Input(Bool())
}


case class UnrollSegmentIO(bitwidth : Int = 32) extends Bundle
{
    val magic = new MagicNumber(bitwidth)
    val index = Output(UInt(32.W))
    val remainder = Output(UInt(32.W))
    val inValue = Input(UInt(32.W))
}

class UnrollSegment() extends Module
{
    val io = IO(new UnrollSegmentIO(32))



    val mul = Wire(UInt(64.W))
    val int_mag_res = Wire(UInt(32.W))
    val magic_res = Wire(UInt(32.W))


    mul := io.inValue * io.magic.M
    int_mag_res := (mul >> 32)


    when (io.magic.add_indicator)
    {
        magic_res := (int_mag_res + ((io.inValue - int_mag_res) >> 1)) >> io.magic.s
    }
    .otherwise
    {
        magic_res := (int_mag_res >> io.magic.s)
    }



    io.index := magic_res
    io.remainder := io.inValue - magic_res
}