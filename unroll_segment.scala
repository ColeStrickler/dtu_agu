package agu

import chisel3._
import chisel3.util._
//import firrtl.options.TargetDirAnnotation
//import midas.targetutils.SynthesizePrintf


case class MagicNumber(bitwidth : Int = 32) extends Bundle
{
    val M = Input(UInt(64.W))
    val s = Input(UInt(32.W))
    val add_indicator = Input(Bool())
    val stride = Input(UInt(32.W))
}


case class UnrollSegmentIO(bitwidth : Int = 32, maxOffsetBitWidth: Int) extends Bundle
{
    val magic = new MagicNumber(bitwidth)
    val index = Valid(Output(UInt(maxOffsetBitWidth.W)))
    val remainder = Output(UInt(maxOffsetBitWidth.W))
    val inValue = Flipped(Valid((UInt(maxOffsetBitWidth.W))))
    val rst = Input(Bool())
}

class UnrollSegment32(index: Int, maxOffsetBitWidth: Int) extends Module
{
    val io = IO(new UnrollSegmentIO(32, maxOffsetBitWidth))


    val reg = RegInit(0.U(maxOffsetBitWidth.W))
    val remreg = RegInit(0.U(maxOffsetBitWidth.W))
    val vreg_1 = RegInit(false.B)
    val vreg_2 = RegInit(false.B)
    val mul = Reg(UInt(64.W))
    val int_mag_res = Wire(UInt(32.W))
    val magic_res = Wire(UInt(32.W))


    mul := io.inValue.bits * io.magic.M // cycle 0
    int_mag_res := (mul >> 32) // cycle 1
    
    vreg_2 := vreg_1 // cycle 1
    when (io.magic.add_indicator)
    {
        magic_res := (int_mag_res + ((io.inValue.bits - int_mag_res) >> 1)) >> io.magic.s // cycle 1
    }
    .otherwise
    {
        magic_res := (int_mag_res >> io.magic.s) //  cycle 1
    }

    when(io.rst)
    {
        reg := 0.U
        vreg_1 := false.B
        vreg_2 := false.B
        remreg := 0.U
        //SynthesizePrintf("[UnrollSegment32_%d] rst\n", index.U)
        //SynthesizePrintf("[UnrollSegment32_%d] M %d, S %d, add_indicator %d\n", index.U, io.magic.M, io.magic.s, io.magic.add_indicator)
       // SynthesizePrintf("[UnrollSegment32_%d] Reg %d, remreg %d\n", index.U, reg, remreg)
    }
    .elsewhen(io.inValue.valid)
    {
        vreg_1 := io.inValue.valid  // cycle 0
        
    }


    /* 
        We break up the multiplications into two stages
    */
    when (vreg_1) // cycle 1
    {
        reg := magic_res // cycle 1
        remreg := (io.inValue.bits - (magic_res*io.magic.stride)) // cycle 1
      //  SynthesizePrintf("vreg(%d) %d %d\n", index.U, vreg_1, vreg_2)
    }


    when (vreg_2)
    {
       // SynthesizePrintf("(UnrollSegment32_%d) vreg_2 in=%d --> div %d rem %d\n", index.U, io.inValue.bits, reg, remreg)
    }
    when (vreg_1)
    {
        //SynthesizePrintf("(UnrollSegment32_%d) vreg_1 in=%d --> div %d rem %d\n", index.U, io.inValue.bits, reg, remreg)
    }

    io.index.valid := vreg_2 // cycle 2
    io.index.bits := reg // cycle 2
    io.remainder := remreg // cycle 2
}