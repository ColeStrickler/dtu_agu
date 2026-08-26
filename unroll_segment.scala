package agu

import chisel3._
import chisel3.util._
//import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf


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
    val rstGlobal = Input(Bool())
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
    val magic_s = RegInit(0.U(io.magic.s.getWidth.W)) 
    val inBits = RegInit(0.U(io.inValue.bits.getWidth.W))    
    val magic_stride = RegInit(0.U(io.magic.stride.getWidth.W))



    /*
        Unroll unit caching
    */
    val cached_res = RegInit(0.U(32.W))
    val cached_times_stride = RegInit(0.U(32.W))
    val is_cacheable = Wire(Bool())
    
    val cached_result_div = Wire(UInt(32.W))
    val cached_result_rem = Wire(UInt(32.W))
    val using_cache = RegInit(false.B)
    val cache_valid = RegInit(false.B)
    using_cache := Mux(io.rst, false.B, Mux(io.inValue.valid, is_cacheable, false.B))


    cache_valid := Mux(io.rstGlobal, false.B, Mux(vreg_2, true.B, cache_valid))
    is_cacheable := io.inValue.bits >= cached_times_stride && io.inValue.bits < (cached_times_stride + magic_stride) && cache_valid

    when (io.inValue.valid && is_cacheable)
    {
        //SynthesizePrintf("(UnrollSegment32->%d) using cache\n", index.U)
    }


    val div_times_stride = magic_res*magic_stride
    cached_times_stride := Mux(!using_cache && vreg_2, div_times_stride, cached_times_stride)
    cached_result_div := cached_res
    cached_result_rem := (io.inValue.bits - cached_times_stride)
    cached_res := Mux(vreg_2 && !using_cache, reg, cached_res)

    mul := io.inValue.bits * io.magic.M // cycle 0 -- 
    int_mag_res := (mul >> 32) // cycle 1 --> if this breaks timing, we can probably just slice instead of shift?
    
    vreg_2 := vreg_1 // cycle 1
    when (io.magic.add_indicator)
    {
        magic_res := (int_mag_res + ((inBits - int_mag_res) >> 1)) >> magic_s // cycle 1
    }
    .otherwise
    {
        magic_res := (int_mag_res >> magic_s) //  cycle 1
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
    }.elsewhen (is_cacheable && io.inValue.valid)
    {
        vreg_1 := true.B
        vreg_2 := true.B
        reg := cached_result_div
        remreg := cached_result_rem
    }
    .elsewhen(io.inValue.valid)
    {
        vreg_1 := io.inValue.valid  // cycle 0
        magic_s := io.magic.s
        inBits := io.inValue.bits
        magic_stride := io.magic.stride
        
    }




    /* 
        We break up the multiplications into two stages
    */
    when (vreg_1 && !vreg_2 && !io.rst && !using_cache) // cycle 1
    {
        reg := magic_res // cycle 1
        remreg := (inBits - (div_times_stride)) // cycle 1
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



