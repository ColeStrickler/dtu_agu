package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf



/* 
    Works for powers of 2
*/
class ShiftDivider(bitwidth: Int) extends Module
{
    val io = IO(new Bundle {
        val data_size = Flipped(Valid(UInt(bitwidth.W)))
        val addr_in = Input(UInt(bitwidth.W))
        val quotient = Output(UInt(bitwidth.W))
        val remainder = Output(UInt(bitwidth.W))
    })

    /*
        We can avoid a division here if we enforce power of 2 data_size, which is reasonable
    */
    val allowedSizes = Seq(1, 2, 4, 8, 16, 32).map(_.U(bitwidth.W))
    val isPowerOf2 = io.data_size.bits =/= 0.U && (io.data_size.bits & (io.data_size.bits - 1.U)) === 0.U
    val isAllowedValue = allowedSizes.map(_ === io.data_size.bits).reduce(_ || _)

    when(io.data_size.valid) {
        assert(isPowerOf2 && isAllowedValue, "data_size must be one of the allowed power-of-2 values: 1,2,4,8,16,32")
    }

    

    val res = Wire(UInt(bitwidth.W)) 
    val rem = Wire(UInt(bitwidth.W))
    res := 0.U // default
    switch(io.data_size.bits)
    {
        is(1.U)   { res := io.addr_in }
        is(2.U)   { res := (io.addr_in >> 1.U) }
        is(4.U)   { res := (io.addr_in >> 2.U) }
        is(8.U)   { res := (io.addr_in >> 3.U) }
        is(16.U)  { res := (io.addr_in >> 4.U) }
        is(32.U)  { res := (io.addr_in >> 5.U) }
    }

    rem := io.addr_in - res
    io.remainder := rem
    io.quotient := res



}