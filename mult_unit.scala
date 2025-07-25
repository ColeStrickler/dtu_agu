package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import midas.targetutils.SynthesizePrintf




class MultUnit(bitwidth: Int, layer: Int) extends Module
{
    val io = IO(new Bundle{
        val inA = Input(UInt(bitwidth.W))
        val inB = Input(UInt(bitwidth.W))
        val out = Output(UInt(bitwidth.W))
        val overflow = Output(Bool())
    })

    val doPrint = (io.inA =/= 0.U) || (io.inB =/= 0.U)
    when (doPrint)
    {
        //SynthesizePrintf("[layer %d] : multUnit : %d * %d\n", layer.U, io.inA, io.inB)
    }


    val result = (io.inA * io.inB).asUInt
    io.out := result(bitwidth - 1, 0)
    io.overflow := result(2 * bitwidth - 1, bitwidth) =/= 0.U
}