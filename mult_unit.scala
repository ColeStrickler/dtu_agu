package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._




class MultUnit(bitwidth: Int) extends Module
{
    val io = IO(new Bundle{
        val inA = Input(UInt(bitwidth.W))
        val inB = Input(UInt(bitwidth.W))
        val out = Output(UInt(bitwidth.W))
        val overflow = Output(Bool())
    })

    val result = (io.inA * io.inB).asUInt
    io.out := result(bitwidth - 1, 0)
    io.overflow := result(2 * bitwidth - 1, bitwidth) =/= 0.U
}