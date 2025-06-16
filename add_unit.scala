package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._



class AddUnit(bitwidth: Int, nInputs: Int) extends Module {
    val io = IO(new Bundle{
        val input = Input(Vec(nInputs, UInt(bitwidth.W)))
        val output = Output(UInt(bitwidth.W))
    })



    io.output := io.input.reduce(_ + _)
}