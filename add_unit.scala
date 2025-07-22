package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import midas.targetutils.SynthesizePrintf



class AddUnit(bitwidth: Int, nInputs: Int, layer: Int) extends Module {
    val io = IO(new Bundle{
        val input = Input(Vec(nInputs, UInt(bitwidth.W)))
        val output = Output(UInt(bitwidth.W))
    })


    val doPrint = (io.input(0) =/= 0.U) || (io.input(1) =/= 0.U)
    when (doPrint)
    {
        SynthesizePrintf("layer %d : addUnit %d+%d\n", layer.U, io.input(0), io.input(1))
    }

    io.output := io.input.reduce(_ + _)
}