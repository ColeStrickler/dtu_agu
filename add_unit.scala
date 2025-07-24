package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import midas.targetutils.SynthesizePrintf



class AddUnit(bitwidth: Int, nInputs: Int, layer: Int) extends Module {
    val io = IO(new Bundle{
        val inA = Input(UInt(bitwidth.W))
        val inB = Input(UInt(bitwidth.W))
        val output = Output(UInt(bitwidth.W))
    })


    
    
    io.output := io.inA + io.inB
    
    SynthesizePrintf("addunit-layer%d (%d) + (%d) = [%d]\n", layer.U, io.inA, io.inB, io.output)
}


