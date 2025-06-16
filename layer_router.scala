package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._







class LayerRouter(nInputs: Int, nOutputs: Int, outputSize : Int, bitwidth: Int) extends Module 
{
    val io = IO(new Bundle{
        val inputs = Input(Vec(nInputs, UInt(bitwidth.W)))
        val outputs = Output(Vec(nOutputs, Vec(outputSize, UInt(bitwidth.W))))
        val routing = Input(Vec(nInputs, UInt(log2Ceil(nOutputs).W))) // we need to map multiple inputs to each output
    })

    val index = VecInit(Seq.fill(nInputs+1)(VecInit(Seq.fill(nOutputs)(0.U((log2Ceil(outputSize)).W)))))
    // we can probably handle this somehow with multi-dimensionalvec


    for (i <- 0 until nOutputs) {
        for (j <- 0 until outputSize) {
            io.outputs(i)(j) := 0.U
        }
    }
    // Route each output from the input specified in routing
    for (i <- 0 until nInputs) {

        val sel_output = io.routing(i)
        val current_index = index(i)(sel_output)
        assert(current_index < outputSize.U)
        io.outputs(sel_output)(current_index) := io.inputs(i)
        index(i+1)(sel_output) := index(i)(sel_output) + 1.U
    }
}