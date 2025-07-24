package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import midas.targetutils.SynthesizePrintf





class LayerRouter(nInputs: Int, nOutputs: Int, outputSize : Int, bitwidth: Int, maxOutputs: Int, layer: Int) extends Module 
{

    
    val io = IO(new Bundle{
        val inputs = Input(Vec(nInputs, UInt(bitwidth.W)))
        val outputs = Output(Vec(nOutputs, Vec(outputSize, UInt(bitwidth.W))))
        val routing = Input(Vec(nInputs, Vec(maxOutputs, UInt(8.W)))) // we need to map multiple inputs to each output
        val stall = Input(Bool())
    })

    val index = VecInit(Seq.fill(nInputs+1)(VecInit(Seq.fill(nOutputs)(0.U((log2Ceil(outputSize)).W)))))
    // we can probably handle this somehow with multi-dimensionalvec



    // we store the data in a buffer do each stage takes a single cycle
    // maybe we could change this so it is only at some layers? 
    val buffer = RegInit(VecInit(Seq.fill(nInputs)(0.U(bitwidth.W))))
    for (i <- 0 until nInputs)
    {
        buffer(i) := io.inputs(i)
    }


    


    when (io.stall)
    {
        for (i <- 0 until nInputs)
        {
            buffer(i) := buffer(i)
        }
    }




    for (i <- 0 until nOutputs) {
        for (j <- 0 until outputSize) {
            io.outputs(i)(j) := 0.U
        }
    }
    // Route each output from the input specified in routing
    for (i <- 0 until nInputs) {
        
        for (x <- 0 until nOutputs) // default just pass ahead
        {
            index(i+1)(x) := index(i)(x)
        }

        for (j <- 0 until maxOutputs)
        {
            val sel_output = io.routing(i)(j)
            when (sel_output =/= Constants.NULL_ROUTE.U) // this is default value
            {
                val current_index = index(i)(sel_output)
                assert(current_index < outputSize.U)
                SynthesizePrintf("[Layer%d] input%d (%d) -> output(%d) stall=%d index %d\n", layer.U, i.U, buffer(i), sel_output, io.stall, index(i)(sel_output))

                io.outputs(sel_output)(current_index) := buffer(i)//io.inputs(i)
                index(i+1)(sel_output) := index(i)(sel_output) + 1.U
            }
        }      
    }
}