package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import midas.targetutils.SynthesizePrintf





class LayerRouter(params: AGUParams, nInputs: Int, nOutputs: Int, outputSize : Int, bitwidth: Int, maxOutputs: Int, layer: Int) extends Module 
{

    val NULL_ROUTE : Int = {
        val totalFuncUnits = params.nAdd + params.nMult + params.nPassthru
        val bits = log2Ceil(totalFuncUnits)
        if (math.pow(2, bits)-1 == totalFuncUnits)
            (math.pow(2,bits+1)-1).toInt
        else
            (math.pow(2, bits)-1).toInt
    }
    val routerRegBitsNeeded = log2Ceil(NULL_ROUTE)
    
    val io = IO(new Bundle{
        val inputs = Input(Vec(nInputs, UInt(bitwidth.W)))
        val outputs = Output(Vec(nOutputs, Vec(outputSize, UInt(bitwidth.W))))
        val routing = Input(Vec(nInputs, Vec(maxOutputs, UInt(routerRegBitsNeeded.W)))) // we need to map multiple inputs to each output
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
        
        for (j <- 0 until maxOutputs)
        {
            val sel_output = io.routing(i)(j)
            when (sel_output =/= NULL_ROUTE.U) // this is default value
            {
                val idx = if (i > 0)
                {
                    (0 until i).map(k =>
                     (0 until maxOutputs).map{x => io.routing(k)(x) === sel_output}.map(b => b.asUInt).reduce(_ + _)
                    ).reduce(_ +  _) + Mux(j.U === 1.U && io.routing(i)(0) === sel_output, 1.U, 0.U)  
                }
                else
                {
                    Mux(j.U === 1.U && io.routing(i)(0) === sel_output, 1.U, 0.U)
                }
                
                io.outputs(sel_output)(idx) := buffer(i)//io.inputs(i)

                SynthesizePrintf("[Layer%d Router] [%d] %d-%d (idx %d)\n", layer.U, buffer(i), i.U, sel_output, idx)
                assert(maxOutputs == 2) // our routing logic only works when we have 2


                // when (idx >= 2.U)
                //{
                //    //SynthesizePrintf("idx > 2 (%d)(%d) == %d", i.U, j.U, sel_output)
                //    assert(false.B)
                //    
                //}
            }
        }      
    }
}