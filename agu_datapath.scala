package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation

class AGUDatapath(nLoopRegs : Int, nConstRegs: Int, nLayers: Int, nMultUnits: Int, nAddUnits: Int, nPassthru: Int) extends Module
{
    val bitwidth = 32
    val totalFuncUnits = nAddUnits + nMultUnits + nPassthru
    val io = IO(new Bundle{
        val doGen = Input(Bool())
        val output = Output(UInt(bitwidth.W))
        val reset = Input(Bool())
        val RoutingConfigIn = Input(Vec(nLayers+1, Vec(totalFuncUnits, UInt(8.W))))
        val StallLayer = Input(Vec(nLayers+1, Bool()))



        val LoopRegsIn =        Input(Vec(nLoopRegs, UInt(bitwidth.W)))
        val LoopIncRegsIn =     Input(Vec(nLoopRegs, UInt(bitwidth.W)))
        val ConstantRegsIn =    Input(Vec(nConstRegs, UInt(bitwidth.W)))
    })

    /*
        We will need to have these compiled for us from source. We can maintain an invariant like we were in the processor model
    */
    val LoopRegs = Wire(Vec(nLoopRegs, UInt(bitwidth.W)))
    val LoopIncRegs = Wire(Vec(nLoopRegs, UInt(bitwidth.W)))
    val ConstantRegs = Wire(Vec(nConstRegs, UInt(bitwidth.W)))
    LoopRegs := io.LoopRegsIn
    LoopIncRegs := io.LoopIncRegsIn
    ConstantRegs := io.ConstantRegsIn



    /*
        Initialize Layers
    */

    val AddLayers = VecInit(Seq.fill(nLayers)(
        VecInit(Seq.fill(nAddUnits)(
            Module(new AddUnit(nAddUnits, 2)).io
        ))
    ))

    val MultLayers = VecInit(Seq.fill(nLayers)(
        VecInit(Seq.fill(nMultUnits)(
            Module(new MultUnit(bitwidth)).io
        ))
    ))


    val PassThru = WireInit(VecInit(Seq.fill(nLayers)(VecInit(Seq.fill(nPassthru)(0.U(bitwidth.W))))))
    val routing = VecInit(Seq.fill(nLayers+1)(
        Module(new LayerRouter(nAddUnits + nMultUnits + nPassthru, nAddUnits + nMultUnits + nPassthru, 4, 32)).io
    ))


    /*
        Loop registers
    */



    /*
        Map control to routers
    */
    routing.zipWithIndex.foreach { case (router, i) => 
        router.routing := io.RoutingConfigIn(i)    
        router.stall := io.StallLayer(i)
    }

    for (i <- 0 until nLayers+1)
    {
        for (j <- 0 until nAddUnits + nMultUnits)
            routing(i).routing(j) := j.U
    }


    /*
        Put loop and constant registers into router     
    */
    for (i <- 0 until nLoopRegs)
    {
        routing(0).inputs(i) := LoopRegs(i)
    }

    for (i <- 0 until nConstRegs)
    {
        routing(0).inputs(i+nLoopRegs) := ConstantRegs(i)
    }



    /*
        Iterate over layers, use routers to route intermediate results
    */
    for (i <- 0 until nLayers)
    {
        for (j <- 0 until nAddUnits)
        {
            for (x <- 0 until 2) // we currently only support 2, not sure if we would see benefit with more
            {
                AddLayers(i)(j).input(x) := routing(i).outputs(j)(x)
            }

            routing(i+1).inputs(j) := AddLayers(i)(j).output
        }

        for (j <- 0 until nMultUnits)
        {
            MultLayers(i)(j).inA := routing(i).outputs(j+nAddUnits)(0)
            MultLayers(i)(j).inB := routing(i).outputs(j+nAddUnits)(1)
            for (x <- 2 until 4)
            {
                assert(routing(i).outputs(j)(x) === 0.U)
            }

            routing(i+1).inputs(j+nAddUnits) := MultLayers(i)(j).out
        }


        for (j <- 0 until nPassthru)
        {
            PassThru(i)(j) := routing(i).outputs(j+nAddUnits+nMultUnits)(0)
            for (x <- 1 until 4)
            {
                assert(routing(i).outputs(j)(x) === 0.U)
            }
            routing(i+1).inputs(j+nAddUnits+nMultUnits) := PassThru(i)(j)

        }

    }
    

    io.output := routing(nLayers).outputs(nAddUnits+nMultUnits)(0) // output will always come from last pass thru --> some inefficiency here
}

