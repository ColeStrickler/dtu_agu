package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf

class AGUDatapath(params: AGUParams, nLoopRegs : Int, nConstRegs: Int, nLayers: Int, nMultUnits: Int, nAddUnits: Int, nPassthru: Int, maxVarOutputs: Int) extends Module
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

    val bitwidth = 32
    val totalFuncUnits = nAddUnits + nMultUnits + nPassthru
    val io = IO(new Bundle{
        val doGen = Input(Bool())
        val output = Output(UInt(bitwidth.W))
        val reset = Input(Bool())
        val RoutingConfigIn = Input(Vec(nLayers+1, Vec(totalFuncUnits, Vec(maxVarOutputs, UInt(routerRegBitsNeeded.W)))))
        val StallLayer = Input(Vec(nLayers+1, Bool()))


        val LoopRegsIn =            Input(Vec(nLoopRegs, UInt(bitwidth.W)))
        val LoopIncRegsIn =         Input(Vec(nLoopRegs, UInt(bitwidth.W)))
        val ConstantRegsIn =        Input(Vec(nConstRegs, UInt(bitwidth.W)))
        val ConstantArrayRegIn =    Input(Vec(params.nConstArray, UInt(bitwidth.W)))
    })

    /*
        We will need to have these compiled for us from source. We can maintain an invariant like we were in the processor model
    */
    val LoopRegs = Wire(Vec(nLoopRegs, UInt(bitwidth.W)))
    val LoopIncRegs = Wire(Vec(nLoopRegs, UInt(bitwidth.W)))
    val ConstantRegs = Wire(Vec(nConstRegs, UInt(bitwidth.W)))
    val ConstantArrayRegs = Wire(Vec(params.nConstArray, UInt(bitwidth.W)))
    LoopRegs := io.LoopRegsIn
    LoopIncRegs := io.LoopIncRegsIn
    ConstantRegs := io.ConstantRegsIn
    ConstantArrayRegs := io.ConstantArrayRegIn


    when (io.doGen)
    {
       // SynthesizePrintf("[AGUDatapath] io.doGen %d\n", io.doGen)
        for (i <- 0 until nLoopRegs)
        {
         //   SynthesizePrintf("loopreg(%d) %d\n", i.U, LoopRegs(i))
         //   SynthesizePrintf("loopincreg(%d) %d\n", i.U, LoopIncRegs(i))
        }

        for (i <- 0 until nConstRegs)
        {
           // SynthesizePrintf("constReg(%d) %d\n", i.U, ConstantRegs(i))
        }

        for (i <- 0 until params.nConstArray)
        {
           // SynthesizePrintf("constArrayReg(%d) %d\n", i.U, ConstantArrayRegs(i))
        }
    }


    /*
        Initialize Layers
    */

    
    val AddLayers = VecInit(
        (0 until nLayers).map { layerIdx =>
            VecInit(
            (0 until nAddUnits).map { unitIdx =>
                val addUnit = Module(new AddUnit(bitwidth, 2, layerIdx)) // pass layerIdx
                addUnit.io
            }
            )
        }
    )

    val MultLayers = VecInit(
        (0 until nLayers).map { layerIdx =>
            VecInit(
            (0 until nMultUnits).map { unitIdx =>
                val multUnit = Module(new MultUnit(bitwidth, layerIdx)) // pass layerIdx
                multUnit.io
            }
            )
        }
    )

    val nUnits = nAddUnits + nMultUnits + nPassthru
    val PassThru = WireInit(VecInit(Seq.fill(nLayers)(VecInit(Seq.fill(nPassthru)(0.U(bitwidth.W))))))
    val routing = VecInit(
        (0 to nLayers).map { layerIdx =>
            val router = Module(new LayerRouter(params, nUnits, nUnits, 2, 32, maxVarOutputs, layerIdx)) // you can pass layerIdx if needed
            router.io
        }
    )


    /*
        Map control to routers
    */
    routing.zipWithIndex.foreach { case (router, i) => 
        router.routing := io.RoutingConfigIn(i)    
        router.stall := io.StallLayer(i)
    }



    /*
        Put loop and constant registers into router 


        We flipped the indexes?
    */
    for (i <- 0 until nConstRegs)
    {
        routing(0).inputs(i) := ConstantRegs(i)
    }

    for (i <- 0 until params.nConstArray)
    {
        routing(0).inputs(i + nConstRegs) := ConstantArrayRegs(i)
    }


    for (i <- 0 until nLoopRegs)
    {
        routing(0).inputs(i + nConstRegs + params.nConstArray) := LoopRegs(i)
    }
    assert(nLoopRegs + params.nConstArray + nConstRegs <= nUnits)
    // make sure everything is initialized
    if (nLoopRegs + params.nConstArray + nConstRegs < nUnits)
    {
        for (i <- (nLoopRegs + params.nConstArray + nConstRegs) until nUnits)
        {
            routing(0).inputs(i) := NULL_ROUTE.U
        }
    }


    



    /*
        Iterate over layers, use routers to route intermediate results
    */
    for (i <- 0 until nLayers)
    {
        for (j <- 0 until nAddUnits)
        {
            //SynthesizePrintf("layer%d addUnit%d <-- input %d\n", i.U, j.U, routing(i).outputs(j)(0))
            //SynthesizePrintf("layer%d addUnit%d <-- input %d\n", i.U, j.U, routing(i).outputs(j)(1))
            AddLayers(i)(j).inA := routing(i).outputs(j)(0)
            AddLayers(i)(j).inB := routing(i).outputs(j)(1)
            routing(i+1).inputs(j) := AddLayers(i)(j).output
        }

        for (j <- 0 until nMultUnits)
        {
            MultLayers(i)(j).inA := routing(i).outputs(j+nAddUnits)(0)
            MultLayers(i)(j).inB := routing(i).outputs(j+nAddUnits)(1)
            //for (x <- 2 until 4)
            //{
            //    assert(routing(i).outputs(j)(x) === 0.U)
            //}

            routing(i+1).inputs(j+nAddUnits) := MultLayers(i)(j).out
        }


        for (j <- 0 until nPassthru)
        {
            PassThru(i)(j) := routing(i).outputs(j+nAddUnits+nMultUnits)(0)
            //for (x <- 1 until 2)
            //{
            //    assert(routing(i).outputs(j)(x) === 0.U)
            //}
            when(PassThru(i)(j) =/= 0.U)
            {
                //SynthesizePrintf("PassThru(%d)(%d) %d\n", i.U, j.U, PassThru(i)(j))
            }
            
            routing(i+1).inputs(j+nAddUnits+nMultUnits) := PassThru(i)(j)

        }

    }
    

    io.output := routing(nLayers).outputs(nAddUnits+nMultUnits)(0) // output will always come from last pass thru --> some inefficiency here
}

