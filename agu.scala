package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._


class AGU(nLoopRegs : Int, nConstRegs: Int, nLayers: Int, nMultUnits: Int, nAddUnits: Int) extends Module
{
    val bitwidth = 32

    val io = IO(new Bundle{
      val doGen = Input(Bool())
        
    })

    /*
        We will need to have these compiled for us from source. We can maintain an invariant like we were in the processor model
    */
    val LoopRegs = VecInit(Seq.fill(nLoopRegs)(RegInit(0.U(bitwidth.W))))
    val LoopIncRegs = VecInit(Seq.fill(nLoopRegs)(RegInit(0.U(bitwidth.W))))
    val ConstantRegs = VecInit(Seq.fill(nLoopRegs)(RegInit(0.U(bitwidth.W))))


    val MultLayers = VecInit(Seq.fill(nLayers)(
        VecInit(Seq.fill(nMultUnits)(
            Module(new MultUnit(bitwidth)).io
        ))
    ))

    val AddLayers = VecInit(Seq.fill(nLayers)(
        VecInit(Seq.fill(nAddUnits)(
            Module(new AddUnit(nAddUnits, 4)).io
        ))
    ))

    val routing = VecInit(Seq.fill(nLayers)(
        Module(new LayerRouter(nAddUnits + nMultUnits, nAddUnits + nMultUnits, 4, 32)).io
    ))

    when (io.doGen)
    {
        LoopRegs(0) := Mux(LoopRegs(0) === LoopIncRegs(0), 0.U, LoopRegs(0)+1.U)
        for (i <- 1 until nLoopRegs)
        {
            LoopRegs(i) := Mux(LoopRegs(i-1) === LoopIncRegs(i-1), Mux(LoopRegs(i) === LoopIncRegs(i), 0.U, LoopRegs(i)), LoopRegs(i))
        }
    }

    /*
        Iterate over layers, use routers to route intermediate results
    */

    for (i <- 1 until nLayers)
    {

    }
    


}