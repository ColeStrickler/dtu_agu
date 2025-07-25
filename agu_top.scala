package agu


import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import freechips.rocketchip
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import scala.collection.mutable.ArrayBuffer
import subsystem.rme.RequestorAGUPort
import mainargs.TokensReader.Constant
/* 
    In this module we do the following:

    1. Initialize the config registers that are written to by the DTL spec
    2. Instrument the finite state machine that will control the routing 
    3. 

*/

object Constants {
  final val NULL_ROUTE = 255
}



case class AGUParams
(
    maxOutStatements: Int = 8,
    nLayers: Int = 5,
    nAdd : Int = 4,
    nMult : Int = 4,
    bitwidth : Int = 32,
    nPassthru : Int = 4,
    nLoopRegs : Int = 6,
    nConstRegs : Int = 6,
    regAddress : Int = 0x4000000,
    controlBeatBytes : Int = 8,
    maxVarOutputs : Int = 4
)



class AGUTop(params : AGUParams)(implicit p: Parameters) extends LazyModule 
{

    val device = new SimpleDevice("dtlagu",Seq("ku-csl,dtlagu"))

    val ctlnode = TLRegisterNode(
        address     = Seq(AddressSet(params.regAddress, 0xfff)),
        device      = device,
        concurrency = 1, // Only one flush at a time (else need to track who answers)
        beatBytes   = params.controlBeatBytes)


    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {
        val totalFuncUnits = params.nAdd+params.nMult+params.nPassthru
        val io = IO(new Bundle {
            val reqIO = Flipped(new RequestorAGUPort)
            // config out to datapath

        })

        /*
            General configuration 
        */
        val nOutStatements = RegInit(0.U(log2Ceil(params.maxOutStatements).W))
        val usedOutStatements = RegInit(0.U(log2Ceil(params.maxOutStatements).W))
        val config_reset = RegInit(false.B)
        



        /* 
            Routing configuration
        
        
            We allow a byte width each to simplify writing to these "Cells"


            Initialize to 255 so they are ignored by router
        */
        val RoutingConfig = RegInit(VecInit(Seq.fill(params.maxOutStatements)(
                                        VecInit(Seq.fill(params.nLayers+1)(
                                            VecInit(Seq.fill(totalFuncUnits)(VecInit(Seq.fill(params.maxVarOutputs)(Constants.NULL_ROUTE.U(8.W))))))))))
        


        

        
        



        /*
            [Creating MMIO config]
        */
        var cell = 0
        val bytesPerCell = 1
        val mmregBuf = ArrayBuffer[(Int, Seq[RegField])]()
        for (i <- 0 until params.maxOutStatements) {
            for (j <- 0 until params.nLayers+1) {
                for (k <- 0 until totalFuncUnits) {
                    for (l <- 0 until params.maxVarOutputs)
                    {
                        mmregBuf += (cell -> Seq(RegField(bytesPerCell*8, RoutingConfig(i)(j)(k)(l), RegFieldDesc("agurouting", "agurouting"))))
                        cell += 1
                    }
                }
            }
        }
        
        val bytesUsedRouting = bytesPerCell*params.maxOutStatements*(params.nLayers+1)*totalFuncUnits*params.maxVarOutputs
        val reg_reset = ((0xf00) -> Seq(RegField(1, config_reset, RegFieldDesc("reset", "reset"))))
        val usedOutStatementsReg = ((0xf01) -> Seq(RegField(8, usedOutStatements, RegFieldDesc("nOutStatements", "nOutStatements"))))
        mmregBuf += reg_reset
        mmregBuf += usedOutStatementsReg


        val LoopRegs = Seq.fill(params.nLoopRegs)(RegInit(0.U(params.bitwidth.W)))
        val LoopIncRegs = Seq.fill(params.nLoopRegs)(RegInit(0.U(params.bitwidth.W)))
        val ConstantRegs = Seq.fill(params.nConstRegs)(RegInit(0.U(params.bitwidth.W)))
        // we will give each of these 32 bits for now
        when (io.reqIO.doGen.fire)
    {
        for (i <- 0 until params.nLoopRegs)
        {
            //SynthesizePrintf("loopreg(%d) %d\n", i.U, LoopRegs(i))
            //SynthesizePrintf("loopincreg(%d) %d\n", i.U, LoopIncRegs(i))
        }

        for (i <- 0 until params.nConstRegs)
        {
            //SynthesizePrintf("constReg(%d) %d\n", i.U, ConstantRegs(i))
        }
    }





        val bytesPerLoop = params.bitwidth/8
        val bytesPerConst = params.bitwidth/8
        val bytesUsedForLoop = bytesPerLoop * params.nLoopRegs 
        val bytesUsedIncLoop = bytesPerLoop * params.nLoopRegs 
        val offsetConstRegs = bytesUsedRouting+bytesUsedForLoop+bytesUsedIncLoop
        assert(offsetConstRegs < 0xf00)
        for (i <- 0 until params.nLoopRegs) // Loop registers immediately after routing cells
        {
            mmregBuf += ((bytesUsedRouting+(i*bytesPerLoop) -> Seq(RegField(params.bitwidth, LoopRegs(i), RegFieldDesc("forloop", "forloop")))))
            mmregBuf += ((bytesUsedRouting+bytesUsedForLoop+(i*bytesPerLoop) -> Seq(RegField(params.bitwidth, LoopIncRegs(i), RegFieldDesc("incloop", "incloop")))))
        }
        for (i <- 0 until params.nConstRegs)
        {
            mmregBuf += ((offsetConstRegs + i*bytesPerConst) -> Seq(RegField(params.bitwidth, ConstantRegs(i), RegFieldDesc("constreg", "constreg"))))
        }

        val mmreg: Seq[(Int, Seq[RegField])] = mmregBuf.toSeq
        mmregBuf.foreach { case (addr, fields) =>
            println(f"MMIO reg @ 0x$addr%x : ${fields.map(_.desc.get.name).mkString(", ")}")
        }

        ctlnode.regmap(mmreg: _*)

        /*
            [Creating MMIO config] --> END
        */














        /*
            [Control Plane]
        */
        val currentOutStatement = RegInit(0.U(log2Ceil(params.maxOutStatements))) // which out statement do we send down the pipeline
        val readyNewGen = Wire(Bool()) // will we send a new outstatement down the pipeline
        val outStatementAtLayer = RegInit(VecInit(Seq.fill(params.nLayers+1)(0.U(log2Ceil(params.maxOutStatements).W))))
        val validAtLayer = RegInit(VecInit(Seq.fill(params.nLayers+1)(false.B)))
        val stallLayers = Wire(Vec(params.nLayers+1, Bool()))
        readyNewGen := io.reqIO.doGen.fire && io.reqIO.doGen.bits && !stallLayers(0)



        // shift in valid signal
        validAtLayer := readyNewGen +: validAtLayer.init // we will shift in falses here by definition
        // shift in current outStatement
        outStatementAtLayer := currentOutStatement +: outStatementAtLayer.init


        // increment currentOutStatement if it was used. If at the last one, go back to the first
        currentOutStatement := Mux(readyNewGen, Mux(currentOutStatement === usedOutStatements-1.U, 0.U, currentOutStatement+1.U), currentOutStatement)


        val RoutingConfigOut = Wire(
            Vec(params.nLayers+1, Vec(totalFuncUnits, Vec(params.maxVarOutputs, UInt(8.W))))
        )
        for (i <- 0 until params.nLayers+1)
        {
            RoutingConfigOut(i) := RoutingConfig(outStatementAtLayer(i))(i)
            when (io.reqIO.doGen.fire)
            {
                for (x <- 0 until (totalFuncUnits)) {
                    for (y <- 0 until params.maxVarOutputs) {
                       // SynthesizePrintf("[AGUTop] Layer %d Routing input %d, output index %d: %d\n", i.U, x.U, y.U, RoutingConfigOut(i)(x)(y))
                    }
                }
               // SynthesizePrintf("routingconfigout(%d) %d\n", i.U, RoutingConfigOut(i).asUInt)
            }
            
        }


        when (config_reset)
        {
            SynthesizePrintf("configReset=true\n")
            // zero all routing config
            RoutingConfig.foreach(i => i.foreach(j => j.foreach(k => k.foreach(l => l := Constants.NULL_ROUTE.U))))// ignore value

            
            // invalidate all layers
            validAtLayer.foreach(f => f := false.B)    
        }


        val dpath = Module(new AGUDatapath(params.nLoopRegs, params.nConstRegs, params.nLayers, params.nMult, params.nAdd, params.nPassthru, params.maxVarOutputs))
        

        dpath.io.doGen := readyNewGen
        dpath.io.reset := config_reset
        dpath.io.ConstantRegsIn := ConstantRegs
        dpath.io.LoopIncRegsIn := LoopIncRegs
        dpath.io.LoopRegsIn := LoopRegs



        // update state registers
        when (readyNewGen)
        {
            LoopRegs(0) := Mux(LoopRegs(0) === LoopIncRegs(0) - 1.U, 0.U, LoopRegs(0)+1.U)
            for (i <- 1 until params.nLoopRegs)
            {
                LoopRegs(i) := Mux(LoopRegs(i-1) + 1.U === LoopIncRegs(i-1), Mux(LoopRegs(i) + 1.U === LoopIncRegs(i), 0.U, LoopRegs(i)+1.U), LoopRegs(i))
            }
        }
        when (config_reset)
        {
            LoopRegs.foreach(r => r := 0.U)
            LoopIncRegs.foreach(r => r := 0.U)
            ConstantRegs.foreach(r => r := 0.U)
        }







        /*
            [LAYER STALL CONTROL]
        */
        
        /* 
            Stall last layer if there is valid data there and we do not fire it out
        */
        stallLayers(stallLayers.length-1) := (!io.reqIO.offset.ready && validAtLayer(validAtLayer.length-1)) // stall last layer 
        for (i <- (stallLayers.length-2) to 0 by -1)
        {
            stallLayers(i) := stallLayers(i+1) && validAtLayer(i)
        }
        dpath.io.StallLayer := stallLayers
        io.reqIO.doGen.ready := !stallLayers(0) // accept new request if we are not stalled at layer 0


        io.reqIO.offset.valid := validAtLayer(validAtLayer.length-1) && !stallLayers(stallLayers.length-1) 
        io.reqIO.offset.bits := dpath.io.output


        /*
            [MAP ROUTING CONFIG TO EACH LAYER]
        */
        dpath.io.RoutingConfigIn.zipWithIndex.foreach { case (layer, i) =>
            layer := RoutingConfigOut(i)
        }

    }

}