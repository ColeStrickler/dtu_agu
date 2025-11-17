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


case class AGUParams
(
    maxOutStatements: Int = 1,
    nLayers: Int = 5,
    nAdd : Int = 4,
    nMult : Int = 4,
    bitwidth : Int = 32,
    nPassthru : Int = 4,
    nLoopRegs : Int = 6,
    nConstRegs : Int = 5,
    nConstArray : Int = 1,
    nConstArraySize : Int = 32,
    regAddress : Int = 0x4000000,
    controlBeatBytes : Int = 8,
    maxVarOutputs : Int = 2,
)


class AGUTop(params : AGUParams, config: Int = 0)(implicit p: Parameters) extends LazyModule 
{


    /*
        Compute parameterization. Will be used elsewhere
    */
    val NULL_ROUTE : Int = {
        val totalFuncUnits = params.nAdd + params.nMult + params.nPassthru
        val bits = log2Ceil(totalFuncUnits)
        if (math.pow(2, bits)-1 == totalFuncUnits)
            (math.pow(2,bits+1)-1).toInt
        else
            (math.pow(2, bits)-1).toInt
    }
    val routerRegBitsNeeded = log2Ceil(NULL_ROUTE)
    def alignTo8(x: Int): Int = ((x + 7) / 8) * 8

    val CacheLineSizeBytes = 64.U // bytes
    val device = new SimpleDevice("dtlagu",Seq("ku-csl,dtlagu"))

    val ctlnode = TLRegisterNode(
        address     = Seq(AddressSet(params.regAddress + 0x1000*config, 0xfff)),
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

        when (io.reqIO.offsetAddrFromBase.fire)
        {
            SynthesizePrintf("[AGUTop] io.reqIO.offsetAddrFromBase.fire 0x%x\n", io.reqIO.offsetAddrFromBase.bits)
        }

        /*
            General configuration 
        */
        val nOutStatements = RegInit(1.U(log2Ceil(params.maxOutStatements+1).W))
        val usedOutStatements = RegInit(0.U(log2Ceil(params.maxOutStatements+1).W))
        val usedForLoops = RegInit(0.U(log2Ceil(params.nLoopRegs).W))
        val config_reset = RegInit(false.B)

        val magic_reg_M = RegInit(VecInit(Seq.fill(params.nLoopRegs)(0.U(64.W))))
        val magic_reg_S = RegInit(VecInit(Seq.fill(params.nLoopRegs)(0.U(32.W))))
        val magic_reg_AddInidicator = RegInit(VecInit(Seq.fill(params.nLoopRegs)(false.B)))

        
        assert(params.nConstArraySize <= 255) // we aren't allowing for more rn
        val constArrayValues = RegInit(VecInit(Seq.fill(params.nConstArray)(VecInit(Seq.fill(params.nConstArraySize)(0.U(32.W))))))
        val constArrayIndexSelector = RegInit(VecInit(Seq.fill(params.nConstArray)(0.U(8.W))))
        val constArraySelected = WireInit(VecInit(Seq.fill(params.nConstArray)(0.U(32.W))))
        
        




        /*
            1.  We need to set up writesfrom the unroll unit, [x]
            2.  Pass in data size from outside, [x]
            3.  We are not currently passing in an offset address, just boolean doGen. [x]
                We need to pass in the offset for computation. Preferably with the RME base [x]

            3.  Sync up the io.reqIO.doGen with the unroll unit in[x]
            4.  Sync up the unroll unit.out.valid with datapath doGen [x]
            5.  Write unroll unit out registers to for loop registers [x]
            6.  We only need one input doGen from Requestor now, we need to control the rest from here. [x]
                Before we are inputting 1 doGen from requestor for each instance of 
            7.  possibly get rid of for loop register writes in the compiler --> actually i think we just set them to zero, which is fine
            8.  add "usedForLoops" write to compiler [x]
            9.  add magicReg writes to compiler [x]
            10. need to implement decoupled IO interface control for the unroll unit [x]
                unroll_unit.io.AddressIn := io.reqIO.offset
        */



        /* 
            Routing configuration
        
        
            We allow a byte width each to simplify writing to these "Cells"


            Initialize to 255 so they are ignored by router



            We can cut the size of this in half if we set width of each register to log2ceil(totalFucncUnits)
            I also believe we can cut maxVarOutputs to 2
        */
        val RoutingConfig = RegInit(VecInit(Seq.fill(params.maxOutStatements)(
                                        VecInit(Seq.fill(params.nLayers+1)(
                                            VecInit(Seq.fill(totalFuncUnits)(VecInit(Seq.fill(params.maxVarOutputs)(NULL_ROUTE.U(routerRegBitsNeeded.W))))))))))
        


        

        
        



        /*
            [Creating MMIO config]
        */
        var cell = 0
        val bytesPerCell = 1
        val totalRegSize = 0x1000
        val mmregBuf = ArrayBuffer[(Int, Seq[RegField])]()
        for (i <- 0 until params.maxOutStatements) {
            for (j <- 0 until params.nLayers+1) {
                for (k <- 0 until totalFuncUnits) {
                    for (l <- 0 until params.maxVarOutputs)
                    {
                        mmregBuf += (cell -> Seq(RegField(routerRegBitsNeeded, RoutingConfig(i)(j)(k)(l), RegFieldDesc("agurouting", "agurouting"))))
                        cell += 1
                    }
                }
            }
        }
        
        val bytesUsedRouting = bytesPerCell*params.maxOutStatements*(params.nLayers+1)*totalFuncUnits*params.maxVarOutputs
        val reg_reset = ((0xf00) -> Seq(RegField(1, config_reset, RegFieldDesc("reset", "reset"))))
        val usedOutStatementsReg = ((0xf01) -> Seq(RegField(8, usedOutStatements, RegFieldDesc("nOutStatements", "nOutStatements"))))
        val usedForLoopsReg = ((0xf02) -> Seq(RegField(8, usedForLoops, RegFieldDesc("nForLoops", "nForLoops"))))
        mmregBuf += reg_reset
        mmregBuf += usedOutStatementsReg
        mmregBuf += usedForLoopsReg
        



        /*
            We need an nLoopRegs used register to use with the unroll_unit?
        */
        val LoopRegs =      RegInit(VecInit(Seq.fill(params.nLoopRegs)(0.U(params.bitwidth.W))))
        val LoopIncRegs =   RegInit(VecInit(Seq.fill(params.nLoopRegs)(0.U(params.bitwidth.W))))
        val ConstantRegs =  RegInit(VecInit(Seq.fill(params.nConstRegs)(0.U(params.bitwidth.W))))
        val StrideRegs =    RegInit(VecInit(Seq.fill(params.nLoopRegs)(0.U(32.W))))

        // this will make a mux tree that will use the loop index to bring in the value
        for (i <- 0 until params.nConstArray)
        {
            constArraySelected(i) := constArrayValues(i)(LoopRegs(constArrayIndexSelector(i)))
        }




        /*
            probably want to do this better for power efficiency later on, maybe trigger on a write function to mmio register
            but this will suffice for now
            may even want to map this in mmio? and handle writes with compiler, idk

            We definitely want to put this into compiler
            --> or just use registers? this should be fine
            --> we made a change here, haven't yet made sure it will work
        */
        val stride = RegInit(VecInit(Seq.fill(params.nLoopRegs)(0.U(32.W))))
        stride(0) := usedOutStatements
        for (i <- 1 until params.nLoopRegs)
        {
            stride(i) := stride(i-1) * LoopIncRegs(i-1)
        }

        
        val unroll_unit = Module(new UnrollUnit(params))

        /*
            Need to think if we want to do this the other way around/backwards (not that it matters for functionality)
            for readability
        */
        for (i <- 0 until params.nLoopRegs)
        {
            unroll_unit.io.MagicNumbers(i).M :=                 magic_reg_M(i)
            unroll_unit.io.MagicNumbers(i).s :=                 magic_reg_S(i)
            unroll_unit.io.MagicNumbers(i).add_indicator :=     magic_reg_AddInidicator(i).asBool
            unroll_unit.io.MagicNumbers(i).stride :=            stride(params.nLoopRegs-1-i)
            
        }

        val datapath_active = RegInit(false.B)
        
        unroll_unit.io.DataSize := io.reqIO.data_size
        unroll_unit.io.nForLoopsActive := usedForLoops
        // Feed incoming addresses into the unroll unit
        unroll_unit.io.AddressIn.bits  := io.reqIO.offsetAddrFromBase.bits
        unroll_unit.io.AddressIn.valid := io.reqIO.offsetAddrFromBase.valid
        io.reqIO.offsetAddrFromBase.ready := unroll_unit.io.AddressIn.ready

        unroll_unit.io.UnrolledInit.ready := !datapath_active





        /* 
            We need to write the init to the for loops registers

            The last index from unroll_unit.io.UnrolledInit.Reg is the first and innermost for-loop

        */
        when (unroll_unit.io.UnrolledInit.fire)
        {
            // we wire backwards
            for (i <- (params.nLoopRegs - 1) to 0 by -1)
            {
                LoopRegs(params.nLoopRegs - i - 1) := unroll_unit.io.UnrolledInit.bits.RegInitValues(i)
            }
            
        }


        // we will give each of these 32 bits for now



        val bytesPerLoop =      params.bitwidth/8
        val bytesPerConst =     params.bitwidth/8
        val bytesUsedForLoop =  bytesPerLoop * params.nLoopRegs 
        val bytesUsedIncLoop =  bytesPerLoop * params.nLoopRegs 
        val offsetConstRegs =   bytesUsedRouting+bytesUsedForLoop+bytesUsedIncLoop
        val bytesUsedConst =    params.nConstRegs*bytesPerConst
        val offsetConstArray = offsetConstRegs+bytesUsedConst
        val bytesPerConstArray = (params.nConstArraySize*4 + 8) 
        val bytesUsedConstArray = params.nConstArray*bytesPerConstArray// # array * (nregister + selector)
        val offsetMagicRegs =   alignTo8(offsetConstRegs+bytesUsedConst+bytesUsedConstArray)
        val bytesPerMagic =     16 // m(4), s(4), add_indicator(4) --> we give whole word even tho it is just bool
        assert(offsetMagicRegs < 0xf00, "offset magic regs < 0xf00")
        for (i <- 0 until params.nLoopRegs) // Loop registers immediately after routing cells
        {
            mmregBuf += ((bytesUsedRouting+(i*bytesPerLoop) -> Seq(RegField(params.bitwidth, LoopRegs(i), RegFieldDesc("forloop", "forloop")))))
            mmregBuf += ((bytesUsedRouting+bytesUsedForLoop+(i*bytesPerLoop) -> Seq(RegField(params.bitwidth, LoopIncRegs(i), RegFieldDesc("incloop", "incloop")))))
        }
        for (i <- 0 until params.nConstRegs)
        {
            mmregBuf += ((offsetConstRegs + i*bytesPerConst) -> Seq(RegField(params.bitwidth, ConstantRegs(i), RegFieldDesc("constreg", "constreg"))))
        }

        for (i <- 0 until params.nConstArray)
        {
            for (j <- 0 until params.nConstArraySize)
            {
                mmregBuf += (((offsetConstArray + i*bytesPerConstArray + j*4) ->Seq(RegField(32, constArrayValues(i)(j), RegFieldDesc("constArrVal", "constArrVal")))))
            }
            mmregBuf += (((offsetConstArray + i*bytesPerConstArray + params.nConstArraySize*4) ->Seq(RegField(8, constArrayIndexSelector(i), RegFieldDesc("constArrIndexSel", "constArrIndexSel")))))
        }

        for (i <- 0 until params.nLoopRegs)
        {
            mmregBuf += (((offsetMagicRegs + i*bytesPerMagic)  -> Seq(RegField(64, magic_reg_M(i), RegFieldDesc("magic_m", "magic_m")))))
            mmregBuf += (((offsetMagicRegs + i*bytesPerMagic + 0x8)  -> Seq(RegField(32, magic_reg_S(i), RegFieldDesc("magic_s", "magic_s")))))
            mmregBuf += (((offsetMagicRegs + i*bytesPerMagic + 0xc)  -> Seq(RegField(1, magic_reg_AddInidicator(i), RegFieldDesc("magic_addIndicator", "magic_addIndicator")))))
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
        val currentOutStatement = RegInit(0.U(8.W)) // which out statement do we send down the pipeline
        val readyNewGen = Wire(Bool()) // will we send a new outstatement down the pipeline
        val outStatementAtLayer = RegInit(VecInit(Seq.fill(params.nLayers+1)(0.U(log2Ceil(params.maxOutStatements+1).W))))
        val validAtLayer = RegInit(VecInit(Seq.fill(params.nLayers+1)(false.B)))
        val stallLayers = Wire(Vec(params.nLayers+1, Bool()))
        val lastOutStmt = Wire(Bool())
        val toGen = RegInit(0.U(8.W))
        val sentForGen = RegInit(0.U(8.W))

        val shift_divider = Module(new ShiftDivider(8)) // save timing latency, since we only allow certain sizes
        shift_divider.io.addr_in := CacheLineSizeBytes
        shift_divider.io.data_size.bits := io.reqIO.data_size
        shift_divider.io.data_size.valid := datapath_active
        toGen := shift_divider.io.quotient
        //assert(shift_divider.io.remainder === 0.U)

        // if we pass in the last one for gen, we can take anbother
        datapath_active := Mux(datapath_active, !(sentForGen === (toGen-1.U) && readyNewGen), unroll_unit.io.UnrolledInit.fire)  
        readyNewGen := datapath_active && !stallLayers(0)
        sentForGen := Mux(datapath_active, sentForGen + readyNewGen, 0.U) // dataflow architecture should take input every time if not stalled


        // shift in valid signal
        //validAtLayer := readyNewGen +: validAtLayer.init // we will shift in falses here by definition

        validAtLayer(0) := Mux(stallLayers(0), validAtLayer(0), readyNewGen)
        for (i <- 1 until validAtLayer.length)
        {
            validAtLayer(i) := Mux(stallLayers(i), validAtLayer(i), validAtLayer(i-1))
        }
        


        // shift in current outStatement
        outStatementAtLayer := currentOutStatement +: outStatementAtLayer.init


        // increment currentOutStatement if it was used. If at the last one, go back to the first
        

        lastOutStmt := ((currentOutStatement + 1.U) % usedOutStatements) === 0.U

        when(unroll_unit.io.UnrolledInit.fire)
        {
            currentOutStatement := unroll_unit.io.UnrolledInit.bits.OutStmtStart
        }
        .elsewhen (readyNewGen) {
            currentOutStatement := (currentOutStatement + 1.U) % usedOutStatements
        }


        when (readyNewGen)
        {
           // SynthesizePrintf("CurrentOutStatement %d, usedOutStatement %d\n", currentOutStatement, usedOutStatements)
            for (i <- 0 until params.nLoopRegs)
            {
             //   SynthesizePrintf("loopReg(%d) %d\n", i.U, LoopRegs(i))
            }
        }
        

        val RoutingConfigOut = Wire(
            Vec(params.nLayers+1, Vec(totalFuncUnits, Vec(params.maxVarOutputs, UInt(8.W))))
        )
        for (i <- 0 until params.nLayers+1)
        {
            RoutingConfigOut(i) := RoutingConfig(outStatementAtLayer(i))(i)       
        }


        when (config_reset)
        {
           // SynthesizePrintf("configReset=true\n")
            // zero all routing config
            RoutingConfig.foreach(i => i.foreach(j => j.foreach(k => k.foreach(l => l := NULL_ROUTE.U))))// ignore value

            
            // invalidate all layers
            validAtLayer.foreach(f => f := false.B)    
        }


        val dpath = Module(new AGUDatapath(params, params.nLoopRegs, params.nConstRegs, params.nLayers, params.nMult, params.nAdd, params.nPassthru, params.maxVarOutputs))
        

        dpath.io.doGen := readyNewGen
        dpath.io.reset := config_reset
        dpath.io.ConstantRegsIn := ConstantRegs
        dpath.io.LoopIncRegsIn := LoopIncRegs
        dpath.io.LoopRegsIn := LoopRegs
        dpath.io.ConstantArrayRegIn := constArraySelected





        // update state registers
        val isIncLoopReg =  Seq.fill(params.nLoopRegs)(WireInit(false.B))
        isIncLoopReg(0) := readyNewGen 
        when (readyNewGen && lastOutStmt)
        {
            LoopRegs(0) := Mux(LoopRegs(0) === LoopIncRegs(0) - 1.U, 0.U, LoopRegs(0)+1.U)
            for (i <- 1 until params.nLoopRegs)
            {
                isIncLoopReg(i) := isIncLoopReg(i-1) && (LoopRegs(i-1) + 1.U === LoopIncRegs(i-1))
                LoopRegs(i) := Mux(isIncLoopReg(i), Mux(LoopRegs(i) + 1.U === LoopIncRegs(i), 0.U, LoopRegs(i)+1.U), LoopRegs(i))
            }
        }
        when (config_reset)
        {
            LoopRegs.foreach(r => r := 0.U)
            LoopIncRegs.foreach(r => r := 0.U)
            ConstantRegs.foreach(r => r := 0.U)
            magic_reg_AddInidicator.foreach(r => r:= false.B)
            magic_reg_M.foreach(r => r := 0.U)
            magic_reg_S.foreach(r => r := 0.U)
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


        when (stallLayers(stallLayers.length-1))
        {
            SynthesizePrintf("STALL AGU DATAPATH\n")
        }

        /*
            We moved this control to datapath_active
        */
        //io.reqIO.doGen.ready := !stallLayers(0) // accept new request if we are not stalled at layer 0


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