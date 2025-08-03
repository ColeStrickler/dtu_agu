package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf


case class UnrolledInitBundle(params: AGUParams) extends Bundle
{
    val OutStmtStart = Output(UInt(log2Ceil(params.maxOutStatements).W))
    val RegInitValues = Output(Vec(params.nLoopRegs, UInt(params.bitwidth.W)))
}


case class UnrollUnitIO(params: AGUParams) extends Bundle
{
    val AddressIn = Flipped(Decoupled(UInt(params.bitwidth.W))) 
    val nForLoopsActive = Input(UInt(log2Ceil(params.nLoopRegs).W))
    val MagicNumbers = Input(Vec(params.nLoopRegs, MagicNumber(params.bitwidth)))
    val DataSize = Input(UInt(8.W))

    val UnrolledInit = Decoupled(new UnrolledInitBundle(params))
    
}


class UnrollUnit(params: AGUParams) extends Module
{
    val io = IO(new UnrollUnitIO(params))
    val shift_divider = Module(new ShiftDivider(params.bitwidth))

    /*
        We need to mark wehen we can reasonably take input IO
    */
    
    val actively_computing = RegInit(false.B)
    val debug_addr_reg = RegInit(0.U(32.W))
    val debug_div_reg = RegInit(0.U(32.W))
    actively_computing := Mux(actively_computing, !io.UnrolledInit.fire, io.AddressIn.fire)
    io.AddressIn.ready:= !actively_computing

    debug_addr_reg := Mux(io.AddressIn.fire, io.AddressIn.bits, debug_addr_reg)
   


    shift_divider.io.addr_in := io.AddressIn.bits
    shift_divider.io.data_size.valid := io.AddressIn.fire // make sure we set up datasize first
    shift_divider.io.data_size.bits := io.DataSize
    val access_num = shift_divider.io.quotient
    debug_div_reg := Mux(io.AddressIn.fire, access_num, debug_div_reg)

    val entry = params.nLoopRegs.U - io.nForLoopsActive
    when(io.AddressIn.fire)
    {

        SynthesizePrintf("[UnrollUnit] io.AddressIn.fire 0x%x loopsUsed %d, data size %d, ENTRY: %d\n", io.AddressIn.bits, io.nForLoopsActive, io.DataSize, entry)
        /*
            This shouldn't happen since we are asserting only power of 2
        */
        SynthesizePrintf("shift_divider.io.remainder === 0.U %d, remainder: %d\n", shift_divider.io.remainder === 0.U, shift_divider.io.remainder)
    }



    /*
        These units are pipelined, and each perform a step of the unroll algorithm
        using the magic numbers
    */
    val UnrollSegments = VecInit(
        (0 until params.nLoopRegs).map { unitIdx =>
            val unroll_seg = Module(new UnrollSegment32(unitIdx)) 
            unroll_seg.io
        }
    )




    val IndicesValidOut = Wire(Vec(params.nLoopRegs, Bool()))
    val AllIndicesValid = IndicesValidOut.reduce(_ && _)
    /*
        Here we attach the first and last UnrollSegments to their correct inputs/outputs

        The first one (index 0) is only used when all loop registers are used, and can only get input from ShiftDivider
    */
    val last = (params.nLoopRegs - 1)
    val singleLoop = (io.nForLoopsActive === 1.U)
    io.UnrolledInit.bits.OutStmtStart := UnrollSegments(last).remainder
    io.UnrolledInit.bits.RegInitValues(last) := UnrollSegments(last).index.bits
    UnrollSegments(last).inValue.bits := Mux(singleLoop,  access_num, UnrollSegments(last - 1).remainder)
    UnrollSegments(last).inValue.valid := Mux(singleLoop, io.AddressIn.fire, UnrollSegments(last - 1).index.valid)
    UnrollSegments(last).magic := io.MagicNumbers(last)
    UnrollSegments(last).rst := io.UnrolledInit.fire
    IndicesValidOut(last) := UnrollSegments(last).index.valid

    val allLoopsUsed = (io.nForLoopsActive === params.nLoopRegs.U)
    io.UnrolledInit.bits.RegInitValues(0) := UnrollSegments(0).index.bits
    UnrollSegments(0).inValue.bits := access_num
    UnrollSegments(0).inValue.valid := io.AddressIn.fire && allLoopsUsed // only valid when using all loop regs
    UnrollSegments(0).magic := io.MagicNumbers(0)
    UnrollSegments(0).rst := io.UnrolledInit.fire
    IndicesValidOut(0) := Mux(allLoopsUsed, UnrollSegments(0).index.valid, true.B)
    

    
    /* 
        Until or to zero? are we handling the zero index correctly

    */
    for (i <- (params.nLoopRegs-2) until 0 by -1)
    {
        // indicates unused
        val isInactive = params.nLoopRegs.U-i.U >= io.nForLoopsActive

        /*
            where we start the pipeline, this will create dynamic pipeline latency
        */
        when (i.U === entry)
        {
            // we use access num as we need the ShiftDivide preprocessing step done first
            UnrollSegments(i).inValue.bits := access_num 
            UnrollSegments(i).inValue.valid := io.AddressIn.valid
            when(io.AddressIn.valid)
            {
                 SynthesizePrintf("Here\n")
            }
           
        }
        .otherwise
        {
            UnrollSegments(i).inValue.bits := UnrollSegments(i-1).remainder
            // currently these valids stay on, may want to shut off after 1 cycle high
            UnrollSegments(i).inValue.valid := UnrollSegments(i-1).index.valid
        }


        /* 
            Pass in magic numbers
        */
        UnrollSegments(i).magic := io.MagicNumbers(i)


        /* 
            We reset when fully unrolled 
        */
        UnrollSegments(i).rst := io.UnrolledInit.fire

        /*
            We need to decide whether we want to hold these values in registers or 
            pass out one at a time.

            I think we should hold the values and then push out all at once
            UPDATE: this actually may be a bad idea, as this will prevent true pipelining?
                    Need to think about this more deeply

                    but also may not be an issue, as we will already be dealing with downstream
                    latency blocking from filled fetch units and memory request round trip

                    either way I should come up with a non-blocking pipeline design
        */
        io.UnrolledInit.bits.RegInitValues(i) := UnrollSegments(i).index.bits


        /* 
            We just set inactives to true
        */
        IndicesValidOut(i) := Mux(isInactive, true.B, UnrollSegments(i).index.valid)

    }

    when (AllIndicesValid)
    {
        SynthesizePrintf("WorkingAddr 0x%x, Working access_num %d\n", debug_addr_reg, debug_div_reg)
        for (i <- 0 until IndicesValidOut.size)
        {       
            SynthesizePrintf("IndicesValidOut(%d): %d\n", i.U, IndicesValidOut(i))
            SynthesizePrintf("UnrollSegments(%d).index.bits %d\n", i.U, UnrollSegments(i).index.bits)
        }
    }

    io.UnrolledInit.valid := AllIndicesValid

}