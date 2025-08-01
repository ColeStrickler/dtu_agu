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


    

    shift_divider.io.addr_in := io.AddressIn.bits
    shift_divider.io.data_size.valid := io.AddressIn.valid // make sure we set up datasize first
    shift_divider.io.data_size.bits := io.DataSize

    val access_num = shift_divider.io.quotient
    when(io.AddressIn.valid)
    {
        /*
            This shouldn't happen since we are asserting only power of 2
        */
        assert(shift_divider.io.remainder === 0.U) 
    }



    val UnrollSegments = VecInit(
        (0 until params.nLoopRegs).map { unitIdx =>
            val unroll_seg = Module(new UnrollSegment32()) 
            unroll_seg.io
        }
    )

    // attach last one to output
    val last = (params.nLoopRegs - 1)
    val singleLoop = (io.nForLoopsActive === 1.U)
    io.UnrolledInit.bits.OutStmtStart := UnrollSegments(last).remainder
    io.UnrolledInit.bits.RegInitValues(last) := UnrollSegments(last).index
    UnrollSegments(last).inValue := Mux(singleLoop, io.AddressIn, UnrollSegments(last - 1).remainder)
    UnrollSegments(last).magic := io.MagicNumbers(last)


    val IndicesValidOut = Wire(Vec(params.nLoopRegs, Bool()))
    val AllIndicesValid = IndicesValidOut.reduce(_ && _)


    /* 
        Until or to zero? are we handling the zero index correctly

    */
    for (i <- (params.nLoopRegs-2) until 0 by -1)
    {
        val isInactive = params.nLoopRegs.U-i.U >= io.nForLoopsActive



        when (io.nForLoopsActive === i.U)
        {
            UnrollSegments(i).inValue.bits := io.AddressIn.bits
            UnrollSegments(i).inValue.valid := io.AddressIn.valid
        }
        .otherwise
        {
            UnrollSegments(i).inValue.bits := UnrollSegments(i+1).remainder
            // currently these valids stay on, may want to shut off after 1 cycle high
            UnrollSegments(i).inValue.valid := UnrollSegments(i+1).index.valid
        }


        /* 
            Pass in magic numbers
        */
        UnrollSegments(i).magic := io.MagicNumbers(i)


        /*
            We need to decide whether we want to hold these values in registers or 
            pass out one at a time.

            I think we should hold the values and then push out all at once
        */
        io.UnrolledInit.bits.RegInitValues(i) := UnrollSegments(i).index.bits


        /* 
            We just set inactives to true
        */
        IndicesValidOut(i) := Mux(isInactive, true.B, UnrollSegments(i).index.valid)

    }

}