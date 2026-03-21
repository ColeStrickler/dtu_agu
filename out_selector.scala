package agu

import chisel3._
import chisel3.util._
import midas.targetutils.SynthesizePrintf




class OutSelector(LoopIndexCount: Int, MaxOutStatements: Int, bitwidth: Int) extends Module
{
    val io = IO(new Bundle{
        val usedOutStatements = Input(UInt(log2Ceil(MaxOutStatements).W))
        val outStatementsPerCond = Input(UInt(log2Ceil(MaxOutStatements).W))
        val loopIndices = Input(Vec(LoopIndexCount, UInt(bitwidth.W)))

        val conditionedIndex = Input(UInt(log2Ceil(LoopIndexCount).W))
        val useConditional = Input(Bool())
        val useEvenCond = Input(Bool()) // restrict to two cond
        val outOffset  = Input(UInt(log2Ceil(MaxOutStatements).W))

        val outStatement = Output(UInt(log2Ceil(MaxOutStatements).W))
    })



    def isEven(idx: UInt) : Bool = {
        !idx(0)
    }

    val conditionedIdx = io.loopIndices(io.conditionedIndex)

    for (j <- 0 until io.loopIndices.length)
    {
       // SynthesizePrintf("loop index %d conditioned index %d, loopIdxVal %d\n", j.U, io.conditionedIndex, io.loopIndices(j))
    }

    

    when (!io.useConditional)
    {
        io.outStatement := io.outOffset
    }
    .elsewhen (io.useEvenCond)
    {
        when(isEven(conditionedIdx))
        {
            SynthesizePrintf("isEven! %d\n", io.outOffset)
            io.outStatement :=  io.outOffset
        }
        .otherwise
        {
            io.outStatement := io.outStatementsPerCond + io.outOffset
            SynthesizePrintf("NotisEven! %d\n", io.outStatementsPerCond + io.outOffset)
        }
    }
    .otherwise
    {

        /*
            For example


            switch (j)
            {
                case 0.U:
                    out = 
                    out =
                case 1.U:
                    out = 
                    out = 
            }
        
        */
        io.outStatement := (io.outStatementsPerCond * conditionedIdx) + io.outOffset
    }

}