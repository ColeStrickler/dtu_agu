package agu

import chisel3._
import chisel3.util._





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



    when (!io.useConditional)
    {
        io.outStatement := io.outOffset
    }
    .elsewhen (io.useEvenCond)
    {
        when(isEven(conditionedIdx))
        {
            io.outStatement :=  io.outOffset
        }
        .otherwise
        {
            io.outStatement := io.outStatementsPerCond + io.outOffset
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