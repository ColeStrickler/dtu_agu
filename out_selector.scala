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
        val conditionedIndex2 = Input(UInt(log2Ceil(LoopIndexCount).W))
        val condCode = Input(UInt(log2Ceil(6).W)) // restrict to two cond
        val outOffset  = Input(UInt(log2Ceil(MaxOutStatements).W))

        val outStatement = Output(UInt(log2Ceil(MaxOutStatements).W))
    })

    
    object COND extends ChiselEnum {
        val DISABLE, ISEVEN, SWITCH, LT, GT, LTE, GTE = Value
    }

    def isEven(idx: UInt) : Bool = {
        !idx(0)
    }

    val conditionedIdx = io.loopIndices(io.conditionedIndex)
    val conditionedIdx2 = io.loopIndices(io.conditionedIndex2)

    for (j <- 0 until io.loopIndices.length)
    {
       // SynthesizePrintf("loop index %d conditioned index %d, loopIdxVal %d\n", j.U, io.conditionedIndex, io.loopIndices(j))
    }


    val condCode = io.condCode.asTypeOf(COND())
    io.outStatement := 0.U // default
    switch(condCode)
    {
        is (COND.DISABLE)
        {
            io.outStatement := io.outOffset
        }


        is (COND.ISEVEN)
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


        is (COND.SWITCH)
        {
            io.outStatement := (io.outStatementsPerCond * conditionedIdx) + io.outOffset
        }


        is (COND.LT)
        {
            when (conditionedIdx < conditionedIdx2)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }
        }



        is (COND.GT)
        {
            when (conditionedIdx > conditionedIdx2)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }
        }



        is (COND.LTE)
        {
            when (conditionedIdx <= conditionedIdx2)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }
        }
        

        is (COND.GTE)
        {
            when (conditionedIdx >= conditionedIdx2)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }
        }
    }

    //when (!io.useConditional)
    //{
    //    io.outStatement := io.outOffset
    //}
    //.elsewhen (io.useEvenCond)
    //{
    //    when(isEven(conditionedIdx))
    //    {
    //        SynthesizePrintf("isEven! %d\n", io.outOffset)
    //        io.outStatement :=  io.outOffset
    //    }
    //    .otherwise
    //    {
    //        io.outStatement := io.outStatementsPerCond + io.outOffset
    //        SynthesizePrintf("NotisEven! %d\n", io.outStatementsPerCond + io.outOffset)
    //    }
    //}
    //.otherwise
    //{
//
    //    /*
    //        For example
//
//
    //        switch (j)
    //        {
    //            case 0.U:
    //                out = 
    //                out =
    //            case 1.U:
    //                out = 
    //                out = 
    //        }
    //    
    //    */
    //    io.outStatement := (io.outStatementsPerCond * conditionedIdx) + io.outOffset
    //}

}