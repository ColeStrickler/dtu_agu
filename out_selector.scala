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
        val loopBounds = Input(Vec(LoopIndexCount, UInt(bitwidth.W)))

        val conditionedIndex = Input(UInt(log2Ceil(LoopIndexCount).W))
        val conditionedIndex2 = Input(UInt(log2Ceil(LoopIndexCount).W))
        val condCode = Input(UInt(log2Ceil(10).W)) // restrict to two cond
        val outOffset  = Input(UInt(log2Ceil(MaxOutStatements).W))

        val outStatement = Output(UInt(log2Ceil(MaxOutStatements).W))
    })

    
    object COND extends ChiselEnum {
        val DISABLE, ISEVEN, SWITCH, LT, GT, LTE, GTE, EDGE, EDGE2OR, EDGE2AND = Value
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


        is (COND.EDGE)
        {
            val bounds = io.loopBounds(io.conditionedIndex) - 1.U
            when (conditionedIdx === 0.U || conditionedIdx === bounds)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }
        }


        is (COND.EDGE2OR)
        {
            val bounds1 = io.loopBounds(io.conditionedIndex) - 1.U
            val bounds1_cond = conditionedIdx === 0.U || conditionedIdx === bounds1


            val bounds2 = io.loopBounds(io.conditionedIndex2) - 1.U
            val bounds2_cond = conditionedIdx2 === 0.U || conditionedIdx2 === bounds2


            when (bounds1_cond || bounds2_cond)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }

        }


        is (COND.EDGE2AND)
        {
            val bounds1 = io.loopBounds(io.conditionedIndex) - 1.U
            val bounds1_cond = conditionedIdx === 0.U || conditionedIdx === bounds1


            val bounds2 = io.loopBounds(io.conditionedIndex2) - 1.U
            val bounds2_cond = conditionedIdx2 === 0.U || conditionedIdx2 === bounds2


            when (bounds1_cond && bounds2_cond)
            {
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
            }
        }


    }


}