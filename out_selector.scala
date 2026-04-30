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
        val conditionedIndices = Input(Vec(LoopIndexCount, UInt(bitwidth.W)))
        val condCode = Input(UInt(log2Ceil(10).W)) // restrict to two cond
        val outOffset  = Input(UInt(log2Ceil(MaxOutStatements).W))

        val outStatement = Output(UInt(log2Ceil(MaxOutStatements).W))
    })

    
    object COND extends ChiselEnum {
        val DISABLE, ISEVEN, SWITCH, LT, GT, LTE, GTE, EDGE, EDGE2OR, EDGE2AND, PAD = Value
    }

    def isEven(idx: UInt) : Bool = {
        !idx(0)
    }


    def getCondIdx(i: Int): UInt = {
        io.loopIndices(io.conditionedIndices(i))
    }

    val conditionedIdx = getCondIdx(0) // 
    val conditionedIdx2 = getCondIdx(1)

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
               // SynthesizePrintf("isEven! %d\n", io.outOffset)
                io.outStatement :=  io.outOffset
            }
            .otherwise
            {
                io.outStatement := io.outStatementsPerCond + io.outOffset
              //  SynthesizePrintf("NotisEven! %d\n", io.outStatementsPerCond + io.outOffset)
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
            val bounds = io.loopBounds(conditionedIdx) - 1.U
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
            val bounds1 = io.loopBounds(conditionedIdx) - 1.U
            val bounds1_cond = conditionedIdx === 0.U || conditionedIdx === bounds1


            val bounds2 = io.loopBounds(conditionedIdx2) - 1.U
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
            val bounds1 = io.loopBounds(conditionedIdx) - 1.U
            val bounds1_cond = conditionedIdx === 0.U || conditionedIdx === bounds1


            val bounds2 = io.loopBounds(conditionedIdx2) - 1.U
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

        is (COND.PAD)
        {
            val h = getCondIdx(0)
            val kh = getCondIdx(1)
            val w = getCondIdx(2)
            val kw = getCondIdx(3)
            //SynthesizePrintf("%d, %d, %d, %d\n", )

            val h_bound = io.loopBounds(io.conditionedIndices(0))
            val w_bound = io.loopBounds(io.conditionedIndices(2))

            val h_comb = h+kh
            val w_comb = w+kw
            val h_z = h_comb === 0.U
            val w_z = w_comb === 0.U

            val h_m = (h_comb-1.U) >= h_bound
            val w_m = (w_comb-1.U) >= w_bound

            val doPad = (h_m || w_m || h_z || w_z)
            //SynthesizePrintf("h %d, kh %d, w %d, kw %d\n", h, kh, w, kw)
            //SynthesizePrintf("PAD %d, %d, %d, %d\n", h_m, w_m, h_z, w_z)
            when (doPad)
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