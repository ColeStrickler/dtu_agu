package agu

import chisel3._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.util._
import firrtl.options.TargetDirAnnotation
import midas.targetutils.SynthesizePrintf

case class UnrollUnitIO(bitwidth : Int = 32) extends Bundle
{
    
}


class UnrollUnit() extends Module
{
    val io = IO(new UnrollUnitIO(32))








}