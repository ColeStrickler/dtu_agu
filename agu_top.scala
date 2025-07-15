package freechips.rocketchip.subsystem.`RME-Firesim`



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
/* 
    In this module we do the following:

    1. Initialize the config registers that are written to by the DTL spec
    2. Instrument the finite state machine that will control the routing 
    3. 

*/


case class AGUParams
(
    maxOutStatements: Int,
    nLayers: Int,
    nAdd : Int,
    nMult : Int,
    nPassthru : Int,
    regAddress : Int,
    controlBeatBytes : Int
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
            val offset = Decoupled(Output(UInt(32.W))) // output offset calculation
        })

        /*
            General configuration 
        */
        val nOutStatements = RegInit(0.U(log2Ceil(params.maxOutStatements).W))
        val usedOutStatements = RegInit(0.U(log2Ceil(params.maxOutStatements).W))
        
        /* 
            Routing configuration
        
        
            We allow a byte width each to simplify writing to these "Cells"
        */
        val RoutingConfig = RegInit(VecInit(Seq.fill(params.maxOutStatements)(
                                        VecInit(Seq.fill(params.nLayers)(
                                            VecInit(Seq.fill(totalFuncUnits)(0.U(8.W))))))))


        var cell = 0
        val mmregBuf = ArrayBuffer[(Int, Seq[RegField])]()
        for (i <- 0 until params.maxOutStatements) {
            for (j <- 0 until params.nLayers) {
                for (k <- 0 until totalFuncUnits) {
                mmregBuf += (cell -> Seq(RegField(8, RoutingConfig(i)(j)(k), RegFieldDesc("enableRME", "enableRME"))))
                cell += 1
                }
            }
        }

        val mmreg: Seq[(Int, Seq[RegField])] = mmregBuf.toSeq
        ctlnode.regmap(mmreg: _*)

        /*
            Control Plane
        */
        val currentOutStatement = RegInit(0.U(log2Ceil(params.maxOutStatements))) // which out statement do we send down the pipeline
        val readyNewGen = Wire(Bool()) // will we send a new outstatement down the pipeline
        val outStatementAtLayer = RegInit(VecInit(Seq.fill(params.nLayers)(0.U(log2Ceil(params.maxOutStatements).W))))
        val validAtLayer = RegInit(VecInit(Seq.fill(params.nLayers)(false.B)))

        // shift in valid signal
        validAtLayer := readyNewGen +: validAtLayer.init
        // shift in current outStatement
        outStatementAtLayer := currentOutStatement +: outStatementAtLayer.init


        // increment currentOutStatement if it was used. If at the last one, go back to the first
        currentOutStatement := Mux(readyNewGen, Mux(currentOutStatement === usedOutStatements-1.U, 0.U, currentOutStatement+1.U), currentOutStatement)


        val RoutingConfigOut = Wire(
            Vec(params.nLayers, Vec(totalFuncUnits, UInt(8.W)))
        )
        for (i <- 0 until params.nLayers)
        {
            RoutingConfigOut(i) := RoutingConfig(outStatementAtLayer(i))(i)
        }
    }
}