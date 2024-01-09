package Chainsaw.ViT

import spinal.lib._
import spinal.core._
import Chainsaw.device._
import MULTMODE._
import DSPPIPELINE._
import Chainsaw._

/** compute AB + DB by DSP48E2
  * @param nameIdx
  * @param latency
  */
case class DspPE(
    nameId: String,
    dataWidth: Int,
    latency: Int,
    multMode: MULTMODE,
    blockSliceConfig: BlockSliceConfig,
    isCascade: Boolean = true
) extends Component {

  require(latency >= 1, s"The latency of DspPE should be large 1! your latency is $latency now!")

  val extraBitWith = 2 * log2Up(blockSliceConfig.xW)
  val MI2Width     = if (multMode == AD0B1) 2 * dataWidth else dataWidth
  val MWidth       = if (multMode == AD0B1) 4 * dataWidth else 2 * dataWidth

  val io = new Bundle {

    // IN port
    val MI1 = in SInt (dataWidth bits)
    val MI2 = in SInt (MI2Width bits)

    // CASCADE port
    val MC   = isCascade generate { in SInt (MWidth bits) }
    val MI1C = isCascade generate { out SInt (dataWidth bits) }
    val MI2C = isCascade generate { out SInt (MI2Width bits) }

    // OUT PORT
    val M = out SInt (MWidth bits)
  }
  noIoPrefix()

  /** *********** use DSP48E2 to compute (A + D) * B ***************
    */

  // DSP48E2 Configuration for Mult-Add (2*^18*A + D) * B
  val dspConfig = DSPAttrBuilder().setMult(multMode).setLatency(latency, AVERAGE).build
  val dspInst   = DSP48E2(dspConfig).setName(s"dsp_add_mult_add_$nameId")

  // control
  dspInst.INST.OPMODE     := B"000000101" // W = 0, X = M, Y = M, Z = 0
  dspInst.INST.ALUMODE    := B"0000"      // Z + W + X + Y + CIN
  dspInst.INST.CARRYINSEL := B"000"       // select CARRYIN(exactly not use in this configuration)

  import dspConfig._

  MASK               = B"111111111111111111111111111111111111111111111110" // 48bits, 置1的bit被忽略
  PATTERN            = B"000000000000000000000000000000000000000000000001" // 48bits, 想要匹配的模式
  SEL_MASK           = "MASK"
  SEL_PATTERN        = "PATTERN"
  USE_PATTERN_DETECT = "PATDET"

  latency match {
    case 4 =>
      // io
      dspInst.INST.INMODE := B"00101"
    case 3 =>
      // io
      dspInst.INST.INMODE := B"00101"
    case _ =>
      // io
      dspInst.INST.INMODE := B"00100"
  }

  // cascade input (all set as 0)
  dspInst.CASCDATAIN.ACIN        := U(0)
  dspInst.CASCDATAIN.BCIN        := U(0)
  dspInst.CASCDATAIN.PCIN        := U(0)
  dspInst.CASCDATAIN.CARRYCASCIN := False
  dspInst.CASCDATAIN.MULTSIGNIN  := False

  // CE and RST logic
  dspInst.RSTs.all.foreach(_.clear())
  dspInst.CEs.all.foreach(_.set())

  val paddedA =
    if (multMode == AD0B1) (io.MI2.takeHigh(dataWidth).asSInt << 18).resize(30).asUInt
    else io.MI2.resize(30).asUInt
  val paddedD = if (multMode == AD0B1) io.MI2.takeLow(dataWidth).asSInt.resize(27).asUInt else U(0, 27 bits)
  val paddedB = io.MI1.resize(18).asUInt

  // data input
  dspInst.DATAIN.A       := paddedA
  dspInst.DATAIN.B       := paddedB
  dspInst.DATAIN.D       := paddedD
  dspInst.DATAIN.C       := U(0)
  dspInst.DATAIN.CARRYIN := False

  /** ***************** cascade signal logic*********************
    */
  val MCH = if (isCascade) io.MC.takeHigh(MWidth / 2).asSInt.d(latency) else S(0)
  val MCL = if (isCascade) io.MC.takeLow(MWidth / 2).asSInt.d(latency) else S(0)

  /** **************** concat output ****************************
    */
  val MHCARRYIN = (multMode == AD0B1) generate SInt(2 bits)
  (multMode == AD0B1) generate when(dspInst.DATAOUT.P(2 * dataWidth - 1)) { MHCARRYIN := S(1) }
    .otherwise(MHCARRYIN := S(0))

  multMode match {
    case AD0B1 =>
      if (isCascade) {
        io.M := (MCH + dspInst.DATAOUT.P.dropLow(18).takeLow(MWidth / 2).asSInt + MHCARRYIN).resize(MWidth / 2) @@
          (MCL + dspInst.DATAOUT.P.takeLow(MWidth / 2).asSInt).resize(MWidth / 2)
      } else {
        io.M := (dspInst.DATAOUT.P.dropLow(18).takeLow(MWidth / 2).asSInt + MHCARRYIN).resize(MWidth / 2) @@
          dspInst.DATAOUT.P.takeLow(MWidth / 2).asSInt.resize(MWidth / 2)
      }
    case _ =>
      if (isCascade) {
        io.M := (MCH @@ MCL + dspInst.DATAOUT.P.takeLow(MWidth).asSInt)
      } else {
        io.M := dspInst.DATAOUT.P.takeLow(MWidth).asSInt
      }

  }

  if (isCascade) io.MI1C := io.MI1.d(latency)
  if (isCascade) io.MI2C := io.MI2.d(latency)
}

object GenDspPETest extends App {
  SpinalVerilog(new DspPE("0", 8, 3, AD0B1, BlockSliceConfig(2, 2, 2)))

  val a = 2
  val b = BigInt(3)
  println(a.toBinaryString.takeRight(8))
  println(b.toBinaryString(4))
}
