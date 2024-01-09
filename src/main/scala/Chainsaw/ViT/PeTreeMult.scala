package Chainsaw.ViT

import spinal.lib._
import spinal.core._
import Chainsaw._
import Chainsaw.device.MULTMODE._

import scala.math._

case class PeTreeMult(dataWidth: Int, multMode: MULTMODE, config: BlockSliceConfig, peLatency: Int) extends Component {
  val extraBitWidth = if (multMode == AD0B1) 2 * log2Up(config.xW) else log2Up(config.xW)
  val peNumber      = config.xW
  val peArrayH      = config.xH
  val peArrayW      = ceil(config.wW.toDouble / 2).toInt
  val latency       = peLatency + log2Up(peNumber)

  val aInWidth  = dataWidth
  val bInWidth  = if (multMode == AD0B1) 2 * dataWidth else dataWidth
  val cOutWidth = if (multMode == AD0B1) 4 * dataWidth else 2 * dataWidth

  val io = new Bundle {
    val aIn = in Vec (Vec(Vec(SInt(dataWidth bits), peNumber), peArrayW), peArrayH)
    val bIn = in Vec (Vec(Vec(SInt(bInWidth bits), peNumber), peArrayW), peArrayH)

    val cOut = out Vec (Vec(SInt(cOutWidth bits), peArrayW), peArrayH)
  }

  val peInsts = Seq.tabulate(peArrayH, peArrayW, peNumber)((i, j, k) =>
    DspPE(s"mult_${i}_${j}_$k", dataWidth, peLatency, multMode, config, isCascade = false)
  )

  // In Drive
  peInsts.zip(io.aIn).foreach { case (peRow, aInRow) =>
    peRow.zip(aInRow).foreach { case (pe, aIn) =>
      pe.zip(aIn).foreach { case (e, in) => e.io.MI1 := in }
    }
  }

  peInsts.zip(io.bIn).foreach { case (peRow, bInRow) =>
    peRow.zip(bInRow).foreach { case (pe, bIn) =>
      pe.zip(bIn).foreach { case (e, in) => e.io.MI2 := in }
    }
  }

  def peMAdd(dspM1: SInt, dspM2: SInt): SInt = {
    multMode match {
      case AD0B1 =>
        val highPart = dspM1.takeHigh(cOutWidth / 2 + extraBitWidth / 2).asSInt +
          dspM2.takeHigh(cOutWidth / 2 + extraBitWidth / 2).asSInt
        val lowPart = dspM1.takeLow(cOutWidth / 2 + extraBitWidth / 2).asSInt +
          dspM2.takeLow(cOutWidth / 2 + extraBitWidth / 2).asSInt
        highPart @@ lowPart
      case _ =>
        dspM1.takeLow(cOutWidth + extraBitWidth).asSInt +
          dspM2.takeLow(cOutWidth + extraBitWidth).asSInt
    }

  }

  // Out Drive
  val peArrayOuts = peInsts.map { peRow =>
    peRow.map { pe =>
      val rawOut = pe
        .map { e =>
          multMode match {
            case AD0B1 =>
              val paddingLowPart  = e.io.M.takeLow(cOutWidth / 2).asSInt.resize(cOutWidth / 2 + extraBitWidth / 2)
              val paddingHighPart = e.io.M.takeHigh(cOutWidth / 2).asSInt.resize(cOutWidth / 2 + extraBitWidth / 2)
              paddingHighPart @@ paddingLowPart
            case _ =>
              e.io.M.resize(cOutWidth + extraBitWidth)
          }
        }
        .reduceBalancedTree(peMAdd, (pe, _) => pe.d())

      multMode match {
        case AD0B1 =>
          (rawOut.takeHigh(cOutWidth / 2 + extraBitWidth / 2).asSInt  >> (extraBitWidth / 2)) @@
            (rawOut.takeLow(cOutWidth / 2 + extraBitWidth / 2).asSInt >> (extraBitWidth / 2))
        case _ =>
          rawOut.takeLow(cOutWidth + extraBitWidth).asSInt >> extraBitWidth
      }
    }
  }

  peArrayOuts.zip(io.cOut).foreach { case (rowOut, ioRowOut) =>
    ioRowOut.zip(rowOut).foreach { case (o, aO) => o := aO }
  }

}
