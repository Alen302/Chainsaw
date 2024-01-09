package Chainsaw.ViT
import Chainsaw._
import Chainsaw.device.MULTMODE.AD0B1
import spinal.lib._
import spinal.core._

case class SystolicMult(blockSliceConfig: BlockSliceConfig, peLatency: Int) extends Component {

  /** ************** config info according to blockSlice parameter *************
    */
  val systolicW            = blockSliceConfig.xW + blockSliceConfig.xH - 1
  val systolicH            = blockSliceConfig.xW + blockSliceConfig.wW / 2 - 1
  val outRow               = blockSliceConfig.xH
  val outCol               = blockSliceConfig.wW / 2
  val extraBitWithForCarry = 2 * log2Up(blockSliceConfig.xW)
  val firstElementPos      = blockSliceConfig.xW - 1
  val matrixRowRange       = Range(firstElementPos, firstElementPos + outRow)
  val matrixColRange       = Range(firstElementPos, firstElementPos + outCol)
  val elementsLatency = Seq
    .tabulate(systolicW, systolicH)((r, c) => (r, c))
    .flatten
    .filter { case (r, c) =>
      matrixRowRange.contains(r) && matrixColRange.contains(c)
    }
    .map { case (r, c) => ((r, c), (r + c - firstElementPos) * peLatency + peLatency) }
    .toMap
  val maxDelay         = elementsLatency.map(_._2).max
  val elementsPipeline = elementsLatency.map { case (pos, l) => (pos, maxDelay - l) }

  /** ******************************* IO define*******************************
    */
  val io = new Bundle {
    val aIns = in Vec (SInt(8 bits), systolicW)
    val bIns = in Vec (SInt(16 bits), systolicH)

    val cOut = out Vec (Vec(SInt(32 + extraBitWithForCarry bits), outCol), outRow)
  }

  val systolicDspInst = Seq.tabulate(systolicW, systolicH) { (r, c) =>
    Map((r, c) -> DspPE(s"${r}_$c", 8, peLatency, AD0B1, blockSliceConfig))
  }

  // Horizontal pipeline
  systolicDspInst
    .sliding(2)
    .toSeq
    .foreach { zipCol =>
      zipCol.last
        .zip(zipCol.head)
        .foreach { case (rPE, lPE) =>
          rPE.values.head.io.MI2 := lPE.values.head.io.MI2C
        }
    }

  // Vertical pipeline
  systolicDspInst.transpose
    .sliding(2)
    .toSeq
    .foreach { zipCol =>
      zipCol.last
        .zip(zipCol.head)
        .foreach { case (uPE, lPE) =>
          uPE.values.head.io.MI1 := lPE.values.head.io.MI1C
        }
    }

  // diagonal pipeline
  systolicDspInst.transpose
    .sliding(2)
    .toSeq
    .foreach { zipCol =>
      zipCol.last.tail
        .zip(zipCol.head.init)
        .foreach { case (rUPE, lLPE) =>
          rUPE.values.head.io.MC := lLPE.values.head.io.M
        }
    }

  // in driver
  systolicDspInst.transpose.head.map(_.values.head).zip(io.aIns).foreach { case (pe, aIn) => pe.io.MI1 := aIn }
  systolicDspInst.head.map(_.values.head).zip(io.bIns).foreach { case (pe, bIn) => pe.io.MI2 := bIn }
  (systolicDspInst.transpose.head ++ systolicDspInst.head).distinct.map(_.values.head).foreach(pe => pe.io.MC := S(0))

  // out driver
  elementsPipeline.foreach { case (pos, delay) =>
    val targetPE = systolicDspInst.flatten.filter(_.keys.head == pos).head.values.head
    io.cOut(pos._1 - firstElementPos)(pos._2 - firstElementPos) := targetPE.io.M.d(delay)
  }

}
