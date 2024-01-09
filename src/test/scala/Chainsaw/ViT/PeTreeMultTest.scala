package Chainsaw.ViT
import Chainsaw.{BigIntUtil, pow2}
import Chainsaw.device.MULTMODE.{AB1, AD0B1}
import spinal.core.sim._
import spinal.lib.sim._
import org.slf4j._
import org.scalatest.flatspec._
import spinal.core.log2Up

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil
import scala.util.Random.nextInt

class PeTreeMultTest extends AnyFlatSpec {
  val logger           = LoggerFactory.getLogger(s"TreeMultTest")
  val dataWidth        = 8
  val multMode         = AD0B1
  val testLength       = 100
  val peLatency        = 1
  val blockSliceConfig = BlockSliceConfig(2, 2, 2)

  val arrayH   = blockSliceConfig.xH
  val arrayW   = ceil(blockSliceConfig.wW.toDouble / 2).toInt
  val peLength = blockSliceConfig.xW

  val aTestcase = Seq.tabulate(testLength, peLength, arrayW)((_, _, _) =>
    nextInt(pow2(dataWidth - 1).toInt - 1) - nextInt(pow2(dataWidth - 1).toInt)
  )
  val bTestcase = Seq.tabulate(testLength, arrayH, peLength)((_, _, _) =>
    nextInt(pow2(dataWidth - 1).toInt - 1) - nextInt(pow2(dataWidth - 1).toInt)
  )
  val dTestcase = Seq.tabulate(testLength, peLength, arrayW)((_, _, _) =>
    nextInt(pow2(dataWidth - 1).toInt - 1) - nextInt(pow2(dataWidth - 1).toInt)
  )

  val goldenML = bTestcase.zip(dTestcase).map { case (bArray, dArray) =>
    bArray.map { bRow =>
      dArray.transpose.map { dCol =>
        val resString = bRow
          .zip(dCol)
          .map { case (b, d) => b * d }
          .sum
          .toBinaryString
          .takeRight(2 * dataWidth + log2Up(blockSliceConfig.xW))
          .reverse
          .padTo(2 * dataWidth + log2Up(blockSliceConfig.xW), '0')
          .reverse
          .take(2 * dataWidth)
        if (resString(0) == '1')
          Integer.parseInt("-" + resString.tail.map(s => if (s == '1') '0' else '1'), 2) - 1
        else Integer.parseInt(resString, 2)
      }
    }
  }

  val goldenMU = bTestcase.zip(aTestcase).map { case (bArray, aArray) =>
    bArray.map { bRow =>
      aArray.transpose.map { aCol =>
        val resString = bRow
          .zip(aCol)
          .map { case (b, a) => b * a }
          .sum
          .toBinaryString
          .takeRight(2 * dataWidth + log2Up(blockSliceConfig.xW))
          .reverse
          .padTo(2 * dataWidth + log2Up(blockSliceConfig.xW), '0')
          .reverse
          .take(2 * dataWidth)

        if (resString(0) == '1')
          Integer.parseInt("-" + resString.tail.map(s => if (s == '1') '0' else '1'), 2) - 1
        else Integer.parseInt(resString, 2)
      }
    }
  }

  val mLResult = ArrayBuffer[Seq[Seq[Int]]]()
  val mUResult = ArrayBuffer[Seq[Seq[Int]]]()

  SimConfig.withFstWave.compile(PeTreeMult(dataWidth, multMode, blockSliceConfig, peLatency)).doSim { dut =>
    var testcaseIdx = 0

    dut.clockDomain.forkStimulus(10)
    // 0+ operate
    dut.clockDomain.onSamplings {
      if (testcaseIdx < testLength) {
        dut.io.aIn.zip(bTestcase(testcaseIdx)).foreach { case (aInRow, bRow) =>
          aInRow.foreach { pes =>
            pes.zip(bRow).foreach { case (pe, b) => pe #= b }
          }
        }
        val MI2Strings =
          aTestcase(testcaseIdx).zip(dTestcase(testcaseIdx)).map { case (aRow, dRow) =>
            aRow.zip(dRow).map { case (a, d) =>
              a.toBinaryString
                .takeRight(8)
                .reverse
                .padTo(8, '0')
                .reverse + d.toBinaryString.takeRight(8).reverse.padTo(8, '0').reverse
            }
          }

        val MI2Datas = MI2Strings.map { rowStr =>
          rowStr.map { str =>
            if (str(0) == '1')
              Integer.parseInt("-" + str.tail.map(s => if (s == '1') '0' else '1'), 2) - 1
            else Integer.parseInt(str, 2)
          }
        }
        if (multMode == AD0B1) {
          dut.io.bIn.foreach { bInRow =>
            bInRow.zip(MI2Datas.transpose).foreach { case (pes, adCol) =>
              pes.zip(adCol).foreach { case (pe, b) => pe #= b }
            }
          }
        } else {
          dut.io.bIn.foreach { bInRow =>
            bInRow.zip(aTestcase(testcaseIdx).transpose).foreach { case (pes, aCol) =>
              pes.zip(aCol).foreach { case (pe, b) => pe #= b }
            }
          }
        }
      }

      if (mLResult.length < testLength + peLatency + log2Up(blockSliceConfig.xW) + 1) {
        val mlInts = dut.io.cOut.map(_.map(_.toBigInt))
        val mlStrings =
          mlInts.map { l =>
            multMode match {
              case AD0B1 => l.map(_.toBinaryString(4 * dataWidth).takeRight(2 * dataWidth))
              case _     => l.map(_.toBinaryString(2 * dataWidth))
            }
          }
        val mls = mlStrings.map {
          _.map { slStr =>
            if (slStr.head == '1')
              Integer.parseInt("-" + slStr.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
            else Integer.parseInt(slStr, 2)
          }
        }

        mLResult += mls
      }

      if (mUResult.length < testLength + peLatency + log2Up(blockSliceConfig.xW) + 1) {
        val muInts = dut.io.cOut.map(_.map(_.toBigInt))
        val muStrings =
          muInts.map { u =>
            multMode match {
              case AD0B1 => u.map(_.toBinaryString(4 * dataWidth).take(2 * dataWidth))
              case _     => u.map(_.toBinaryString(2 * dataWidth))
            }

          }
        val mus = muStrings.map {
          _.map { suStr =>
            if (suStr.head == '1')
              Integer.parseInt("-" + suStr.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
            else Integer.parseInt(suStr, 2)
          }
        }
        mUResult += mus
      }

      testcaseIdx += 1
    }
    dut.clockDomain.waitSamplingWhere(
      mLResult.length == testLength + peLatency + log2Up(
        blockSliceConfig.xW
      ) + 1 && mUResult.length == testLength + peLatency + log2Up(blockSliceConfig.xW) + 1
    )

    val aTestCaseVisualization = aTestcase.zipWithIndex
      .map { case (aArray, i) =>
        s"A$i:\n${aArray.map(_.mkString(",")).mkString("\n")}\n"
      }

    val bTestCaseVisualization = bTestcase.zipWithIndex
      .map { case (bArray, i) =>
        s"B$i:\n${bArray.map(_.mkString(",")).mkString("\n")}\n"
      }

    val dTestCaseVisualization = dTestcase.zipWithIndex
      .map { case (dArray, i) =>
        s"D$i:\n${dArray.map(_.mkString(",")).mkString("\n")}\n"
      }

    val mlGoldenVisualization = goldenML.zipWithIndex.map { case (mlGolden, i) =>
      s"[Golden]ML$i:\n${mlGolden.map(_.mkString(",")).mkString("\n")}\n"
    }

    val muGoldenVisualization = goldenMU.zipWithIndex.map { case (muGolden, i) =>
      s"[Golden]MU$i:\n${muGolden.map(_.mkString(",")).mkString("\n")}\n"
    }

    val dutMlVisualization =
      mLResult.drop(peLatency + log2Up(blockSliceConfig.xW) + 1).zipWithIndex.map { case (mlResult, i) =>
        s"[DUT]ML$i:\n${mlResult.map(_.mkString(",")).mkString("\n")}\n"
      }

    val dutMuVisualization =
      mUResult.drop(peLatency + log2Up(blockSliceConfig.xW) + 1).zipWithIndex.map { case (muResult, i) =>
        s"[DUT]MU$i:\n${muResult.map(_.mkString(",")).mkString("\n")}\n"
      }

    val simPass = multMode match {
      case AD0B1 =>
        goldenML.equals(mLResult.drop(peLatency + log2Up(blockSliceConfig.xW) + 1)) && goldenMU
          .equals(mUResult.drop(peLatency + log2Up(blockSliceConfig.xW) + 1))
      case _ => goldenMU.equals(mUResult.drop(peLatency + log2Up(blockSliceConfig.xW) + 1))
    }
    if (simPass) {
      logger.info(
        s"\n" +
          aTestCaseVisualization
            .zip(bTestCaseVisualization.zip(dTestCaseVisualization))
            .zip(mlGoldenVisualization.zip(muGoldenVisualization))
            .zip(dutMlVisualization.zip(dutMuVisualization))
            .map { case (((a, (b, d)), (ml, mu)), (dml, dmu)) =>
              a + b + d + ml + mu + dml + dmu
            }
            .mkString("\n----\n")
      )
      simSuccess()
    } else {

      logger.info(
        s"\n" +
          aTestCaseVisualization
            .zip(bTestCaseVisualization.zip(dTestCaseVisualization))
            .zip(mlGoldenVisualization.zip(muGoldenVisualization))
            .zip(dutMlVisualization.zip(dutMuVisualization))
            .map { case (((a, (b, d)), (ml, mu)), (dml, dmu)) =>
              a + b + d + ml + mu + dml + dmu
            }
            .mkString("\n----\n")
      )
      simFailure()
    }
    println("SimTimeout!")
    SimTimeout(10000)
  }

}
