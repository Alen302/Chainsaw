package Chainsaw.ViT

import Chainsaw._
import spinal.core.sim._
import spinal.lib.sim._
import org.slf4j._
import org.scalatest.flatspec._
import spinal.core.{BOOT, ClockDomainConfig, SpinalConfig, log2Up}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random._

class SystolicMultTest extends AnyFlatSpec {
  val logger           = LoggerFactory.getLogger("SystolicMult")
  val blockSliceConfig = BlockSliceConfig(2, 3, 4)
  val peLatency        = 1
  val testLength       = 10

//  val aTestcase =
//    Seq.fill(testLength)(Seq.tabulate(blockSliceConfig.xH, blockSliceConfig.xW)((h, w) => nextInt(128) - nextInt(129)))
//  val bTestcase =
//    Seq.fill(testLength)(Seq.tabulate(blockSliceConfig.xW, blockSliceConfig.wW)((h, w) => nextInt(128) - nextInt(129)))

  val aTestcase =
    Seq.fill(testLength)(Seq.tabulate(blockSliceConfig.xH, blockSliceConfig.xW)((h, w) => 2))
  val bTestcase =
    Seq.fill(testLength)(Seq.tabulate(blockSliceConfig.xW, blockSliceConfig.wW)((h, w) => 2))

  def getGolden(aTestcase: Seq[Seq[Seq[Int]]], bTestcase: Seq[Seq[Seq[Int]]]): Seq[Seq[Seq[Int]]] = {
    aTestcase.zip(bTestcase).map { case (aArray, bArray) =>
      aArray.map { r =>
        bArray.transpose.map { c => r.zip(c).map { case (i, j) => i * j }.sum }
      }
    }
  }
  val abGolden = getGolden(aTestcase, bTestcase)

  def getSystolicInForm(
      aTestcase: Seq[Seq[Seq[Int]]],
      bTestcase: Seq[Seq[Seq[Int]]]
  ): (Seq[Seq[Int]], Seq[Seq[Int]]) = {
    val transformATestcase = aTestcase
    val transformBTestCase = bTestcase.map { b =>
      b.map { r =>
        r.grouped(2).toSeq.map { s =>
          val combStr = s.head.toBinaryString.takeRight(8).reverse.padTo(8, '0').reverse +
            s.head.toBinaryString.takeRight(8).reverse.padTo(8, '0').reverse
          if (combStr(0) == '1')
            Integer.parseInt("-" + combStr.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
          else Integer.parseInt(combStr, 2)
        }
      }
    }
    val aInPortNumber = transformATestcase.map(a => a.length + a.transpose.length - 1).max
    val bInPortNumber = transformBTestCase.map(b => b.length + b.transpose.length - 1).max
    val maxPeriod     = Seq(transformATestcase.map(_.length).max, transformBTestCase.map(_.transpose.length).max).max
    val aForm = transformATestcase.flatMap { a =>
      a.zipWithIndex.map { case (r, i) =>
        Seq.fill(i)(0) ++ r ++ Seq.fill(aInPortNumber - i - r.length)(0)
      } ++ Seq.fill(maxPeriod - a.length)(Seq.fill(aInPortNumber)(0))
    }

    val bForm = transformBTestCase.map(_.transpose).flatMap { b =>
      b.zipWithIndex.map { case (c, i) =>
        Seq.fill(i)(0) ++ c ++ Seq.fill(bInPortNumber - i - c.length)(0)
      } ++ Seq.fill(maxPeriod - b.length)(Seq.fill(bInPortNumber)(0))
    }
    (aForm, bForm)
  }

  val abInForms = getSystolicInForm(aTestcase, bTestcase)

  val dutResults   = ArrayBuffer[Seq[Seq[BigInt]]]()
  val dutAbResults = ArrayBuffer[Seq[Seq[Int]]]()
  SimConfig.withFstWave.compile(SystolicMult(blockSliceConfig, peLatency)).doSim { dut =>
    dut.clockDomain.forkStimulus(10)
    // initial
    dut.io.aIns.foreach(_ #= 0)
    dut.io.bIns.foreach(_ #= 0)

    var testcaseIdx  = 0
    var recordPeriod = 0
    val outGap       = blockSliceConfig.xH
    var outGapIdx    = outGap - 1
    dut.clockDomain.onSamplings {
      if (testcaseIdx < abInForms._1.length) {
        dut.io.aIns.zip(abInForms._1(testcaseIdx)).foreach { case (inPort, driver) => inPort #= driver }
      } else dut.io.aIns.foreach(_ #= 0)

      if (testcaseIdx < abInForms._2.length) {
        dut.io.bIns.zip(abInForms._2(testcaseIdx)).foreach { case (inPort, driver) => inPort #= driver }
      } else dut.io.bIns.foreach(_ #= 0)

      if (recordPeriod >= dut.maxDelay + 1) {
        if (outGapIdx == outGap - 1) dutResults += dut.io.cOut.map(_.map(_.toBigInt))
        if (outGapIdx < outGap - 1) outGapIdx += 1 else outGapIdx = 0
      }
      testcaseIdx += 1
      recordPeriod += 1
    }

    dut.clockDomain.waitSamplingWhere(dutResults.length == testLength)

    dutResults.zipWithIndex.foreach { case (res, i) =>
      dutAbResults.prepend(res.map { r =>
        r.flatMap { e =>
          val uStr = e.toBinaryString(32 + 2 * log2Up(blockSliceConfig.xW)).take(16 + log2Up(blockSliceConfig.xW))
          val u =
            if (uStr.head == '1')
              Integer.parseInt("-" + uStr.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
            else Integer.parseInt(uStr, 2)

          val mlStr =
            e.toBinaryString(32 + 2 * log2Up(blockSliceConfig.xW)).takeRight(16 + log2Up(blockSliceConfig.xW))
          val l = {
            if (mlStr.head == '1')
              Integer.parseInt("-" + mlStr.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
            else Integer.parseInt(mlStr, 2)
          }
          Seq(u, l)
        }
      })
    }

    val testCaseVisualization = aTestcase
      .zip(bTestcase)
      .zipWithIndex
      .map { case ((aArray, bArray), i) =>
        s"A$i:\n${aArray.map(_.mkString(",")).mkString("\n")}\n" + s"B$i:\n${bArray.map(_.mkString(",")).mkString("\n")}\n"
      }
    val abGoldenVisualization = abGolden.zipWithIndex.map { case (golden, i) =>
      s"[Golden]C$i:\n${golden.map(_.mkString(",")).mkString("\n")}\n"
    }
    val dutAbResultsVisualization = dutAbResults.zipWithIndex.map { case (dut, i) =>
      s"[Dut]C$i:\n${dut.map(_.mkString(",")).mkString("\n")}\n"
    }
    logger.info(
      s"\n" + testCaseVisualization
        .zip(abGoldenVisualization.zip(dutAbResultsVisualization))
        .map { case (t, (g, d)) => t + g + d }
        .mkString("\n----\n")
    )

  }

}
