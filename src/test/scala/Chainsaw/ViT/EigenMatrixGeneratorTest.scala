package Chainsaw.ViT

import Chainsaw.ViT.TestUtils.SimUtils._
import Chainsaw.ViT.TestUtils.UVM._
import Chainsaw.edaFlow.vivado.VivadoTask
import spinal.core.sim._
import spinal.lib.sim._
import org.slf4j._
import org.scalatest.flatspec._
import spinal.core.log2Up

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil
import scala.util.Random

class EigenMatrixGeneratorTest extends AnyFlatSpec {

  val config = EigenMatrixGeneratorConfig(573, 1024, 1024, BlockSliceConfig(8, 3, 2))

//  val config = EigenMatrixGeneratorConfig(16, 12, 12, BlockSliceConfig(8, 3, 2))

  SimConfig.withFstWave.compile(EigenMatrixGenerator(8, config, 2)).doSim { dut =>
    import dut._

    val tokenRowBlockNumber = ceil(dut.config.LH.toDouble / dut.config.sliceConfig.xH).toInt
    val tokenColBlockNumber = ceil(dut.config.HL.toDouble / dut.config.sliceConfig.xW).toInt

    val weightRowBlockNumber = tokenColBlockNumber
    val weightColBlockNumber = ceil(dut.config.RW.toDouble / dut.config.sliceConfig.wW).toInt

    val eigenMatrixRowBlockNumber = ceil(dut.config.LH.toDouble / dut.config.sliceConfig.xH).toInt
    val eigenMatrixColBlockNumber = ceil(dut.config.RW.toDouble / dut.config.sliceConfig.wW).toInt

    val randomTokenTestCases =
      ArrayBuffer.tabulate(dut.config.LH, dut.config.HL)((h, w) => Random.nextInt(127) - Random.nextInt(128))

    val randomWeightTestCases =
      ArrayBuffer.tabulate(dut.config.HL, dut.config.RW)((h, w) => Random.nextInt(127) - Random.nextInt(128))

    val dutTokenTestCases  = ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]()
    val dutWeightTestCases = ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]()
    val dutEigenMatrixOut =
      ArrayBuffer.fill(dut.config.sliceConfig.xH * dut.config.sliceConfig.wW)(ArrayBuffer[BigInt]())

    def reShapeToken(config: EigenMatrixGeneratorConfig, src: ArrayBuffer[ArrayBuffer[Int]]): Unit = {
      val paddingZeroForShape =
        src
          .map(_.padTo(tokenColBlockNumber * config.sliceConfig.xW, 0))
          .padTo(
            tokenRowBlockNumber * config.sliceConfig.xH,
            ArrayBuffer.fill(tokenColBlockNumber * config.sliceConfig.xW)(0)
          )
      val groupedForW = paddingZeroForShape.transpose.grouped(config.sliceConfig.xW)
      groupedForW
        .flatMap { block =>
          block.transpose.grouped(config.sliceConfig.xH)
        }
        .foreach { src => dutTokenTestCases += src }
    }

    def reShapeWeight(config: EigenMatrixGeneratorConfig, src: ArrayBuffer[ArrayBuffer[Int]]): Unit = {
      val paddingZeroForShape =
        src
          .map(_.padTo(weightColBlockNumber * config.sliceConfig.wW, 0))
          .padTo(
            weightRowBlockNumber * config.sliceConfig.xW,
            ArrayBuffer.fill(weightColBlockNumber * config.sliceConfig.wW)(0)
          )
      val groupedForH = paddingZeroForShape.grouped(config.sliceConfig.xW)
      groupedForH.zipWithIndex
        .flatMap { case (block, i) => block.transpose.grouped(config.sliceConfig.wW).map(_.transpose) }
        .foreach { src => dutWeightTestCases += src }
    }

    def blocksToMatrix(
        blocks: Seq[Seq[Seq[Int]]]
    ): ArrayBuffer[ArrayBuffer[Int]] = {
      val matrixResult =
        ArrayBuffer.fill(eigenMatrixRowBlockNumber * dut.config.sliceConfig.xH)(ArrayBuffer[Int]())
      Range(0, eigenMatrixRowBlockNumber * dut.config.sliceConfig.xH).foreach { r =>
        val groupedForReshape = blocks.grouped(eigenMatrixRowBlockNumber).toSeq
        groupedForReshape
          .flatMap { colBlocks =>
            colBlocks(r / dut.config.sliceConfig.xH)(r % dut.config.sliceConfig.xH)
          }
          .foreach { eigenValue =>
            matrixResult(r) += eigenValue
          }
      }
      matrixResult

    }

    def getGoldenResult(
        token: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]],
        weight: ArrayBuffer[ArrayBuffer[ArrayBuffer[Int]]]
    ): ArrayBuffer[ArrayBuffer[Int]] = {

      def blockGoldenResult(
          tokenBlock: ArrayBuffer[ArrayBuffer[Int]],
          weightBlock: ArrayBuffer[ArrayBuffer[Int]]
      ): ArrayBuffer[ArrayBuffer[Int]] = {
        tokenBlock.map { tRow =>
          weightBlock.transpose.map { wCol =>
            val resString = tRow
              .zip(wCol)
              .map { case (b, a) => b * a }
              .sum
              .toBinaryString
              .takeRight(2 * dataWidth + log2Up(dut.config.sliceConfig.xW))
              .reverse
              .padTo(2 * dataWidth + log2Up(dut.config.sliceConfig.xW), '0')
              .reverse
              .take(2 * dataWidth)

            if (resString(0) == '1')
              Integer.parseInt("-" + resString.tail.map(s => if (s == '1') '0' else '1'), 2) - 1
            else Integer.parseInt(resString, 2)
          }
        }
      }

      val result =
        weight
          .grouped(weightColBlockNumber)
          .toSeq
          .zip(token.grouped(tokenRowBlockNumber).toSeq)
          .map { case (weightRowBlocks, tokenColBlocks) =>
            weightRowBlocks.flatMap { weightRowBlock =>
              tokenColBlocks.map { tokenColBlock =>
                blockGoldenResult(tokenColBlock, weightRowBlock)
              }
            }
          }
          .reduce { (left, right) =>
            left.zip(right).map { case (l, r) =>
              l.zip(r).map { case (lr, rr) =>
                lr.zip(rr).map { case (le, re) =>
                  val resString = (le + re).toBinaryString
                    .takeRight(2 * dataWidth + 1)
                    .reverse
                    .padTo(2 * dataWidth + 1, '0')
                    .reverse
                    .take(2 * dataWidth)

                  if (resString(0) == '1')
                    Integer.parseInt("-" + resString.tail.map(s => if (s == '1') '0' else '1'), 2) - 1
                  else Integer.parseInt(resString, 2)
                }
              }
            }
          }
          .map {
            _.map {
              _.map { e =>
                val resString = e.toBinaryString
                  .takeRight(2 * dataWidth)
                  .reverse
                  .padTo(2 * dataWidth, '0')
                  .reverse
                  .take(dataWidth)

                if (resString(0) == '1')
                  Integer.parseInt("-" + resString.tail.map(s => if (s == '1') '0' else '1'), 2) - 1
                else Integer.parseInt(resString, 2)
              }
            }
          }

      blocksToMatrix(result)
    }

    def formatTestCaseByConfig[T](testCase: ArrayBuffer[ArrayBuffer[T]], H: Int, W: Int, valueWidth: Int = 5) = {
      testCase
        .map(_.map(_.toString.padTo(valueWidth, ' ')).grouped(W).map(_.mkString(",")).mkString("| "))
        .grouped(H)
        .map(lines => lines.mkString("\n") + s"\n${"-" * lines.head.length}")
        .mkString("\n")
    }

    // generate testCase
    reShapeToken(dut.config, randomTokenTestCases)
    reShapeWeight(dut.config, randomWeightTestCases)
    // generate golden
    val goldenResult = getGoldenResult(dutTokenTestCases, dutWeightTestCases)

    println(
      s"randomTestCase: \n${formatTestCaseByConfig(randomTokenTestCases, dut.config.sliceConfig.xH, dut.config.sliceConfig.xW)}"
    )
    println(s"token block Length: ${randomTokenTestCases.length}")
    println(
      s"randomWeight: \n${formatTestCaseByConfig(randomWeightTestCases, dut.config.sliceConfig.xW, dut.config.sliceConfig.wW)}"
    )
    println(s"weight block Length: ${dutWeightTestCases.length}")
    println(
      s"goldenResult: \n${formatTestCaseByConfig(goldenResult, dut.config.sliceConfig.xH, dut.config.sliceConfig.wW, valueWidth = 8)}"
    )
    println(s"result block Length: ${goldenResult.length}")

    // clockDomain driver
    dut.clockDomain.forkStimulus(10)

    // set initial value for dut (initial zero value)
    dut.io.dataIn.setZeroInitValue(dut.clockDomain)
    dut.io.weightIn.setZeroInitValue(dut.clockDomain)
    dut.io.eigenMatrixOut.setZeroInitValue(dut.clockDomain)

    // data Driver
    dut.io.dataIn.setDriverRandomly(
      dut.clockDomain,
      16,
      ArrayBuffer.tabulate(dut.tokenBufferW * dut.tokenBufferH)(i =>
        dutTokenTestCases.map(_.flatten).map(s => BigInt(s(i)))
      ): _*
    )
    dut.io.weightIn.setDriverRandomly(
      dut.clockDomain,
      16,
      ArrayBuffer.tabulate(2 * dut.weightBufferW * dut.weightBufferH)(i =>
        dutWeightTestCases.map(_.flatten).map(s => BigInt(s(i)))
      ): _*
    )
    dut.io.eigenMatrixOut.setRandomDriverRandomly(dut.clockDomain, 16)

    // data Monitor
    dut.io.eigenMatrixOut.setMonitorAlways(dut.clockDomain, 16, dutEigenMatrixOut: _*)

    // protocol Check
    dut.io.flattenForeach(_.setStreamAssert(dut.clockDomain))

    dut.clockDomain
      .waitSamplingWhere {
//        println(dutEigenMatrixOut.head.length)
        dutEigenMatrixOut.forall(_.length == eigenMatrixRowBlockNumber * eigenMatrixColBlockNumber)
      }

    println(s"Done Simulation, begin to compare dut output and golden")

    // format dut result
    val transposeDutEigenMatrixOut = Range(0, eigenMatrixRowBlockNumber * eigenMatrixColBlockNumber).map { i =>
      dutEigenMatrixOut
        .map { eigen => eigen(i).toInt }
        .grouped(dut.config.sliceConfig.wW)
        .toSeq
    }

    println(s"Done transpose!")
    val dutEigenMatrixResult = blocksToMatrix(transposeDutEigenMatrixOut)
    println(s"Done reShape!")
//    val dutEigenMatrixResult =
//      ArrayBuffer.fill(eigenMatrixRowBlockNumber * dut.config.sliceConfig.xH)(ArrayBuffer[BigInt]())
//    Range(0, eigenMatrixRowBlockNumber * dut.config.sliceConfig.xH).foreach { r =>
//      val groupedForReshape = transposeDutEigenMatrixOut.grouped(eigenMatrixRowBlockNumber).toSeq
//      groupedForReshape
//        .flatMap { colBlocks =>
//          colBlocks(r / dut.config.sliceConfig.xH)(r % dut.config.sliceConfig.xH)
//        }
//        .foreach { eigenValue =>
//          dutEigenMatrixResult(r) += eigenValue
//        }
//    }

    // print run log

    println(
      s" dutResult: \n${formatTestCaseByConfig(dutEigenMatrixResult, dut.config.sliceConfig.xH, dut.config.sliceConfig.wW, valueWidth = 8)}"
    )

    val testPass = goldenResult.zip(dutEigenMatrixResult).forall { case (golden, actual) => golden.equals(actual) }
    if (testPass) {
      simSuccess()
    } else {
      val resultDiff = goldenResult.zip(dutEigenMatrixResult).map { case (goldenLine, actualLine) =>
        goldenLine.zip(actualLine).map { case (golden, actual) => golden - actual }
      }
      simFailure(
        s"Result Diff:\n${formatTestCaseByConfig(resultDiff, dut.config.sliceConfig.xH, dut.config.sliceConfig.wW, valueWidth = 8)}"
      )
    }

  }

//  VivadoTask.fastSynthModule("EigenMatrixGenerator", EigenMatrixGenerator(8, config, 2))

}
