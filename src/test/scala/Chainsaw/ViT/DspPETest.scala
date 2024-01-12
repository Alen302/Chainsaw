package Chainsaw.ViT

import Chainsaw._
import Chainsaw.device.MULTMODE.{AB1, AD0B1}
import Chainsaw.edaFlow.vivado.VivadoTask
import spinal.core._
import spinal.lib._
import org.slf4j._
import org.scalatest.flatspec._
import spinal.lib.StreamFifo

import scala.collection.mutable.ArrayBuffer
import scala.util.Random._

class DspPETest extends AnyFlatSpec {

  it should "test pass" in {
    val latency          = 1
    val logger           = LoggerFactory.getLogger("twoMultByDspTest")
    val blockSliceConfig = BlockSliceConfig(2, 2, 2)
    val dataWidth        = 8
    val mode             = AB1
    val isCascade        = false
//    SimConfig
//      .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT)))
//      .withFstWave
//      .compile(new DspPE("0", dataWidth, latency, mode, blockSliceConfig, isCascade))
//      .doSim { dut =>
//        dut.clockDomain.forkStimulus(10)
//        // initial
//        dut.io.MI1               #= -2
//        dut.io.MI2               #= -2
//        if (isCascade) dut.io.MC #= 0
//
//        var testLength = 10
//        val aTestcase  = Seq.tabulate(testLength)(_ => nextInt(128) - nextInt(129))
//        val bTestcase  = Seq.tabulate(testLength)(_ => nextInt(128) - nextInt(129))
//        val dTestcase  = Seq.tabulate(testLength)(_ => nextInt(128) - nextInt(129))
////
////        val aTestcase = Seq.tabulate(testLength)(i => -(i + 3))
////        val bTestcase = Seq.tabulate(testLength)(i => -(i + 2))
////        val dTestcase = Seq.tabulate(testLength)(i => i + 1)
//
//        val mLResult = ArrayBuffer[Int]()
//        val mUResult = ArrayBuffer[Int]()
////        val mResult = ArrayBuffer[Int]()
//
//        var testcaseIdx = 0
//
//        // 0+ operate
//        dut.clockDomain.onSamplings {
//          if (testcaseIdx < testLength) {
//            dut.io.MI1 #= bTestcase(testcaseIdx)
//            val MI2String =
//              aTestcase(testcaseIdx).toBinaryString.takeRight(dataWidth).reverse.padTo(dataWidth, '0').reverse +
//                dTestcase(testcaseIdx).toBinaryString.takeRight(dataWidth).reverse.padTo(dataWidth, '0').reverse
//            val MI2Data =
//              if (MI2String(0) == '1')
//                Integer.parseInt("-" + MI2String.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
//              else Integer.parseInt(MI2String, 2)
//            dut.io.MI2 #= (if (mode == AD0B1) MI2Data else aTestcase(testcaseIdx))
//
//          }
//
//          if (mLResult.length < testLength + latency + 1) {
//            val mlInt = dut.io.M.toBigInt
//            val mlString =
//              mlInt.toBinaryString(4 * dataWidth).takeRight(2 * dataWidth)
//            val ml =
//              if (mlString.head == '1')
//                Integer.parseInt("-" + mlString.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
//              else Integer.parseInt(mlString, 2)
//            mLResult += ml
//          }
//
//          if (mUResult.length < testLength + latency + 1) {
//            val muInt = dut.io.M.toBigInt
//            val muString =
//              muInt.toBinaryString(4 * dataWidth).take(2 * dataWidth)
//            val mu =
//              if (muString.head == '1')
//                Integer.parseInt("-" + muString.tail.map(str => if (str == '1') '0' else '1'), 2) - 1
//              else Integer.parseInt(muString, 2)
//            mUResult += mu
//          }
//
//          testcaseIdx += 1
//        }
//        dut.clockDomain.waitSamplingWhere(
//          mLResult.length == testLength + latency + 1 && mUResult.length == testLength + latency + 1
//        )
//
//        val goldenML = bTestcase.zip(dTestcase).map { case (b, d) => b * d }
//        val goldenMU = aTestcase.zip(bTestcase).map { case (a, b) => a * b }
//
//        val simPass = if (mode == AD0B1) {
//          goldenML.equals(mLResult.drop(latency + 1)) && goldenMU.equals(mUResult.drop(latency + 1))
//        } else {
//          goldenMU.equals(mLResult.drop(latency + 1))
//        }
//        if (simPass) {
//          logger.info(
//            s"\nGolden:" +
//              s"\n[INPUT]A:${aTestcase.mkString(",")}" +
//              s"\n[INPUT]B:${bTestcase.mkString(",")}" +
//              s"\n[INPUT]D:${dTestcase.mkString(",")}" +
//              s"\n[OUTPUT]ML:${if (mode == AD0B1) goldenML.mkString(",") else goldenMU.mkString(",")}" +
//              s"\n[OUTPUT]MU:${if (mode == AD0B1) goldenMU.mkString(",") else goldenML.mkString(",")}" +
//              s"\nDut:" +
//              s"\n[OUTPUT]ML:${mLResult.drop(latency + 1).mkString(",")}" +
//              s"\n[OUTPUT]MU:${mUResult.drop(latency + 1).mkString(",")}\n"
//          )
//          simSuccess()
//        } else {
//          logger.info(
//            s"Golden:" +
//              s"\n[INPUT]A:${aTestcase.mkString(",")}" +
//              s"\n[INPUT]B:${bTestcase.mkString(",")}" +
//              s"\n[INPUT]D:${dTestcase.mkString(",")}" +
//              s"\n[OUTPUT]ML:${goldenML.mkString(",")}" +
//              s"\n[OUTPUT]MU:${goldenMU.mkString(",")}" +
//              s"\nDut:" +
//              s"\n[OUTPUT]ML:${mLResult.drop(latency + 1).mkString(",")}" +
//              s"\n[OUTPUT]MU:${mUResult.drop(latency + 1).mkString(",")}\n" +
//              s"\nDiff[Dut-Golden]:" +
//              s"\n[Diff]ML:${mLResult.drop(latency + 1).zip(goldenML).map { case (dut, golden) => dut - golden }.mkString(",")}" +
//              s"\n[Diff]MU:${mUResult.drop(latency + 1).zip(goldenMU).map { case (dut, golden) => dut - golden }.mkString(",")}\n"
//          )
//          simFailure()
//        }
//        println("SimTimeout!")
//        SimTimeout(10000)
//      }

    VivadoTask.synthModule("DspPE", DspPE("0", dataWidth, latency, mode, blockSliceConfig, isCascade))
  }

}
