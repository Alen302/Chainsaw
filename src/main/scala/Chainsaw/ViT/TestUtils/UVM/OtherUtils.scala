package Chainsaw.ViT.TestUtils.UVM

import spinal.core.sim._
import spinal.lib.sim.Phase

import scala.collection.mutable

object OtherUtils {
  case class Scoreboard[T]() {
    val dut, ref       = mutable.Queue[T]()
    var where, matches = 0

    if (Phase.isUsed) {
      Phase.check {
        checkEmptyness()
      }
    }

    def pushDut(that: T): Unit = {
      dut.enqueue(that)
      check()
    }

    def pushRef(that: T): Unit = {
      ref.enqueue(that)
      check()
    }

    def compare(ref: T, dut: T): Boolean = ref == dut

    def check(): Unit = {
      if (ref.nonEmpty && dut.nonEmpty) {
        val dutHead = dut.dequeue()
        val refHead = ref.dequeue()
        where += 1
        if (!compare(refHead, dutHead)) {
          println(s"Transaction mismatch at ${where}:")
          println(s"REF: $refHead\tDUT: $dutHead\t")
          simFailure()
        }
        matches += 1
      }
    }

    def checkEmptyness(): Unit = {
      if (dut.nonEmpty || ref.nonEmpty) {
        if (dut.nonEmpty) {
          println("Unmatched DUT transaction : \n")
          dut.foreach(d => println(d))
        }

        if (ref.nonEmpty) {
          println("Unmatched reference transaction :\n")
          ref.foreach(d => println(d))
        }
        if (Phase.isUsed) Phase.check.onEnd(simFailure()) else simFailure()
      }
    }
  }
}
