package Chainsaw.dsp

import Chainsaw._
import Chainsaw.edaFlow.vivado._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.util.Random

// TODO: implement upsample & downsample
case class ParallelFir(
    coeffs: Seq[Double],
    coeffType: NumericType,
    dataType: NumericType,
    parallel: Int
) extends ChainsawInfiniteGenerator
    with FixedLatency {

  override def name =
    s"Parallel_${parallel}_Fir_${dataType}_${coeffType}_coeff${hashName(coeffs)}"

  val upSample = 1
  val tap      = coeffs.length
  logger.info(s"parallel fir tap = $tap")

  /** -------- taps calculation
    * --------
    */
  val phaseCount   = parallel * upSample
  val coeffsPadded = coeffs
  val coeffGroups  = (0 until phaseCount).map(i => coeffsPadded.zipWithIndex.filter(_._2 % phaseCount == i).map(_._1))
  coeffGroups.foreach(c => println(c.mkString(" ")))
  val subFilterGens    = coeffGroups.map(coeff => Fir(coeff, coeffType, dataType))
  val subFilterLatency = subFilterGens.map(_.latency()).max

  /** -------- poly phase network construction
    * --------
    */
  // TODO: apply strength reduction for polyphase filter

  def decomposition(symbol: String, order: Int) = {
    val terms = (0 until order).map(i => TermZDomain(i, Seq((symbol, i))))
    PolyZDomain(terms)
  }

  val termsX = (0 until parallel).map(i => TermZDomain(-i * upSample, Seq(("x", i * upSample))))
  val termsH = (0 until phaseCount).map(i => TermZDomain(-i, Seq(("h", i))))

  def getPolyPhase(p: PolyZDomain, n: Int) = {
    def getPhase(i: Int) = p.terms.filter(_.factors.map(_._2).sum % n == i)

    (0 until n).map(i => getPhase(i).map(_.z(i)))
  }

  val termYs: Seq[Seq[TermZDomain]] = {
    val product = PolyZDomain(termsX) * PolyZDomain(termsH)
    getPolyPhase(product, parallel * upSample)
  }

  logger.info(
    s"\n----polyphase filter report----" +
      s"\n\t${termYs.zipWithIndex
        .map { case (terms, i) => s"Y$i = ${terms.mkString(" + ")}" }
        .mkString("\n\t")}"
  )

  // latency of the adderTree
  val sumLatency           = log2Up(termYs.head.length) // 1 before entering adderTree
  override def inputTypes  = Seq.fill(parallel)(dataType)
  val retType              = dataType * coeffType
  override def outputTypes = Seq.fill(parallel * upSample)(retType)
  override def vivadoUtilEstimation =
    VivadoUtil(dsp = coeffsPadded.length * parallel)

  override def fmaxEstimation = 600 MHz

  override def implH =
    new ChainsawInfiniteModule(this) {

      // sub filters
      val rets = termYs.map { terms =>
        val subFilterRets = terms.map { term =>
          val indexIn           = term.getIndexOf("x") / upSample
          val subFilter         = subFilterGens(term.getIndexOf("h"))
          val compensationDelay = subFilterLatency - subFilter.latency()
          val delay =
            -(term.z / parallel) + compensationDelay // divided by sizeOut as the system is running at a higher speed
          val inFlow  = flowIn.mapFragment(seq => seq.slice(indexIn, indexIn + 1))
          val retFlow = (inFlow >> subFilter).d(delay)
          retFlow.fragment.head
        }

        def add(a: AFix, b: AFix) = {
          val ret = a +| b
          ret.addAttribute("use_dsp", "no") // this is a must
        }

        def pipeline(value: AFix, nothing: Int) = value.d()

        subFilterRets.reduceBalancedTree(add, pipeline)
      }
      dataOut := rets.map(_.truncated)
      lastOut := lastIn.validAfter(latency())
    }

  override def impl(testCase: TestCase) = {
    conv(testCase.data.toArray.map(_.toDouble), coeffs.toArray)
      .drop(coeffs.length - 1)
      .map(BigDecimal(_))
  }

  override def metric(yours: Seq[BigDecimal], golden: Seq[BigDecimal]) =
    corrMetric(yours, golden, 0.9)

  override def testCases = Seq.fill(3)(TestCase(randomDataSequence(Random.nextInt(1000) + 100)))

  override def resetCycle = latency()

  override def latency() = subFilterLatency + sumLatency + (tap / parallel) - 1

  override def implNaiveH = None
}
