package Chainsaw.ViT

import spinal.lib._
import spinal.core._
import scala.math._

case class FNN(dataWidth: Int, config: FNNConfig) extends Component {
  val fnn1DataInBufferH   = config.config1.xH
  val fnn1DataInBufferW   = config.config1.xW
  val fnn1WeightInBufferH = fnn1DataInBufferW
  val fnn1WeightInBufferW = ceil(config.config1.wW.toDouble / 2).toInt

  val dataInBlockType   = HardType(Vec(Vec(SInt(dataWidth bits), fnn1DataInBufferW), fnn1DataInBufferH))
  val weightInBlockType = HardType(Vec(Vec(SInt(dataWidth bits), fnn1WeightInBufferW), fnn1WeightInBufferH))

  val io = new Bundle {
    val dataIn   = slave Stream dataInBlockType
    val weightIn = slave Stream weightInBlockType
  }

  val fnn1Config = EigenMatrixGeneratorConfig(config.LH, config.HL1, config.HL2, config.config1)
  val fnn1       = EigenMatrixGenerator(dataWidth, fnn1Config, peLatency = 2)

  fnn1.io.dataIn   << io.dataIn
  fnn1.io.weightIn << io.weightIn

}
