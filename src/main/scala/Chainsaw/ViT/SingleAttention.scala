package Chainsaw.ViT

import spinal.lib._
import spinal.core._

case class SingleAttention(dataWidth: Int, config: SingleAttentionConfig, peLatency: Int) extends Component {
  import config._
  val tokenInType      = HardType(Vec(Vec(SInt(dataWidth bits), tokenShape.W), tokenShape.H))
  val attentionOutType = tokenInType
  val io = new Bundle {
    val tokenIn      = slave Stream tokenInType
    val attentionOut = master Stream attentionOutType
  }

  val tokenLength  = 1024
  val hiddenLength = 1024
  val QMatrixGenerator =
    EigenMatrixGenerator(
      dataWidth,
      EigenMatrixGeneratorConfig(tokenShape.H, tokenLength, hiddenLength, blockSlices.QSlice),
      peLatency
    )
}
