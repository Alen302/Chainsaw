package Chainsaw

package object ViT {

  case class MatrixShape(H: Int, W: Int)
  case class BlockSliceConfig(xH: Int, xW: Int, wW: Int)
  case class AttentionBlockSliceConfig(
      QSlice: BlockSliceConfig,
      KSlice: BlockSliceConfig,
      VSlice: BlockSliceConfig,
      QKSlice: BlockSliceConfig,
      SoftmaxSlice: BlockSliceConfig,
      AttentionSlice: BlockSliceConfig
  )

  case class SingleAttentionConfig(
      tokenShape: MatrixShape,
      weightShape: MatrixShape,
      blockSlices: AttentionBlockSliceConfig
  )

  case class EigenMatrixGeneratorConfig(LH: Int, HL: Int, RW: Int, sliceConfig: BlockSliceConfig)

  case class FNNConfig(
      LH: Int,
      HL1: Int,
      HL2: Int,
      RW: Int,
      config1: BlockSliceConfig,
      config2: BlockSliceConfig,
      minResource: Boolean
  )

}
