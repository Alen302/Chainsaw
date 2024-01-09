package Chainsaw.ViT

import spinal.lib._
import spinal.core._
import Chainsaw._
import scala.collection.mutable.ArrayBuffer
import scala.math._

sealed trait ReShapeMode
object Row extends ReShapeMode
object Col extends ReShapeMode

case class MatrixReShape(dataWidth: Int, shapeIn: MatrixShape, shapeOut: MatrixShape, shapeMode: ReShapeMode)
    extends Component {
  val dataType           = HardType(SInt(dataWidth bits))
  val matrixInBlockType  = Vec(Vec(dataType, shapeIn.W), shapeIn.H)
  val matrixOutBlockType = Vec(Vec(dataType, shapeOut.W), shapeOut.H)
  val matrixOutRegHeight = shapeOut.H
  val matrixOutRegWidth  = shapeOut.W

  // for mode match
  val ioHeightMatch = shapeOut.H % shapeIn.H == 0
  val ioWidthMatch  = shapeOut.W % shapeIn.W == 0

  val reShapeBufferLength = shapeMode match {
    case Row =>
      (ioHeightMatch, ioWidthMatch) match {
        case (true, true)  => 2
        case (true, false) => 2 * ceil(shapeOut.W.toDouble / shapeIn.W).toInt
        case _             => 0
      }
    case Col => 0
  }

  val io = new Bundle {
    val matrixIn  = slave Stream matrixInBlockType
    val matrixOut = master Stream matrixOutBlockType
  }

  val reShapeBuffer = ArrayBuffer.tabulate(shapeIn.H, shapeIn.W)((h, w) =>
    Mem(dataType, Seq.fill(reShapeBufferLength)(dataType.apply().getZero))
  )

  val MatrixOutRegArray =
    ArrayBuffer.tabulate(matrixOutRegHeight, matrixOutRegWidth)((_, _) => RegInit(dataType.apply().getZero))

  val readyMatrixOutBlockNumber = shapeMode match {
    case Row =>
      (ioHeightMatch, ioWidthMatch) match {
        case (true, true)  => Counter(1)
        case (true, false) => Counter(ceil(shapeOut.W.toDouble / shapeIn.W).toInt + 1)
        case _             => ???
      }
    case Col => ???
  }

  val holdValidMatrixOutRegArray = shapeMode match {
    case Row =>
      (ioHeightMatch, ioWidthMatch) match {
        case (true, true)  => ArrayBuffer.tabulate(0, 0)((_, _) => RegInit(dataType.apply().getZero))
        case (true, false) => ArrayBuffer.tabulate(shapeIn.H, shapeIn.W)((_, _) => RegInit(dataType.apply().getZero))
        case _             => ???
      }
    case Col => ???
  }
  val holdValidBlockNumber = shapeMode match {
    case Row =>
      (ioHeightMatch, ioWidthMatch) match {
        case (true, true) =>
          val notUsedCounter = Counter(1)
          notUsedCounter.allowPruning()
          notUsedCounter
        case (true, false) => Counter(ceil(shapeIn.W.toDouble / (shapeOut.W - shapeIn.W)).toInt + 1)
        case _             => ???
      }
    case Col => ???
  }

  val writeBufferAddr = Counter(reShapeBufferLength)
  val readBufferAddr  = Counter(reShapeBufferLength)
  val readWriteRound  = RegInit(False)

  // write buffer logic
  reShapeBuffer.zip(io.matrixIn.payload).foreach { case (rowBuffer, rowIn) =>
    rowBuffer.zip(rowIn).foreach { case (buffer, in) =>
      buffer.write(writeBufferAddr, in, io.matrixIn.fire)
    }
  }

  io.matrixIn.ready.clear()
  io.matrixIn.ready.setWhen(!readWriteRound || (readWriteRound && writeBufferAddr < readBufferAddr))

  val readMatrixInStream = Stream(matrixInBlockType)
  reShapeBuffer.zip(readMatrixInStream.payload).foreach { case (rowBuffer, readRow) =>
    rowBuffer.zip(readRow).foreach { case (buffer, readData) =>
      readData := buffer.readSync(readBufferAddr)
    }
  }

  readMatrixInStream.valid.clear()
  readMatrixInStream.valid.setWhen((!readWriteRound && readBufferAddr < writeBufferAddr) || readWriteRound)

  shapeMode match {
    case Row =>
      (ioHeightMatch, ioWidthMatch) match {
        case (true, true) =>
          readMatrixInStream.ready.clear()
          readMatrixInStream.ready.setWhen(readyMatrixOutBlockNumber.value === U(readyMatrixOutBlockNumber.start))
          when(readMatrixInStream.fire) { readyMatrixOutBlockNumber.increment() }
          when(io.matrixOut.fire) { readyMatrixOutBlockNumber.clear() }
          when(readMatrixInStream.fire) {
            MatrixOutRegArray.zip(readMatrixInStream.payload).foreach { case (rowReg, rowIn) =>
              rowReg.zip(rowIn).foreach { case (reg, in) => when(readMatrixInStream.fire)(reg := in) }
            }
          }
        case (true, false) =>
          readMatrixInStream.ready.clear()
          readMatrixInStream.ready.setWhen(readyMatrixOutBlockNumber.value < U(readyMatrixOutBlockNumber.end))
          when(readMatrixInStream.fire) { readyMatrixOutBlockNumber.increment() }
          when(io.matrixOut.fire) { readyMatrixOutBlockNumber.clear() }
          var candidateMatrixOut = Seq[Seq[SInt]]()

          switch(holdValidBlockNumber.value) {
            Range(0, shapeIn.W).foreach { i =>
              is(U(i)) {
                if (i == 0) {
                  candidateMatrixOut = readMatrixInStream.payload
                } else {
                  // todo: fix
                  candidateMatrixOut = holdValidMatrixOutRegArray
                    .map(_.take(i * (shapeOut.W - shapeIn.W)))
                    .zip(readMatrixInStream.payload.map(_.take(shapeIn.W - i * (shapeOut.W - shapeIn.W))))
                    .map { case (left, right) => left ++ right }
                }
              }
            }
          }

          var candidateHoldMatrix = Seq[Seq[SInt]]()

          switch(holdValidBlockNumber.value) {
            Range(0, ceil(shapeIn.W.toDouble / (shapeOut.W - shapeIn.W)).toInt).foreach { i =>
              is(U(i)) {
                if (i == 0) {
                  candidateHoldMatrix = readMatrixInStream.payload.map { row =>
                    row.takeRight(shapeOut.W - shapeIn.W).padTo(shapeIn.W, dataType.apply().getZero)
                  }
                } else {
                  // todo: fix
                  holdValidMatrixOutRegArray
                    .map(_.take(i * (shapeOut.W - shapeIn.W)))
                    .zip(readMatrixInStream.payload.map(_.take(shapeIn.W - i * (shapeOut.W - shapeIn.W))))
                    .map { case (left, right) => left ++ right }
                }
              }
            }
          }

          switch(readyMatrixOutBlockNumber.value) {
            Range(0, ceil(shapeOut.W.toDouble / shapeIn.W).toInt).foreach { i =>
              is(U(i)) {
                MatrixOutRegArray
                  .map(_.slice(i * shapeIn.W, (i + 1) * shapeIn.W))
                  .zip(candidateMatrixOut)
                  .foreach { case (rowReg, rowIn) =>
                    rowReg.zip(rowIn).foreach { case (reg, in) => when(readMatrixInStream.fire)(reg := in) }
                  }

                if ((i + 1) * shapeIn.W > shapeOut.W) {
                  holdValidMatrixOutRegArray.zip(candidateHoldMatrix).foreach { case (rowReg, rowIn) =>
                    rowReg.zip(rowIn).foreach { case (reg, in) => when(readMatrixInStream.fire)(reg := in) }
                  }
                  when(readMatrixInStream.fire)(holdValidBlockNumber.increment())
                }

              }
            }
          }
        case _ => ???
      }

    case Col => ???
  }

  //todo: add matrixOut valid logic
}
