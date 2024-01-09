package Chainsaw.ViT

import Chainsaw.DataUtil
import Chainsaw.device.MULTMODE._
import spinal.lib._
import spinal.core._
import spinal.core.sim.{SimDataPimper, SimMemPimper}

import scala.math._

case class EigenMatrixGenerator(dataWidth: Int, config: EigenMatrixGeneratorConfig, peLatency: Int) extends Component {
  val tokenBufferH            = config.sliceConfig.xH
  val tokenBufferW            = config.sliceConfig.xW
  val tokenBufferDepth        = ceil(config.LH.toDouble / config.sliceConfig.xH).toInt
  val weightBufferH           = tokenBufferW
  val weightBufferW           = ceil(config.sliceConfig.wW.toDouble / 2).toInt //
  val weightBufferDepth       = ceil(config.RW.toDouble / config.sliceConfig.wW).toInt
  val eigenBufferW            = config.sliceConfig.wW                          //
  val eigenBufferH            = config.sliceConfig.xH
  val eigenBufferDepth        = tokenBufferDepth * weightBufferDepth
  val LoopComputePeriodNumber = ceil(config.HL.toDouble / config.sliceConfig.xW).toInt
  val peExtraWidth            = 2 * log2Up(config.sliceConfig.xW)
  val eigenValueExtraWidth    = log2Up(LoopComputePeriodNumber) + peExtraWidth / 2

  val tokenType             = HardType(SInt(dataWidth bits))
  val weightType            = HardType(SInt(dataWidth bits))
  val bufferWeightType      = HardType(SInt(2 * dataWidth bits))
  val bufferEigenType       = HardType(SInt(2 * dataWidth bits))
  val eigenType             = HardType(SInt(dataWidth bits))
  val tokenBlockType        = Vec(Vec(tokenType, tokenBufferW), tokenBufferH)
  val weightBlockType       = Vec(Vec(weightType, 2 * weightBufferW), weightBufferH)
  val bufferWeightBlockType = Vec(Vec(bufferWeightType, weightBufferW), weightBufferH)
  val eigenBlockType        = Vec(Vec(eigenType, eigenBufferW), eigenBufferH)

  val io = new Bundle {
    val dataIn         = slave Stream tokenBlockType
    val weightIn       = slave Stream weightBlockType
    val eigenMatrixOut = master Stream eigenBlockType
  }

  // Input Buffer
  val tokenBufferArray = Array.tabulate(tokenBufferH, tokenBufferW, 2)((i, j, n) =>
    Mem(tokenType, Seq.fill(tokenBufferDepth)(tokenType.apply().getZero)).addAttribute("ram_style", "Block")
  )

  // Weight Buffer
  val weightBufferArray =
    Array.tabulate(weightBufferH, weightBufferW, 2)((n, i, j) =>
      Mem(bufferWeightType, Seq.fill(weightBufferDepth)(weightType.apply().getZero)).addAttribute("ram_style", "Block")
    )

  // Write Buffer control
  val writeTokenAddr      = Counter(tokenBufferDepth)
  val tokenBufferedNumber = RegInit(U(0, 2 bits))
  val writeTokenMux       = RegInit(False).toggleWhen(writeTokenAddr.willOverflow)

  val writeWeightAddr      = Counter(weightBufferDepth)
  val weightBufferedNumber = RegInit(U(0, 2 bits))
  val writeWeightMux       = RegInit(False).toggleWhen(writeWeightAddr.willOverflow)

  // Read Buffer Control
  val readWeightStream   = Stream(bufferWeightBlockType)
  val readWeightAddr     = Counter(weightBufferDepth)
  val nextReadWeightAddr = UInt(readWeightAddr.value.getBitsWidth bits)
  val readWeightMux      = RegInit(False).toggleWhen(readWeightAddr.willOverflow)

  val readTokenStream   = Stream(tokenBlockType)
  val readTokenAddr     = Counter(tokenBufferDepth)
  val nextReadTokenAddr = UInt(readTokenAddr.value.getBitsWidth bits)
  val readTokenMux      = RegInit(False).toggleWhen(readTokenAddr.willOverflow && readWeightAddr.willOverflow)

  val peArrayDataIn = StreamJoin.vec(Seq(readTokenStream, readWeightStream))

  io.weightIn.ready.clear()

  // Write Token Buffer Logic
  switch(Cat(writeTokenAddr.willOverflow, readWeightAddr.willOverflow)) {
    is(B(2)) { tokenBufferedNumber := tokenBufferedNumber + U(1) }
    is(B(1)) { tokenBufferedNumber := tokenBufferedNumber - U(1) }
  }
  tokenBufferArray.zip(io.dataIn.payload).foreach { case (tokenBufferLines, dataInLine) =>
    tokenBufferLines.zip(dataInLine).foreach { case (tokenBuffers, dataIn) =>
      tokenBuffers.head.write(writeTokenAddr, dataIn, !writeTokenMux && io.dataIn.fire)
      tokenBuffers.last.write(writeTokenAddr, dataIn, writeTokenMux && io.dataIn.fire)
    }
  }

  switch(tokenBufferedNumber) {
    is(U(0), U(1)) { io.dataIn.ready.set() }
    is(U(2)) { io.dataIn.ready.clear() }
    default {
      io.dataIn.ready.clear()
    }
  }

  when(io.dataIn.fire) { writeTokenAddr.increment() }

  // Write Weight Buffer Logic

  switch(Cat(writeWeightAddr.willOverflow, readWeightAddr.willOverflow)) {
    is(B(2)) { weightBufferedNumber := weightBufferedNumber + U(1) }
    is(B(1)) { weightBufferedNumber := weightBufferedNumber - U(1) }
  }

  val parseWeightInPayload =
    io.weightIn.payload.map(
      _.grouped(2).toSeq.map { weights => weights.head @@ weights.last }
    )

  weightBufferArray.zip(parseWeightInPayload).foreach { case (weightBufferLines, weightInLine) =>
    weightBufferLines.zip(weightInLine).foreach { case (weightBuffers, weightIn) =>
      weightBuffers.head.write(writeWeightAddr, weightIn, !writeWeightMux && io.weightIn.fire)
      weightBuffers.last.write(writeWeightAddr, weightIn, writeWeightMux && io.weightIn.fire)
    }
  }

  switch(weightBufferedNumber) {
    is(U(0), U(1)) { io.weightIn.ready.set() }
    is(U(2)) { io.weightIn.ready.clear() }
  }

  when(io.weightIn.fire) { writeWeightAddr.increment() }

  // Read Token Buffer Logic
  val readTokenAddrIncrease = readTokenStream.fire
  when(readTokenAddrIncrease) { readTokenAddr.increment() }

  tokenBufferArray.zip(readTokenStream.payload).foreach { case (tokenBufferLines, tokenLine) =>
    tokenBufferLines.zip(tokenLine).foreach { case (tokenBuffers, token) =>
      token := Mux(
        readTokenMux,
        tokenBuffers.last.readSync(nextReadTokenAddr),
        tokenBuffers.head.readSync(nextReadTokenAddr)
      )
    }
  }

  val zeroTokenBufferedValid = RegInit(False)

  val nextReadTokenAddrRound = Mux(
    readTokenAddr.willOverflow,
    U(readTokenAddr.start, readTokenAddr.value.getBitsWidth bits),
    readTokenAddr.value + U(1)
  )

  val zeroTokenBufferedNextReadAddr = U(readTokenAddr.start, readTokenAddr.value.getBitsWidth bits)

  when(writeTokenAddr === readTokenAddr) {
    zeroTokenBufferedNextReadAddr := readTokenAddr.value
    zeroTokenBufferedValid.clear()
  }.elsewhen(writeTokenAddr === readTokenAddr.value + U(1)) {
    switch(Cat(io.dataIn.fire, peArrayDataIn.fire)) {
      is(B(0), B(2)) {
        zeroTokenBufferedNextReadAddr := readTokenAddr.value
        zeroTokenBufferedValid.set()
      }
      is(B(1), B(3)) {
        zeroTokenBufferedNextReadAddr := nextReadTokenAddrRound
        zeroTokenBufferedValid.clear()
      }
    }
  }.otherwise {
    zeroTokenBufferedNextReadAddr := Mux(peArrayDataIn.fire, nextReadTokenAddrRound, readTokenAddr.value)
    zeroTokenBufferedValid.set()
  }

  val oneAndTwoTokenBufferedNextReadAddr = Mux(peArrayDataIn.fire, nextReadTokenAddrRound, readTokenAddr.value)

  nextReadTokenAddr := Mux(
    tokenBufferedNumber === U(0),
    zeroTokenBufferedNextReadAddr,
    oneAndTwoTokenBufferedNextReadAddr
  )

  val waitNextPatchToken = RegInit(False)
  waitNextPatchToken
    .setWhen(
      !io.dataIn.fire && writeTokenAddr === U(0) && readTokenAddr.willOverflow && readWeightAddr.willOverflow
    )
    .clearWhen(io.dataIn.fire)

  val tokenBufferNumberZeroToOne = !tokenBufferedNumber(1).edge(False) && tokenBufferedNumber(0).rise(False)
  val tokenBufferNumberOneToZero = tokenBufferedNumber(0).fall(False)
  readTokenStream.valid := tokenBufferedNumber.mux(
    0 -> (zeroTokenBufferedValid && !waitNextPatchToken &&
      !(tokenBufferNumberOneToZero && writeTokenAddr.value === U(0))),
    1       -> Mux(tokenBufferNumberZeroToOne, zeroTokenBufferedValid, True),
    2       -> True,
    default -> False
  )

  // Read Weight Buffer Logic
  val readWeightAddrIncrease = readTokenAddr.willOverflow && readWeightStream.fire
  when(readWeightAddrIncrease) { readWeightAddr.increment() }
  weightBufferArray.zip(readWeightStream.payload).foreach { case (weightBufferLines, weightLine) =>
    weightBufferLines.zip(weightLine).foreach { case (weightBuffers, weight) =>
      weight := Mux(
        readWeightMux,
        weightBuffers.last.readSync(nextReadWeightAddr),
        weightBuffers.head.readSync(nextReadWeightAddr)
      )
    }
  }

  val zeroWeightBufferedValid = RegInit(False)

  val nextReadWeightAddrRound = Mux(
    readWeightAddr.willOverflow,
    U(readWeightAddr.start, readWeightAddr.value.getBitsWidth bits),
    readWeightAddr.value + U(1)
  )

  val zeroWeightBufferedNextReadAddr = U(readWeightAddr.start, readWeightAddr.value.getBitsWidth bits)

  when(writeWeightAddr === readWeightAddr) {
    zeroWeightBufferedNextReadAddr := readWeightAddr.value
    zeroWeightBufferedValid.clear()
  }.elsewhen(writeWeightAddr === readWeightAddr.value + U(1)) {
    switch(Cat(io.weightIn.fire, peArrayDataIn.fire)) {
      is(B(0), B(2)) {
        zeroWeightBufferedNextReadAddr := readWeightAddr.value
        zeroWeightBufferedValid.set()
      }
      is(B(1)) {
        zeroWeightBufferedNextReadAddr := Mux(
          readWeightAddr.willIncrement,
          nextReadWeightAddrRound,
          readWeightAddr.value
        )
        zeroWeightBufferedValid := Mux(readWeightAddr.willIncrement, False, True)
      }
      is(B(3)) {
        zeroWeightBufferedNextReadAddr := Mux(
          readWeightAddr.willIncrement,
          nextReadWeightAddrRound,
          readWeightAddr.value
        )
        zeroWeightBufferedValid := Mux(readWeightAddr.willIncrement, False, True)
      }
    }
  }.otherwise {
    zeroWeightBufferedNextReadAddr := Mux(readWeightAddr.willIncrement, nextReadWeightAddrRound, readWeightAddr.value)
    zeroWeightBufferedValid.set()
  }

  val oneAndTwoWeightBufferedNextReadAddr =
    Mux(readTokenAddr.willOverflow, nextReadWeightAddrRound, readWeightAddr.value)

  nextReadWeightAddr := Mux(
    weightBufferedNumber === U(0),
    zeroWeightBufferedNextReadAddr,
    oneAndTwoWeightBufferedNextReadAddr
  )

  val waitNextPatchWeight = RegInit(False)
  waitNextPatchWeight
    .setWhen(!io.weightIn.fire && writeWeightAddr === U(0) && readTokenAddr.willOverflow && readWeightAddr.willOverflow)
    .clearWhen(io.weightIn.fire)

  val weightBufferNumberZeroToOne = !weightBufferedNumber(1).edge(False) && weightBufferedNumber(0).rise(False)
  readWeightStream.valid := weightBufferedNumber.mux(
    0 -> (zeroWeightBufferedValid && !waitNextPatchWeight),
    1 -> Mux(
      weightBufferNumberZeroToOne,
      zeroWeightBufferedValid,
      True
    ),
    2       -> True,
    default -> False
  )

  val peArrayInstLoopCounter   = Counter(LoopComputePeriodNumber)
  val peArrayInstDataInCounter = Counter(eigenBufferDepth)
  val waitEigenMatrixOut       = RegInit(False)
  when(peArrayDataIn.fire) { peArrayInstDataInCounter.increment() }
  when(peArrayInstDataInCounter.willOverflow) { peArrayInstLoopCounter.increment() }

  val peArrayInst = PeTreeMult(dataWidth, AD0B1, config.sliceConfig, 2)

  // peArray data drive
  peArrayInst.io.aIn.zip(peArrayDataIn.payload.head).foreach { case (peLine, peLineToken) =>
    peLine.foreach { pe =>
      pe.zip(peLineToken).foreach { case (e, token) => e := token }
    }
  }

  peArrayInst.io.bIn.foreach { peLine =>
    peLine.zip(peArrayDataIn.payload.last.transpose).foreach { case (pe, peWeight) =>
      pe.zip(peWeight).foreach { case (e, weight) => e := weight }
    }
  }

  // peArray handshake logic
  val peArrayDataOut = Stream(HardType(peArrayInst.io.cOut))
  peArrayDataOut.payload := peArrayInst.io.cOut
  peArrayDataOut.valid   := peArrayDataIn.valid.d(peArrayInst.latency, False)

  val eigenMatrixBufferArray =
    Array.tabulate(eigenBufferH, eigenBufferW)((i, j) =>
      Mem(bufferEigenType, Seq.fill(eigenBufferDepth)(bufferEigenType.apply().getZero))
        .addAttribute("ram_style", "Block")
    )

  val eigenMatrixReadyCounter = Counter(LoopComputePeriodNumber)
  val writeEigenAddr          = Counter(eigenBufferDepth)
  val nextWriteEigenAddr      = UInt(log2Up(eigenBufferDepth + 1) bits)

  when(writeEigenAddr.willOverflow) { eigenMatrixReadyCounter.increment() }
  when(peArrayDataOut.fire) { writeEigenAddr.increment() }

  val nextWriteEigenAddrWhenTrue =
    Mux(
      writeEigenAddr.willOverflow,
      U(writeEigenAddr.start, writeEigenAddr.value.getBitsWidth bits),
      writeEigenAddr.value + U(1)
    )

  nextWriteEigenAddr := Mux(peArrayDataOut.fire, nextWriteEigenAddrWhenTrue, writeEigenAddr.value)

  val readEigenAddr     = Counter(eigenBufferDepth)
  val nextReadEigenAddr = UInt(log2Up(eigenBufferDepth + 1) bits)

  val nextReadEigenAddrWhenTrue =
    Mux(
      readEigenAddr.willOverflow,
      U(readEigenAddr.start, readEigenAddr.value.getBitsWidth bits),
      readEigenAddr.value + U(1)
    )

  nextReadEigenAddr := Mux(io.eigenMatrixOut.fire, nextReadEigenAddrWhenTrue, readEigenAddr.value)

  when(io.eigenMatrixOut.fire) { readEigenAddr.increment() }

  waitEigenMatrixOut
    .setWhen(peArrayInstLoopCounter.willOverflow)
    .clearWhen(waitEigenMatrixOut && readEigenAddr.willOverflow)
  peArrayDataIn.ready.set()
  peArrayDataIn.ready.clearWhen(waitEigenMatrixOut && peArrayInstDataInCounter === readEigenAddr)

  peArrayDataOut.ready.set()

  val parsePeArrayDataOut =
    peArrayDataOut.payload.map(
      _.flatMap(value => Seq(value.takeHigh(2 * dataWidth).asSInt, value.takeLow(2 * dataWidth).asSInt))
    )

  val lastBufferedEigenValueArray = Array.tabulate(eigenBufferH, eigenBufferW)((i, j) => bufferEigenType.apply())
  val lastValidEigenValue         = Array.tabulate(eigenBufferH, eigenBufferW)((i, j) => bufferEigenType.apply())
  val readLastAddr                = Mux(io.eigenMatrixOut.valid, nextReadEigenAddr, nextWriteEigenAddr)

  eigenMatrixBufferArray.zip(lastBufferedEigenValueArray).zip(lastValidEigenValue).foreach {
    case ((eigenMatrixLine, lastBufferLine), lastEigenLine) =>
      eigenMatrixLine.zip(lastBufferLine).zip(lastEigenLine).foreach { case ((ramPort, lastBuffer), lastEigen) =>
        val lastValue = ramPort.readSync(readLastAddr)
        lastBuffer := Mux(io.eigenMatrixOut.valid, bufferEigenType.apply().getZero, lastValue)
        lastEigen  := Mux(io.eigenMatrixOut.valid, lastValue, bufferEigenType.apply().getZero)
      }
  }

  eigenMatrixBufferArray.zip(parsePeArrayDataOut).zip(lastBufferedEigenValueArray).foreach {
    case ((bufferLine, eigenMatrixLine), lastLine) =>
      bufferLine.zip(eigenMatrixLine).zip(lastLine).foreach { case ((buffer, eigenValue), lastValue) =>
        buffer.write(
          writeEigenAddr,
          Mux(
            eigenMatrixReadyCounter.value === U(0),
            eigenValue.resize(bufferEigenType.getBitsWidth),
            (eigenValue +^ lastValue).takeHigh(bufferEigenType.getBitsWidth).asSInt
          ),
          peArrayDataOut.fire
        )
      }
  }

  io.eigenMatrixOut.valid := RegInit(False)
    .setWhen(eigenMatrixReadyCounter.willOverflow)
    .clearWhen(readEigenAddr.willOverflow)

  io.eigenMatrixOut.payload.zip(lastValidEigenValue).foreach { case (eigenOutLine, lastValidEigenLine) =>
    eigenOutLine.zip(lastValidEigenLine).foreach { case (eigenOutValue, lastValidEigenValue) =>
      eigenOutValue := lastValidEigenValue.takeHigh(dataWidth).asSInt
    }
  }

}
