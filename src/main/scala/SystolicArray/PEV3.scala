package SystolicArray

import chisel3._
import chisel3.util.{DeqIO, EnqIO, Queue}

class ControlBundle extends Bundle {
  val outputSum = Bool()
  val calculate = Bool()
  val clearSum = Bool()
}

class PEBundleOutputV2(
                        val dataWidth: Int,
                        val weightWidth: Int
                      ) extends Bundle {
  val data = EnqIO(UInt(dataWidth.W))
  val weight = EnqIO(UInt(weightWidth.W))
  val result = EnqIO(UInt((dataWidth + weightWidth).W))
  val control = EnqIO(new ControlBundle)
}

class PEBundleV2(
                val dataWidth: Int,
                val weightWidth: Int
                ) extends Bundle {
  val in = Flipped(new PEBundleOutputV2(dataWidth, weightWidth))
  val out = new PEBundleOutputV2(dataWidth, weightWidth)
}

trait ControlBit {
  val CONTROL_WIDTH = 3
  val OUTPUT_SUM = 2
  val CALCULATE = 1
  val CLEAR_SUM = 0
}

class PEV3(
            dataWidth: Int,
            weightWidth: Int,
            resultBufferDepth: Int
          ) extends Module with ControlBit {
  val io = IO(new PEBundleV2(dataWidth, weightWidth))

  // Pipeline Buffers
  val dataBuffer = RegInit(0.U(dataWidth.W))
  val weightBuffer = RegInit(0.U(weightWidth.W))
  val resultWidth = dataWidth + weightWidth
  //val resultBuffer = RegNext(0.U(dataBits.W))
  //val controlBuffer = RegInit(0.U(CONTROL_WIDTH.W))
  val controlCalculateBuffer = RegInit(false.B)
  val controlOutputSumBuffer = RegInit(false.B)
  val controlClearSumBuffer = RegInit(true.B)

  val queueIn = Wire(EnqIO(UInt(resultWidth.W)))
  val queueOut = Wire(DeqIO(UInt(resultWidth.W)))
  val dataValid = RegInit(false.B)
  val weightValid = RegInit(false.B)
  val resultValid = RegInit(false.B)
  //val controlValid = RegInit(false.B)

  val partialSum = RegInit(0.U(resultWidth.W))
  val mulAddResult = Wire(UInt(resultWidth.W))
  val addRhs = Mux(io.in.control.bits.clearSum, 0.U(resultWidth.W), partialSum)
  val inputValid = io.in.data.valid & io.in.weight.valid & io.in.control.valid
  val outputReady = io.out.data.ready & io.out.weight.ready & io.out.control.ready
  mulAddResult := io.in.data.bits * io.in.weight.bits + addRhs
  when(io.in.control.valid) {
    when(io.in.control.bits.calculate) {
      partialSum := mulAddResult
    } .elsewhen(io.in.control.bits.clearSum) {
      partialSum := 0.U
    }
  }
//  partialSum := Mux(io.in.control.bits.calculate & io.in.control.valid,
//    mulAddResult, partialSum)
  queueIn.bits := partialSum
  //queueIn.valid := io.in.control.bits & inputValid
  queueIn.valid := io.in.control.bits.outputSum & io.in.control.valid

  queueOut <> Queue(queueIn, resultBufferDepth)
  queueOut.ready := io.out.result.ready

  io.out.weight.bits := weightBuffer
  io.out.data.bits := dataBuffer
  io.out.result.bits := Mux(queueOut.valid, queueOut.bits, io.in.result.bits)
  //io.out.control.bits := controlBuffer
  //io.out.control.bits := io.in.control.bits
  io.out.control.bits.clearSum := controlClearSumBuffer
  io.out.control.bits.calculate := controlCalculateBuffer
  io.out.control.bits.outputSum := controlOutputSumBuffer

  io.out.data.valid := dataValid
  io.out.weight.valid := weightValid
  //io.out.control.valid := controlValid
  io.out.control.valid := io.in.control.valid
  io.out.result.valid := queueOut.valid | io.in.result.valid

  io.in.weight.ready := io.out.weight.ready
  io.in.data.ready := io.out.data.ready
  io.in.result.ready := !queueOut.valid
  //io.in.control.ready := io.out.control.fire() | !controlValid
  io.in.control.ready := io.out.control.ready

  when(io.out.data.ready) {
    dataValid := io.in.data.valid
    dataBuffer := io.in.data.bits
  }

  when(io.out.weight.ready) {
    weightValid := io.in.weight.valid
    weightBuffer := io.in.weight.bits
  }

  when(io.out.control.ready) {
    //controlValid := io.in.control.valid
    //controlBuffer := io.in.control.bits
    controlCalculateBuffer := io.in.control.bits.calculate
    controlOutputSumBuffer := io.in.control.bits.outputSum
    controlClearSumBuffer := io.in.control.bits.clearSum
  }

  when(queueOut.valid | io.in.result.valid) {
    resultValid := true.B
  } .elsewhen(io.out.result.fire()) {
    resultValid := false.B
  }

}

