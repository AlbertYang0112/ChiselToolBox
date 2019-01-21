package SystolicArray

import chisel3._
import chisel3.util.{DeqIO, EnqIO, Queue}

class PEBundleOutputV2(
                        val dataBits: Int,
                        val controlBits: Int
                      ) extends Bundle {
  val data = EnqIO(UInt(dataBits.W))
  val weight = EnqIO(UInt(dataBits.W))
  val result = EnqIO(UInt(dataBits.W))
  val control = EnqIO(UInt(controlBits.W))
}

class PEBundleV2(
                val dataBits: Int,
                val controlBits: Int
                ) extends Bundle {
  val in = Flipped(new PEBundleOutputV2(dataBits, controlBits))
  val out = new PEBundleOutputV2(dataBits, controlBits)
}

trait ControlBit {
  val CONTROL_WIDTH = 3
  val OUTPUT_SUM = 2
  val CALCULATE = 1
  val CLEAR_SUM = 0
}

class PEV3(
            dataBits: Int,
            resultBufferDepth: Int
          ) extends Module with ControlBit {
  val io = IO(new PEBundleV2(dataBits, CONTROL_WIDTH))

  // Pipeline Buffers
  val dataBuffer = RegInit(0.U(dataBits.W))
  val weightBuffer = RegInit(0.U(dataBits.W))
  //val resultBuffer = RegNext(0.U(dataBits.W))
  //val controlBuffer = RegInit(0.U(dataBits.W))

  val queueIn = Wire(EnqIO(UInt(dataBits.W)))
  val queueOut = Wire(DeqIO(UInt(dataBits.W)))
  val dataValid = RegInit(false.B)
  val weightValid = RegInit(false.B)
  val resultValid = RegInit(false.B)
  //val controlValid = RegInit(false.B)

  val partialSum = RegInit(0.U(dataBits.W))
  val mulAddResult = Wire(UInt(dataBits.W))
  val addRhs = Mux(io.in.control.bits(CLEAR_SUM), 0.U(dataBits.W), partialSum)
  val inputValid = io.in.data.valid & io.in.weight.valid & io.in.control.valid
  val outputReady = io.out.data.ready & io.out.weight.ready & io.out.control.ready
  mulAddResult := io.in.data.bits * io.in.weight.bits + addRhs
  partialSum := Mux(io.in.control.bits(CALCULATE),
    mulAddResult, partialSum)
  queueIn.bits := partialSum
  //queueIn.valid := io.in.control.bits & inputValid
  queueIn.valid := io.in.control.bits(OUTPUT_SUM)

  queueOut <> Queue(queueIn, resultBufferDepth)
  queueOut.ready := io.out.result.ready

  io.out.weight.bits := weightBuffer
  io.out.data.bits := dataBuffer
  io.out.result.bits := Mux(queueOut.valid, queueOut.bits, io.in.result.bits)
  //io.out.control.bits := controlBuffer
  io.out.control.bits := io.in.control.bits

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

//  when(io.out.control.ready) {
//    controlValid := io.in.control.valid
//    controlBuffer := io.in.control.bits
//  }

  when(queueOut.valid | io.in.result.valid) {
    resultValid := true.B
  } .elsewhen(io.out.result.fire()) {
    resultValid := false.B
  }

}

