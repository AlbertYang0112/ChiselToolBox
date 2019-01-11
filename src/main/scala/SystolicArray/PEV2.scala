package SystolicArray

import chisel3._
import chisel3.util.{DeqIO, EnqIO, Queue}

class PEV2(
                         dataBits: Int,
                         resultBufferDepth: Int
                       ) extends Module {
  val io = IO(new PEBundle(dataBits))

  // Pipeline Buffers
  val dataBuffer = RegInit(0.U(dataBits.W))
  val weightBuffer = RegInit(0.U(dataBits.W))
  //val resultBuffer = RegNext(0.U(dataBits.W))
  val controlBuffer = RegInit(0.U(dataBits.W))

  val queueIn = Wire(EnqIO(UInt(dataBits.W)))
  val queueOut = Wire(DeqIO(UInt(dataBits.W)))
  val dataValid = RegInit(false.B)
  val weightValid = RegInit(false.B)
  val resultValid = RegInit(false.B)
  val controlValid = RegInit(false.B)

  val partialSum = RegInit(0.U(dataBits.W))
  val mulAddResult = Wire(UInt(dataBits.W))
  val addRhs = Mux(io.in.control.bits, 0.U(dataBits.W), partialSum)
  val inputValid = io.in.data.valid & io.in.weight.valid & io.in.control.valid
  val outputReady = io.out.data.ready & io.out.weight.ready & io.out.control.ready
  mulAddResult := io.in.data.bits * io.in.weight.bits + addRhs
  partialSum := Mux(io.out.data.ready & io.out.weight.ready & io.in.control.valid,
    mulAddResult, partialSum)
  queueIn.bits := partialSum
  queueIn.valid := io.in.control.bits & inputValid

  queueOut <> Queue(queueIn, resultBufferDepth)
  queueOut.ready := io.out.result.ready

  io.out.weight.bits := weightBuffer
  io.out.data.bits := dataBuffer
  io.out.result.bits := Mux(queueOut.valid, queueOut.bits, io.in.result.bits)
  io.out.control.bits := controlBuffer

  io.out.data.valid := dataValid
  io.out.weight.valid := weightValid
  io.out.control.valid := controlValid
  io.out.result.valid := queueOut.valid | io.in.result.valid

  io.in.weight.ready := io.out.weight.fire() | !weightValid
  io.in.data.ready := io.out.data.fire() | !dataValid
  io.in.result.ready := !queueOut.valid
  io.in.control.ready := io.out.control.fire() | !controlValid

  when(io.in.data.fire()) {
    dataValid := true.B
    dataBuffer := io.in.data.bits
  } .elsewhen(io.out.data.fire()) {
    dataValid := false.B
    dataBuffer := 0.U(dataBits.W)
  }
  when(io.in.weight.fire()) {
    weightValid := true.B
    weightBuffer := io.in.weight.bits
  } .elsewhen(io.out.weight.fire()) {
    weightValid := false.B
    weightBuffer := 0.U(dataBits.W)
  }
  when(io.in.control.fire()) {
    controlValid := true.B
    controlBuffer := io.in.control.bits
  } .elsewhen(io.out.control.fire()) {
    controlValid := false.B
    controlBuffer := 0.U(dataBits.W)
  }

  when(queueOut.valid | io.in.result.valid) {
    resultValid := true.B
  } .elsewhen(io.out.result.fire()) {
    resultValid := false.B
  }

}

