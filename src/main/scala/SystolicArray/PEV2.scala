package SystolicArray

import chisel3._
import chisel3.util.{DeqIO, EnqIO, Queue}

class PEV2(
                         dataBits: Int,
                         resultBufferDepth: Int
                       ) extends Module {
  val io = IO(new PEBundle(dataBits))

  // Pipeline Buffers
  val dataBuffer = RegNext(io.in.data.bits)
  val weightBuffer = RegNext(io.in.weight.bits)
  // val resultBuffer = RegNext(io.in.result.bits)
  val controlBuffer = RegNext(io.in.control.bits)

  val queueIn = Wire(EnqIO(UInt(dataBits.W)))
  val queueOut = Wire(DeqIO(UInt(dataBits.W)))
  val dataValid = RegNext(io.in.data.valid)
  val weightValid = RegNext(io.in.weight.valid)
  val resultValid = RegNext(io.in.result.valid)
  val controlValid = RegNext(io.in.control.valid)

  val PERunning = RegInit(false.B)

  val partialSum = RegInit(0.U(dataBits.W))
  val mulAddResult = Wire(UInt(dataBits.W))
  val addRhs = Mux(io.in.control.bits, 0.U(dataBits.W), partialSum)
  mulAddResult := io.in.data.bits * io.in.weight.bits + addRhs
  partialSum := mulAddResult
  queueIn.bits := partialSum
  queueIn.valid := io.in.control.bits

  queueOut <> Queue(queueIn, resultBufferDepth)
  queueOut.ready := io.out.result.ready

  io.out.weight.bits := weightBuffer
  io.out.data.bits := dataBuffer
  io.out.result.bits := Mux(queueOut.valid, queueOut.bits, io.in.result.bits)
  io.out.control.bits := controlBuffer

  io.out.data.valid := dataValid
  io.out.weight.valid := weightValid
  io.out.result.valid := queueOut.valid | resultValid

  io.out.control.valid := controlValid
  io.in.weight.ready := true.B
  io.in.data.ready := true.B
  io.in.result.ready := !queueOut.valid
  io.in.control.ready := true.B

}

