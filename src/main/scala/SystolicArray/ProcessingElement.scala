package SystolicArray

import chisel3._
import chisel3.util.{DeqIO, EnqIO, Queue}

class PEBundle(dataBits: Int) extends Bundle {
  val dataIn      = DeqIO(UInt(dataBits.W))
  val dataOut     = EnqIO(UInt(dataBits.W))
  val weightIn    = DeqIO(UInt(dataBits.W))
  val weightOut   = EnqIO(UInt(dataBits.W))
  val resultIn    = DeqIO(UInt(dataBits.W))
  val resultOut   = EnqIO(UInt(dataBits.W))
  val controlIn   = DeqIO(Bool())
  val controlOut  = EnqIO(Bool())
}

trait PEMode {
  val Running = 0.U
  val Outputing = 1.U
}

class ProcessingElement(
                         dataBits: Int,
                         resultBufferDepth: Int
                       ) extends Module {
  val io = IO(new PEBundle(dataBits))
  val dataBuffer = RegNext(io.dataIn.bits)
  val weightBuffer = RegNext(io.weightIn.bits)
  val resultBuffer = RegInit(0.U(dataBits.W))
  val controlBuffer = RegNext(io.controlIn.bits)
  val queueIn = Wire(EnqIO(UInt(dataBits.W)))
  val queueOut = Wire(DeqIO(UInt(dataBits.W)))
  val dataValid = RegNext(io.dataIn.valid)
  val weightValid = RegNext(io.weightIn.valid)
  val resultValid = RegInit(false.B)
  val controlValid = RegNext(io.controlIn.valid)
  val select = RegNext(io.controlIn.bits)
  val PERunning = RegInit(false.B)
  val queueRun = Wire(Bool())
  queueRun := weightValid & dataValid | select
  queueOut <> Queue(queueIn, resultBufferDepth)
  queueOut.ready := queueRun
  queueIn.valid := queueRun
  io.weightOut.bits := weightBuffer
  io.dataOut.bits := dataBuffer
  io.resultOut.bits := Mux(select, io.resultIn.bits, queueOut.bits)
  io.controlOut.bits := controlBuffer
  io.dataOut.valid := dataValid
  io.weightOut.valid := weightValid
  io.resultOut.valid := resultValid
  io.controlOut.valid := controlValid
  io.weightIn.ready := true.B
  io.dataIn.ready := true.B
  io.resultIn.ready := true.B
  io.controlIn.ready := true.B
  resultValid := Mux(select, io.resultIn.valid, queueOut.valid)
  queueIn.bits := io.dataIn.bits * io.weightIn.bits + queueOut.bits
}
