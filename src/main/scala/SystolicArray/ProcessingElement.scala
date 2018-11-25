package SystolicArray

import chisel3._
import chisel3.util.{DeqIO, EnqIO, Queue}

class PEBundleOutput(val dataBits: Int) extends Bundle {
  val data     = EnqIO(UInt(dataBits.W))
  val weight   = EnqIO(UInt(dataBits.W))
  val result   = EnqIO(UInt(dataBits.W))
  val control  = EnqIO(Bool())
}

class PEBundle(val dataBits: Int) extends Bundle {
  val in = Flipped(new PEBundleOutput(dataBits))
  val out = new PEBundleOutput(dataBits)
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
  val dataBuffer = RegNext(io.in.data.bits)
  val weightBuffer = RegNext(io.in.weight.bits)
  val resultBuffer = RegInit(0.U(dataBits.W))
  val controlBuffer = RegNext(io.in.control.bits)
  val queueIn = Wire(EnqIO(UInt(dataBits.W)))
  val queueOut = Wire(DeqIO(UInt(dataBits.W)))
  val dataValid = RegNext(io.in.data.valid)
  val weightValid = RegNext(io.in.weight.valid)
  val resultValid = RegInit(false.B)
  val controlValid = RegNext(io.in.control.valid)
  val select = RegNext(io.in.control.bits)
  val PERunning = RegInit(false.B)
  val queueRun = Wire(Bool())
  val partialSum = Wire(UInt(dataBits.W))
  queueRun := io.in.weight.valid & io.in.data.valid
  queueOut <> Queue(queueIn, resultBufferDepth)
  queueOut.ready := Mux(select, queueRun, io.out.result.ready)
  queueIn.valid := queueRun
  partialSum := Mux(select, queueOut.bits, 0.U(dataBits.W))
  io.out.weight.bits := weightBuffer
  io.out.data.bits := dataBuffer
  io.out.result.bits := Mux(select, io.in.result.bits, queueOut.bits)
  io.out.control.bits := controlBuffer
  io.out.data.valid := dataValid
  io.out.weight.valid := weightValid
  io.out.result.valid := Mux(select, io.in.result.valid, queueOut.valid)
  // io.resultOut.valid := resultValid
  io.out.control.valid := controlValid
  io.in.weight.ready := true.B
  io.in.data.ready := true.B
  io.in.result.ready := true.B
  io.in.control.ready := true.B
  // resultValid := Mux(select, io.resultIn.valid, queueOut.valid)
  queueIn.bits := io.in.data.bits * io.in.weight.bits + partialSum
}
