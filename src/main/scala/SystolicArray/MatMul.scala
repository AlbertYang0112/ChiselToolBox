package SystolicArray

import chisel3._
import chisel3.util.{Cat, DeqIO, EnqIO}

class inputController(channels: Int, dataBits: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(channels, Flipped(new PEBundleOutput(dataBits)))
    val out = Vec(channels, new PEBundleOutput(dataBits))
    // val in = List.fill(channels)(Flipped(new PEBundleOutput(dataBits)))
    // val out = List.fill(channels)(new PEBundleOutput(dataBits))
    val run = Input(Bool())
  })

  def bundleAllReady(bundle: PEBundleOutput): Bool =
    bundle.weight.ready & bundle.data.ready & bundle.result.ready & bundle.control.ready
  def bundleAllValid(bundle: PEBundleOutput): Bool =
    bundle.weight.valid & bundle.data.valid & bundle.result.valid & bundle.control.valid

  val inReady = Wire(Bool())
  val outReady = Wire(Bool())
  val allReady = Wire(Bool())
  inReady := Cat(io.in.map(bundleAllValid(_))).andR()
  outReady := Cat(io.out.map(bundleAllReady(_))).andR()
  allReady := inReady & outReady

  val readyBuffer = List.fill(channels + 1)(RegInit(false.B))
  for(chan <- 0 until channels) {
    // Link the ready buffer to the IO.
    io.in(chan).data.ready := readyBuffer(chan)
    io.in(chan).weight.ready := readyBuffer(chan)
    io.in(chan).control.ready := readyBuffer(chan)
    io.in(chan).result.ready := readyBuffer(chan)
    // io.out(chan).data.valid := readyBuffer(chan + 1) & io.in(chan).data.valid
    // io.out(chan).weight.valid := readyBuffer(chan + 1) & io.in(chan).weight.valid
    // io.out(chan).control.valid := readyBuffer(chan + 1) & io.in(chan).control.valid
    io.out(chan).data.valid := readyBuffer(chan + 1) & readyBuffer(chan)
    io.out(chan).weight.valid := readyBuffer(chan + 1) & readyBuffer(chan)
    io.out(chan).control.valid := readyBuffer(chan + 1) & readyBuffer(chan)
    // io.out(chan).result.valid := readyBuffer(chan) & io.in(chan).result.valid
    io.out(chan).result.valid := false.B
  }
  for(chan <- 0 until channels) {
    // Construct the register chain.
    readyBuffer(chan + 1) := readyBuffer(chan)
  }
  when(allReady & io.run) {
    readyBuffer.head := true.B
  } .otherwise {
    readyBuffer.head := false.B
  }

  for(chan <- 0 until channels) {
    // Link other wires.
    io.out(chan).data.bits := io.in(chan).data.bits
    io.out(chan).weight.bits := io.in(chan).weight.bits
    io.out(chan).result.bits := io.in(chan).result.bits
    // io.out(chan).control.bits := io.in(chan).control.bits
    io.out(chan).control.bits := io.out(chan).data.valid
  }
}

class MatMul(dataBits: Int, n: Int) extends Module {
  val io = IO(new Bundle{
    val ioArray = Vec(n, new PEBundle(dataBits))
    //val ioArray = List.fill(n)(new PEBundle(dataBits))
    val run = Input(Bool())
  })
  private val PEArray = List.fill(n, n)(Module(new ProcessingElement(dataBits, 2)))
  private val InputController = Module(new inputController(n, dataBits))
  InputController.io.run := io.run
  for(chan <- 0 until n) {
    InputController.io.in(chan).control <> io.ioArray(chan).in.control
    InputController.io.in(chan).data <> io.ioArray(chan).in.data
    InputController.io.in(chan).result <> io.ioArray(chan).in.result
    InputController.io.in(chan).weight <> io.ioArray(chan).in.weight
  }
  // PE interconnect
  for(row <- 1 until n) {
    for(col <- 1 until n) {
      PEArray(row)(col).io.in.data <> PEArray(row)(col - 1).io.out.data
      PEArray(row)(col).io.in.result <> PEArray(row)(col - 1).io.out.result
      PEArray(row)(col).io.in.control <> PEArray(row)(col - 1).io.out.control
      PEArray(row)(col).io.in.weight <> PEArray(row - 1)(col).io.out.weight
    }
  }
  // Boundary PE
  // Data, result, control--Horizontal
  for(row <- 0 until n) {
    PEArray(row).head.io.in.data <> InputController.io.out(row).data
    PEArray(row).head.io.in.control <> InputController.io.out(row).control
    PEArray(row).head.io.in.result <> InputController.io.out(row).result
    io.ioArray(row).out.data <> PEArray(row).last.io.out.data
    io.ioArray(row).out.control <> PEArray(row).last.io.out.control
    io.ioArray(row).out.result <> PEArray(row).last.io.out.result
  }
  for(col <- 1 until n) {
    PEArray.head(col).io.in.control <> PEArray.head(col - 1).io.out.control
    PEArray.head(col).io.in.data <> PEArray.head(col - 1).io.out.data
    PEArray.head(col).io.in.result <> PEArray.head(col - 1).io.out.result
  }
  // Weight--Vertical
  for(col <- 0 until n) {
    PEArray.head(col).io.in.weight <> InputController.io.out(col).weight
    io.ioArray(col).out.weight <> PEArray.last(col).io.out.weight
  }
  for(row <- 1 until n) {
    PEArray(row).head.io.in.weight <> PEArray(row - 1).head.io.out.weight
  }
}
