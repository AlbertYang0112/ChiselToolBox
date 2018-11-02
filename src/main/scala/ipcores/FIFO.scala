package ipcores

import chisel3._
import scala.math

/*
class FIFO(width: Int, depth: Int, isSync: Bool) extends Module{
  val io = IO(new Bundle{
    val readPort = Output(UInt(width.W))
    val writePort = Input(UInt(width.W))
    val readClock = Input(Bool())
    val writeClock = Input(Bool())
    val empty = Output(Bool())
    val full = Output(Bool())
  })
  require(width >= 1, "FIFO's width must >= 1.")
  require(depth >= 1, "FIFO's depth must >= 1.")
  val mem = Vec(depth, RegInit(0.U(width.W)))
  val addrBits:Int = Integral(Math.ceil(Math.log(depth) / Math.log(2)) + 1)
  val readAddr = RegInit(0.U(addrBits.W))
  val writeAddr = RegInit(0.U(addrBits.W))
  def iteratePointer(x:UInt):UInt = Mux(readAddr === depth.U, 0.U, readAddr + 1.U)

}
*/
