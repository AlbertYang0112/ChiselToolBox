package ipcores

import chisel3._
import chisel3.core.{withClock, withClockAndReset}
import chisel3.util.{Cat, HasBlackBoxInline}

class SyncFromLowFreq(level: Int, width: Int) extends Module {
  val io = IO(new Bundle{
    val src = Input(UInt(width.W))
    val dst = Output(UInt(width.W))
    val highFreqDstClock = Input(Bool())
  })
  withClock(io.highFreqDstClock.asClock()) {
    //val flipflopVec =flipflopVec Vec(level, RegInit(0.U(width.W)))
    val flipflopVec = List.fill(level)(RegInit(0.U(width.W)))
    var i: Int = 0
    flipflopVec(0) := io.src
    for (i <- 0 to level - 2) {
      flipflopVec(i + 1) := flipflopVec(i)
    }
    io.dst := flipflopVec(level - 1)
  }
}

class FlipFlopWithAsyncReset extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle() {
    val in = Input(Bool())
    val out = Output(Bool())
    val clk = Input(Bool())
    val clr = Input(Bool())
  })
  setInline("FlipFlopWithAsyncReset.v",
    s"""
       |module FlipFlopWithAsyncReset(
       |  input in,
       |  output out,
       |  input clk,
       |  input clr
       |);
       |  reg ff = 1'b0;
       |  assign out = ff;
       |  always @(posedge clk or posedge clr) begin
       |    if(clr)
       |      ff <= 1'b0;
       |    else
       |      ff <= in;
       |  end
       |endmodule
     """.stripMargin)
}

class SyncFromHighFreqSingle(level: Int) extends Module {
  val io = IO(new Bundle{
    val asyncIn = Input(UInt(1.W))
    val syncOut = Output(UInt(1.W))
    val lowFreqDstClock = Input(Bool())
  })
  val clear = (~io.asyncIn).asUInt() & io.syncOut
  val conn = Wire(Bool())
  val asyncClock = (io.asyncIn | clear)(0)
  val asyncFlipFlop = Module(new FlipFlopWithAsyncReset)
  asyncFlipFlop.io.in := 1.U
  asyncFlipFlop.io.clk := io.asyncIn
  asyncFlipFlop.io.clr := clear(0)
  conn := asyncFlipFlop.io.out
  withClock(io.lowFreqDstClock.asClock()) {
    val flipflopVec = List.fill(level)(RegInit(0.U(1.W)))
    var i: Int = 0
    for (i <- 0 to level - 2) {
      flipflopVec(i + 1) := flipflopVec(i)
    }
    flipflopVec(0) := conn
    io.syncOut := flipflopVec(level - 1)
  }
}

class SyncFromHighFreq(level: Int, width: Int) extends Module {
  val io = IO(new Bundle{
    val asyncIn = Input(UInt(width.W))
    val syncOut = Output(UInt(width.W))
    val lowFreqDstClock = Input(Bool())
  })
  var i:Int = 0
  val singleModules = List.fill(width)(Module(new SyncFromHighFreqSingle(level)))
  for(i <- 0 until width) {
    singleModules(i).io.lowFreqDstClock := io.lowFreqDstClock
    singleModules(i).io.asyncIn := io.asyncIn(i)
  }
  io.syncOut := Cat(singleModules.map(_.io.syncOut).reverse)
}


