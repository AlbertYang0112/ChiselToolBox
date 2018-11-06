package ipcores

import chisel3._
import chisel3.core.withClock
import scala.math

class ClockDivider(div: Int) extends Module {
  val io = IO(new Bundle {
    val clockIn = Input(Bool())
    val clockOut = Output(Bool())
  })
  if(div % 2 == 1){
    val mod: Int = div / 2 + 1
    val bits = (math.ceil(math.log(mod) / math.log(2))).toInt
    println("Mod: " + mod + "  CounterBits: " + bits)
    val flipSig = Wire(Bool())
    val halfClock = Mux(flipSig, !io.clockIn, io.clockIn)
    val trigger = Wire(Bool())
    val fullCounter = RegInit(0.U(2.W))
    withClock(halfClock.asClock()) {
      val flip = RegInit(false.B)
      val halfCounter = RegInit(0.U(bits.W))
      halfCounter := Mux(halfCounter =/= (mod - 1).U, halfCounter + 1.U, 0.U)
      trigger := halfCounter === (mod - 1).U
      flipSig := flip
      when(halfCounter === 0.U) {
        flip := !flip
      }
    }
    when(trigger) {
      fullCounter := fullCounter + 1.U
    }
    io.clockOut := fullCounter(1)
  } else {
    val mod: Int = div / 2
    val bits = (math.log(mod) / math.log(2)).toInt
    val counter = RegInit(0.U(bits.W))
    val output = RegInit(false.B)
    when(counter === (mod - 1).U) {
      counter := 0.U
      output := !output
    } .otherwise {
      counter := counter + 1.U
    }
    io.clockOut := output
  }

}

class ClockDividerThree extends Module {
  val io = IO(new Bundle {
    val clockIn = Input(Bool())
    val clockOut = Output(Bool())
  })
  val fullCounter = RegInit(false.B)
  val setSig = Wire(Bool())
  val clrSig = Wire(Bool())
  val flipSig = Wire(Bool())
  val halfClock = Mux(flipSig, !io.clockIn, io.clockIn)
  val testCounter = Wire(Bool())
  withClock(halfClock.asClock()) {
    val flip = RegInit(false.B)
    val halfCounter = RegInit(false.B)
    flipSig := flip
    testCounter := halfCounter
    halfCounter := halfCounter + 1.U
    setSig := halfCounter === 1.U
    clrSig := halfCounter === 0.U
    when(clrSig) {
      flip := !flip
    }
  }
  when(setSig) {
    fullCounter := fullCounter + 1.U
  }
  io.clockOut := fullCounter


}
