package ipcores

import AXI4.{AXI4Bundle, AXI4Parameter}
import chisel3._
import chisel3.core.{withClock, withClockAndReset}
import chisel3.util.Cat
import chisel3.util.{BitPat, Irrevocable, ListLookup, Lookup}
import scala.math

/*
class UART(axiParam: AXI4Parameter,
           baud: Int,
           ClockFreq: Int) extends Module {
  val io = IO(new Bundle{
    val axi = Flipped(Irrevocable(new AXI4Bundle(axiParam)))
    val tx = Output(Bool())
    val rx = Input(Bool())
  })
  require(bufferSize >= 1, "UART's buffer size must be larger than 1.")

}
*/
trait UARTReceiverStates {
  val stateWidth = 4
  val Idle = 0.U(stateWidth.W)
  val IdentifyingHead = 1.U(stateWidth.W)
  val Receiving = 2.U(stateWidth.W)
  val ReceivingStopBit = 3.U(stateWidth.W)
  val ReceiveDone = 4.U(stateWidth.W)
  val FormatError = 5.U(stateWidth.W)
}

class UARTReceiver(byteLength: Int) extends Module with UARTReceiverStates {
  // Todo: Add the odd/even parity check
  val io = IO(new Bundle{
    val scanClock = Input(Bool())
    val rx = Input(Bool())
    val recvDone = Output(Bool())
    val readDone = Input(Bool())
    val recvData = Output(UInt(byteLength.W))
  })
  require(byteLength >= 5 && byteLength <= 9, "UART can only deal with byte length within 6~9")
  val bitClock = Wire(Bool())
  val receiverClock = Wire(Bool())
  val bitCountFull = Wire(Bool())
  val bitCountEmpty = Wire(Bool())
  def writeOneBitToBuffer(buffer:UInt, bit:Bool):UInt = Cat(buffer, bit)
  val stopBit = Wire(Bool())
  // Wires for debug
  val testPulseCounter = Wire(UInt(4.W))
  val testState = Wire(UInt(stateWidth.W))
  val testBitCount = Wire(UInt(4.W))

  withClock(io.scanClock.asClock()) {
    val receiverState = RegInit(Idle)
    val pulseCounter = RegInit(0.U(4.W))
    val activeByteReceive = receiverState === Receiving || receiverState === ReceivingStopBit
    receiverClock := Mux(activeByteReceive, bitClock, true.B)
    bitClock := pulseCounter(3)
    testPulseCounter := pulseCounter
    testState := receiverState
    io.recvDone := receiverState === ReceiveDone
    when(receiverState === Idle) {
      when(!io.rx) {
        receiverState := IdentifyingHead
        pulseCounter := 1.U
      }
    } .elsewhen(receiverState === IdentifyingHead) {
      when(io.rx) {
        receiverState := Idle
      } .elsewhen(pulseCounter(3)) {
        receiverState := Receiving
      } .otherwise {
        pulseCounter := pulseCounter + 1.U
      }
    } .elsewhen(receiverState === Receiving) {
      when(bitCountFull) {
        receiverState := ReceivingStopBit
      } .otherwise {
        pulseCounter := pulseCounter + 1.U
      }
    } .elsewhen(receiverState === ReceivingStopBit) {
      when(bitCountEmpty) {
        receiverState := Mux(stopBit, FormatError, ReceiveDone)
      } .otherwise {
        pulseCounter := pulseCounter + 1.U
      }
    }.elsewhen(receiverState === ReceiveDone) {
      when(io.readDone) {
        receiverState := Idle
      }
    }
  }
  withClock(receiverClock.asClock()) {
    val bitCount = RegInit(0.U(4.W))
    val recvBuffer = RegInit(0.U((byteLength + 1).W))
    recvBuffer := writeOneBitToBuffer(recvBuffer, io.rx)
    bitCount := Mux(bitCount =/= byteLength.U, bitCount + 1.U, 0.U)
    io.recvData := recvBuffer(byteLength,1)
    stopBit := recvBuffer(0)
    bitCountFull := bitCount === byteLength.U
    bitCountEmpty := bitCount === 0.U
    testBitCount := bitCount
  }
}
