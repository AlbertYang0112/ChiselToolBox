package ipcores

import chisel3._
import chisel3.core.withClock
import chisel3.util.Cat
import ipcores.ParityBit.ParityBit


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
object ParityBit extends Enumeration {
  type ParityBit = Value
  val NoParityCheck, EvenParityCheck, OddParityCheck = Value
}

trait UARTReceiverStates {
  val stateWidth = 4
  val Idle = 0.U(stateWidth.W)
  val IdentifyingHead = 1.U(stateWidth.W)
  val Receiving = 2.U(stateWidth.W)
  val ReceivingStopBit = 3.U(stateWidth.W)
  val ReceiveDone = 4.U(stateWidth.W)
  val FormatError = 5.U(stateWidth.W)
}

class UARTReceiver(byteLength: Int, parityCheck: ParityBit) extends Module with UARTReceiverStates {
  val io = IO(new Bundle{
    val scanClock = Input(Bool())
    val rx = Input(Bool())
    val recvDone = Output(Bool())
    val readDone = Input(Bool())
    val recvData = Output(UInt(byteLength.W))
  })
  require(byteLength >= 5 && byteLength <= 9, "UART can only deal with byte length within 6~9")
  val receivingBits = byteLength + 1 + (if(parityCheck != ParityBit.NoParityCheck) 1 else 0)
  val withParity = parityCheck != ParityBit.NoParityCheck
  val bitClock = Wire(Bool())
  val receiverClock = Wire(Bool())
  val bitCountFull = Wire(Bool())
  val bitCountEmpty = Wire(Bool())
  def writeOneBitToBuffer(buffer:UInt, bit:Bool):UInt = Cat(buffer, bit)
  val stopBit = Wire(Bool())
  val parityExpected = Wire(Bool())
  val parityReceived = Wire(Bool())

  withClock(io.scanClock.asClock()) {
    val receiverState = RegInit(Idle)
    val pulseCounter = RegInit(0.U(4.W))
    val activeByteReceive = receiverState === Receiving || receiverState === ReceivingStopBit
    receiverClock := Mux(activeByteReceive, bitClock, true.B)
    bitClock := pulseCounter(3)
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
        receiverState := Mux(stopBit || parityExpected =/= parityReceived, FormatError, ReceiveDone)
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
    val recvBuffer = RegInit(0.U(receivingBits.W))
    val parityExpectedReg = RegInit(0.U(1.W))
    recvBuffer := writeOneBitToBuffer(recvBuffer, io.rx)
    parityExpectedReg := Mux(bitCount =/= (receivingBits - 1).U, parityExpectedReg ^ io.rx, 0.U)
    bitCount := Mux(bitCount =/= (receivingBits - 1).U, bitCount + 1.U, 0.U)
    parityExpected := Mux((parityCheck == ParityBit.EvenParityCheck).B, ~parityExpectedReg, parityExpectedReg)
    parityReceived := Mux(withParity.B, recvBuffer(0), parityExpected)
    io.recvData := recvBuffer(receivingBits - 1, if(withParity) 2 else 1)
    stopBit := recvBuffer(0)
    bitCountFull := bitCount === (receivingBits - 1).U
    bitCountEmpty := bitCount === 0.U
  }
}
