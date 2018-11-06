package ipcores

import chisel3._
import chisel3.core.withClock
import chisel3.util.Cat
import ipcores.ParityBit.ParityBit


class UART(baud: Int, clockFreq: Int) extends Module {
  val io = IO(new Bundle{
    val tx = Output(Bool())
    val rx = Input(Bool())
    val txData = Input(UInt(8.W))
    val recvData = Output(UInt(8.W))
    val rxRecvDone = Output(Bool())
    val rxReadDone = Input(Bool())
    val txReady = Output(Bool())
    val txDataValid = Input(Bool())
  })
  val clockDiv = clockFreq / baud / 16
  println("Actual Baud Clock: " + (clockFreq.toDouble / (clockDiv * 16).toDouble))
  val receiverScanClock = RegInit(false.B)
  val receiver = Module(new UARTReceiver(byteLength = 8, parityCheck = ParityBit.NoParityCheck))
  val transmitter = Module(new UARTTransmitter(byteLength = 8, parityCheck = ParityBit.NoParityCheck))
  val clockDivider = Module(new ClockDivider(clockDiv))
  val baudClock = Wire(Bool())
  clockDivider.io.clockIn := clock.asUInt()
  withClock(clockDivider.io.clockOut.asClock()) {
    val divider = RegInit(0.U(4.W))
    divider := divider + 1.U
    baudClock := divider(3)
  }
  transmitter.io.byteClock := baudClock
  receiver.io.scanClock := clockDivider.io.clockOut
  io.tx := transmitter.io.tx
  io.txReady := transmitter.io.ready
  transmitter.io.dataValid := io.txDataValid
  transmitter.io.txData := io.txData
  receiver.io.rx := io.rx
  receiver.io.readDone := io.rxReadDone
  io.rxRecvDone := receiver.io.recvDone
  io.recvData := receiver.io.recvData
}

trait UARTTransmitterStates {
  val stateWidth = 4
  val Idle = 0.U(stateWidth.W)
  val TransmittingHead = 1.U(stateWidth.W)
  val Transmitting = 2.U(stateWidth.W)
  val TransmittingParity = 3.U(stateWidth.W)
  val TransmittingStopBit = 4.U(stateWidth.W)
}

class UARTTransmitter(byteLength: Int, parityCheck: ParityBit) extends Module with UARTTransmitterStates {
  val io = IO(new Bundle{
    val byteClock = Input(Bool())
    val tx = Output(Bool())
    val txData = Input(UInt(byteLength.W))
    val ready = Output(Bool())
    val dataValid = Input(Bool())
  })
  val txBufferFast = RegInit(0.U(byteLength.W))
  val bufferValid = RegInit(false.B)
  val testState = Wire(UInt(stateWidth.W))
  val testBitCount = Wire(UInt(3.W))
  val testParity = Wire(Bool())
  val testBuffer = Wire(UInt(byteLength.W))
  when(!bufferValid & io.dataValid) {
    txBufferFast := io.txData
    bufferValid := true.B
  }

  withClock(io.byteClock.asClock()) {
    val txState = RegInit(Idle)
    val bitCount = RegInit(0.U(3.W))
    val parity = RegInit(false.B)
    val tx = RegInit(true.B)
    val txBuffer = RegInit(0.U(byteLength.W))
    testState := txState
    testBitCount := bitCount
    testParity := parity
    testBuffer := txBuffer
    io.tx := tx
    io.ready := txState === Idle
    when(txState === Idle) {
      tx := true.B
      when(bufferValid) {
        txState := TransmittingHead
        txBuffer := txBufferFast
        bitCount := 0.U
      }
    } .elsewhen(txState === TransmittingHead) {
      tx := false.B
      txState := Transmitting
    } .elsewhen(txState === Transmitting) {
      tx := txBuffer(byteLength - 1)
      parity := parity ^ txBuffer(byteLength - 1)
      txBuffer := Cat(txBuffer(byteLength - 2, 0), 0.U)
      when(bitCount === 7.U) {
        bitCount := 0.U
        txState := Mux((parityCheck == ParityBit.NoParityCheck).B, TransmittingStopBit, TransmittingParity)
      } .otherwise {
        bitCount := bitCount + 1.U
        txState := txState
      }
    } .elsewhen(txState === TransmittingParity) {
      tx := Mux((parityCheck == ParityBit.EvenParityCheck).B, parity, ~parity)
      txState := TransmittingStopBit
    } .elsewhen(txState === TransmittingStopBit) {
      tx := 0.U
      txState := Idle
      bufferValid := false.B
    }
  }
}

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
