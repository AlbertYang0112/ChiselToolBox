package ipcores

import AXI4.{AXI4Bundle, AXI4BundleAddr, AXI4Parameter, AXI4RespCode}
import chisel3._
import chisel3.core.withClock
import chisel3.util.Cat
import ipcores.ParityBit.ParityBit

trait UARTAXI4Address {
  val ReadBuffer = 0.U
  val SendBuffer = 1.U
  val StateBuffer = 2.U
}

class UARTAXI4(baud: Int,
               clockFreq:Int,
               axi4param : AXI4Parameter,
               byteLength: Int
              ) extends Module with UARTAXI4Address with AXI4RespCode {
  val io = IO(new Bundle{
    val axi4Lite = Flipped(new AXI4Bundle(axi4param))
    val recvDone = Output(Bool())
    val txDone = Output(Bool())
    val tx = Output(Bool())
    val rx = Input(Bool())
  })
  val baudClockSource = Module(new BaudClockGenerator(baud, clockFreq))
  val receiver = Module(new UARTReceiver(byteLength = 8, parityCheck = ParityBit.NoParityCheck))
  val transmitter = Module(new UARTTransmitter(byteLength = 8, parityCheck = ParityBit.NoParityCheck))
  val arReady = RegInit(false.B)
  val readState = RegInit(0.U(4.W))
  val writeState = RegInit(0.U(4.W))
  val txFIFO = RegInit(0.U(byteLength.W))
  val rxFIFO = RegInit(0.U(byteLength.W))
  val axiARReady = RegInit(true.B)
  val axiAWReady = RegInit(true.B)
  val axiRValid = RegInit(false.B)
  val axiWReady = RegInit(false.B)
  val axiBValid = RegInit(false.B)
  val axiBResp = RegInit(OKAY)
  val axiRResp = RegInit(OKAY)
  val transmitterBufferUpdated = RegInit(false.B)
  val axiRAddr = RegInit(0.U(2.W))

  io.axi4Lite.aw.nodeq()
  io.axi4Lite.ar.nodeq()
  io.axi4Lite.w.nodeq()
  io.axi4Lite.r.noenq()
  io.axi4Lite.b.noenq()

  io.axi4Lite.r.bits.id := 0.U
  io.axi4Lite.r.bits.resp := axiRResp
  io.axi4Lite.r.bits.last := false.B
  io.axi4Lite.b.bits.id := 0.U
  io.axi4Lite.r.bits.data := 0.U
  io.axi4Lite.aw.ready := axiAWReady
  io.axi4Lite.ar.ready := axiARReady
  io.axi4Lite.r.valid := axiRValid
  io.axi4Lite.w.ready := axiWReady
  io.axi4Lite.b.valid := axiBValid
  io.axi4Lite.b.bits.resp := axiBResp
  def isValidLenAndSize(addrBundle: AXI4BundleAddr):Bool =
    (addrBundle.len === 0.U) & (addrBundle.size === (if(byteLength == 9) 1.U else 0.U))

  io.recvDone := receiver.io.recvDone
  io.txDone := transmitter.io.ready

  // Clock Interconnect
  transmitter.io.byteClock := baudClockSource.io.baudClock
  receiver.io.scanClock := baudClockSource.io.baudClock16x

  // IO Connect
  io.tx := transmitter.io.tx
  receiver.io.rx := io.rx

  transmitter.io.txData := txFIFO
  transmitter.io.dataValid := transmitterBufferUpdated

  // Update the RX FIFO
  when(!(io.axi4Lite.r.fire() && readState =/= ReadBuffer) && receiver.io.recvDone) {
    rxFIFO := receiver.io.recvData
    receiver.io.readDone := true.B
  } .otherwise {
    receiver.io.readDone := false.B
  }
  when(io.axi4Lite.ar.fire()) {
    when(io.axi4Lite.ar.bits.addr === ReadBuffer & isValidLenAndSize(io.axi4Lite.ar.bits)) {
      axiRAddr := ReadBuffer
      axiRValid := true.B
      axiRResp := OKAY
    } .elsewhen(io.axi4Lite.ar.bits.addr === SendBuffer & isValidLenAndSize(io.axi4Lite.ar.bits)) {
      axiRAddr := SendBuffer
      axiRValid := true.B
      axiRResp := OKAY
    } .otherwise {
      axiRAddr := ReadBuffer
      axiRResp := SLVERR
      axiRValid := true.B
    }
    io.axi4Lite.r.bits.last := true.B
  }
  when(io.axi4Lite.r.fire()) {
    axiRValid := false.B
    io.axi4Lite.r.bits.last := false.B
  }
  when(axiRAddr === ReadBuffer) {
    io.axi4Lite.r.bits.data := rxFIFO
  } .elsewhen(axiRAddr === SendBuffer) {
    io.axi4Lite.r.bits.data := txFIFO
  } otherwise {
    io.axi4Lite.r.bits.data := 0.U
  }

  when(io.axi4Lite.aw.fire()) {
    axiWReady := true.B
    when(io.axi4Lite.aw.bits.addr === SendBuffer & isValidLenAndSize(io.axi4Lite.aw.bits)) {
      axiBResp := OKAY
    } .otherwise {
      axiBResp := SLVERR
    }
  }
  when(io.axi4Lite.w.fire()){
    axiWReady := false.B
    axiBValid := true.B
    when(io.axi4Lite.aw.bits.addr === SendBuffer & isValidLenAndSize(io.axi4Lite.aw.bits)) {
      txFIFO := io.axi4Lite.w.bits.data
      transmitterBufferUpdated := true.B
    }
  }
  when(io.axi4Lite.b.fire()) {
    axiBValid := false.B
  }
  when(!transmitter.io.ready) {
    transmitterBufferUpdated := false.B
  }
}


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
  val receiver = Module(new UARTReceiver(byteLength = 8, parityCheck = ParityBit.NoParityCheck))
  val transmitter = Module(new UARTTransmitter(byteLength = 8, parityCheck = ParityBit.NoParityCheck))
  val baudClockSource = Module(new BaudClockGenerator(baud, clockFreq))
  transmitter.io.byteClock := baudClockSource.io.baudClock
  receiver.io.scanClock := baudClockSource.io.baudClock16x
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
  // val txBufferFast = RegInit(0.U(byteLength.W))
  val txBufferFast = Wire(UInt(byteLength.W))
  val bufferValid = RegInit(false.B)
  val testState = Wire(UInt(stateWidth.W))
  val testBitCount = Wire(UInt(3.W))
  val testParity = Wire(Bool())
  val testBuffer = Wire(UInt(byteLength.W))
  txBufferFast := io.txData

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
      when(io.dataValid) {
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
    io.recvDone := receiverState === ReceiveDone
    receiverClock := Mux(activeByteReceive, bitClock, true.B)
    bitClock := pulseCounter(3)
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

class BaudClockGenerator(baud: Int, clockFreq: Int) extends Module {
  val io = IO(new Bundle{
    val baudClock = Output(Bool())
    val baudClock16x = Output(Bool())
  })
  val clockDiv = clockFreq / baud / 16
  println("Actual Baud Clock: " + (clockFreq.toDouble / (clockDiv * 16).toDouble))
  val clockDivider = Module(new ClockDivider(clockDiv))
  clockDivider.io.clockIn := clock.asUInt()
  io.baudClock16x := clockDivider.io.clockOut
  withClock(clockDivider.io.clockOut.asClock()) {
    val divider = RegInit(0.U(4.W))
    divider := divider + 1.U
    io.baudClock := divider(3)
  }
}

