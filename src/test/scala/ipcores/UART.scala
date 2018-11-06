package ipcores

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

import scala.collection.mutable
import scala.util.Random

class UARTTests(c: UART) extends PeekPokeTester(c) {
  val TEST_CYCLES = 2000000
  val sigList = new mutable.Queue[Int]
  var scanClock = false
  var sigClock = false
  var pointer = 0
  var state = 0
  var recState = 0
  for(i <- 0 until TEST_CYCLES) {
    if(state == 0) {
      if(peek(c.io.txReady) == 1) {
        var sendRandom = Random.nextInt(0xFF)
        sigList += sendRandom
        poke(c.io.txData, sendRandom)
        poke(c.io.txDataValid, true)
        pointer = (pointer + 1) % sigList.length
        state = 1
      }
    } else if(state == 1) {
      state = if(peek(c.io.txReady) == 0) 0 else 1
    }
    if(recState == 0) {
      if (peek(c.io.rxRecvDone) == 1) {
        var recvByte = peek(c.io.recvData)
        var expected = 0
        if(sigList.nonEmpty)
          expected = sigList.dequeue()
        if(expected != recvByte) {
          println("Receive: 0x" + recvByte.toString(16).toUpperCase() + "  Expected: 0x" + expected.toHexString.toUpperCase())
          fail
        }
        poke(c.io.rxReadDone, true)
        recState = 1
      }
    } else if(recState == 1) {
      if(peek(c.io.rxRecvDone) == 0) {
        poke(c.io.rxReadDone, false)
        recState = 0
      }
    }
    poke(c.io.rx, peek(c.io.tx))
    step(1)
  }
}

class UARTTester extends ChiselFlatSpec {
  behavior of "UARTReceiverTester"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new UART(9600, 10000000), backend
      )(c => new UARTTests(c)) should be (true)
    }
  }
}

class UARTTransmitterTests(c: UARTTransmitter) extends PeekPokeTester(c) {
  val TEST_CYCLES = 20000
  val sigList = Seq(0xde, 0xad, 0xbe, 0xef)
  var scanClock = false
  var sigClock = false
  var pointer = 0
  var state = 0
  for(i <- 0 until TEST_CYCLES) {
    if(i % 32 == 0) {
      scanClock = !scanClock
      poke(c.io.byteClock, scanClock)
    }
    if(state == 0) {
      if (peek(c.io.ready) == 1) {
        poke(c.io.txData, sigList(pointer))
        poke(c.io.dataValid, true)
        pointer = (pointer + 1) % sigList.length
        state = 1
      }
    } else if (state == 1) {
      state = if(peek(c.io.ready) == 0) 0 else 1
    }
    step(1)
  }
}

class UARTTransmitterTester extends ChiselFlatSpec {
  behavior of "UARTReceiverTester"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new UARTTransmitter(8, ParityBit.OddParityCheck), backend
      )(c => new UARTTransmitterTests(c)) should be (true)
    }
  }
}

class UARTReceiverTests(c: UARTReceiver) extends PeekPokeTester(c) {
  val TEST_CYCLES = 20000
  val sigList = Seq(
    0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0,
    0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0,
    0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
    0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0
  )
  var scanClock = false
  var sigClock = false
  var pointer = 0
  poke(c.io.rx, true)
  for(i <- 0 until TEST_CYCLES) {
    if(i % 2 == 0) {
      scanClock = !scanClock
      poke(c.io.scanClock, scanClock)
    }
    if(i % 32 == 0) {
      sigClock = !sigClock
      if(!sigClock) {
        if (peek(c.io.recvDone) == 1) {
          var recvByte = peek(c.io.recvData)
          println("Receive: 0x" + recvByte.toString(16).toUpperCase())
          poke(c.io.readDone, true)
        } else {
          poke(c.io.readDone, false)
        }
        poke(c.io.rx, sigList(pointer))
        pointer += 1
        if (pointer == sigList.length)
          pointer = 0
      }
    }
    step(1)
  }
}

class UARTReceiverTester extends ChiselFlatSpec {
  behavior of "UARTReceiverTester"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new UARTReceiver(8, ParityBit.OddParityCheck), backend
      )(c => new UARTReceiverTests(c)) should be (true)
    }
  }
}
