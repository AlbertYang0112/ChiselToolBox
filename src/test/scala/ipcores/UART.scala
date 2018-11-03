package ipcores

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

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
