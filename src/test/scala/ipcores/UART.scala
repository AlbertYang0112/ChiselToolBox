package ipcores

import AXI4.{AXI4Bundle, AXI4Parameter}
import chisel3.Module
import chisel3.iotesters._

import scala.collection.mutable
import scala.util.Random


class UARTAXI4Tests(c: UARTAXI4) extends PeekPokeTester(c) {
  var ARState = 0
  var AWState = 0
  val WriteAddress = List(0, 1, 2, 3)
  val WriteList = List(0, 1, 2, 3)
  var WritePointer = 0
  val ReadAddress = List(0, 1, 2, 3)
  var ReadPointer = 0
  var cycle = 0
  val TOTAL_CYCLE = 50000
  var sendBuffer = 0
  for (cycle <- 0 to TOTAL_CYCLE) {
    // AR Bundle
    if(ARState == 0) {
      poke(c.io.axi4Lite.ar.bits.addr, ReadAddress(ReadPointer))
      poke(c.io.axi4Lite.ar.bits.len, 0)
      poke(c.io.axi4Lite.ar.bits.size, 0)
      poke(c.io.axi4Lite.ar.bits.burst, 0)
      poke(c.io.axi4Lite.ar.bits.lock, 0)
      poke(c.io.axi4Lite.ar.bits.cache, 0)
      poke(c.io.axi4Lite.ar.bits.prot, 0)
      poke(c.io.axi4Lite.ar.bits.qos, 0)
      poke(c.io.axi4Lite.ar.bits.region, 0)
      poke(c.io.axi4Lite.ar.valid, 1)
      poke(c.io.axi4Lite.ar.bits.id, Random.nextInt(0xFF))

      poke(c.io.axi4Lite.r.ready, 0)
      if(peek(c.io.axi4Lite.ar.ready) == 1) {
        ARState = 1
      }
    } else if(ARState == 1) {
      poke(c.io.axi4Lite.ar.valid, 0)
      if(peek(c.io.axi4Lite.r.valid) == 1) {
        ARState = 0
        poke(c.io.axi4Lite.r.ready, 1)
        val receiveData = peek(c.io.axi4Lite.r.bits.data)
        val receiveResp = peek(c.io.axi4Lite.r.bits.resp)
        println(s"Read Address " + ReadAddress(ReadPointer) +
          s"  Get Data: $receiveData" + s"  Resp: $receiveResp")
        ReadPointer += 1
        ReadPointer %= ReadAddress.length
      }
    } else {
      ARState = 0
    }

    if(AWState == 0) {
      poke(c.io.axi4Lite.aw.bits.addr, WriteAddress(WritePointer))
      poke(c.io.axi4Lite.aw.bits.len, 0)
      poke(c.io.axi4Lite.aw.bits.size, 0)
      poke(c.io.axi4Lite.aw.bits.burst, 0)
      poke(c.io.axi4Lite.aw.bits.lock, 0)
      poke(c.io.axi4Lite.aw.bits.cache, 0)
      poke(c.io.axi4Lite.aw.bits.prot, 0)
      poke(c.io.axi4Lite.aw.bits.qos, 0)
      poke(c.io.axi4Lite.aw.bits.region, 0)
      poke(c.io.axi4Lite.aw.valid, 1)
      poke(c.io.axi4Lite.aw.bits.id, Random.nextInt(0xFF))

      //poke(c.io.axi4Lite.w.valid, 1)
      poke(c.io.axi4Lite.w.bits.last, 0)
      poke(c.io.axi4Lite.b.ready, 0)
      if(peek(c.io.axi4Lite.aw.ready) == 1) {
        AWState = 1
      }
    } else if(AWState == 1) {
      poke(c.io.axi4Lite.aw.valid,0)
      poke(c.io.axi4Lite.w.valid, 1)
      if(peek(c.io.axi4Lite.w.ready) == 1) {
        sendBuffer = Random.nextInt(0xFF)
        AWState = 2
        poke(c.io.axi4Lite.w.bits.data, sendBuffer)
        poke(c.io.axi4Lite.w.bits.strobe, 0)
        poke(c.io.axi4Lite.w.bits.last, 1)
        WritePointer += 1
        WritePointer %= WriteAddress.length
      }
    } else if(AWState == 2) {
      poke(c.io.axi4Lite.w.bits.last, 0)
      poke(c.io.axi4Lite.w.valid, 0)
      if(peek(c.io.axi4Lite.b.valid) == 1) {
        poke(c.io.axi4Lite.b.ready, 1)
        val BResp = peek(c.io.axi4Lite.b.bits.resp)
        println("Write Address " + WriteAddress(WritePointer) + " Write " + sendBuffer + s" Resp: $BResp")
        AWState = 0
      }
    } else {
      AWState = 0
    }
    val tx = peek(c.io.tx)
    poke(c.io.rx, tx)
    // println(s"AWS: $AWState  ARS: $ARState")
    step(1)
  }
  poke(c.io.axi4Lite.r.ready, 0)
  poke(c.io.axi4Lite.w.valid, 0)
  poke(c.io.axi4Lite.b.ready, 0)
  poke(c.io.axi4Lite.ar.valid, 0)
  poke(c.io.axi4Lite.aw.valid, 0)
  step(20)
  for (cycle <- 0 to TOTAL_CYCLE) {
    // AR Bundle
    if(ARState == 0) {
      poke(c.io.axi4Lite.ar.bits.addr, ReadAddress(ReadPointer))
      poke(c.io.axi4Lite.ar.bits.len, 0)
      poke(c.io.axi4Lite.ar.bits.size, 0)
      poke(c.io.axi4Lite.ar.bits.burst, 0)
      poke(c.io.axi4Lite.ar.bits.lock, 0)
      poke(c.io.axi4Lite.ar.bits.cache, 0)
      poke(c.io.axi4Lite.ar.bits.prot, 0)
      poke(c.io.axi4Lite.ar.bits.qos, 0)
      poke(c.io.axi4Lite.ar.bits.region, 0)
      poke(c.io.axi4Lite.ar.valid, 1)
      poke(c.io.axi4Lite.ar.bits.id, Random.nextInt(0xFF))

      poke(c.io.axi4Lite.r.ready, 0)
      if(peek(c.io.axi4Lite.ar.ready) == 1) {
        ARState = 1
      }
    } else if(ARState == 1) {
      poke(c.io.axi4Lite.ar.valid, 0)
      if(peek(c.io.axi4Lite.r.valid) == 1) {
        ARState = 0
        poke(c.io.axi4Lite.r.ready, 1)
        val receiveData = peek(c.io.axi4Lite.r.bits.data)
        val receiveResp = peek(c.io.axi4Lite.r.bits.resp)
        println(s"Read Address " + ReadAddress(ReadPointer) +
          s"  Get Data: $receiveData" + s"  Resp: $receiveResp")
        ReadPointer += 1
        ReadPointer %= ReadAddress.length
      }
    } else {
      ARState = 0
    }

    var chanAWDone = false
    var chanWDone = false

    if(AWState == 0) {
      poke(c.io.axi4Lite.aw.bits.addr, WriteAddress(WritePointer))
      poke(c.io.axi4Lite.aw.bits.len, 0)
      poke(c.io.axi4Lite.aw.bits.size, 0)
      poke(c.io.axi4Lite.aw.bits.burst, 0)
      poke(c.io.axi4Lite.aw.bits.lock, 0)
      poke(c.io.axi4Lite.aw.bits.cache, 0)
      poke(c.io.axi4Lite.aw.bits.prot, 0)
      poke(c.io.axi4Lite.aw.bits.qos, 0)
      poke(c.io.axi4Lite.aw.bits.region, 0)
      poke(c.io.axi4Lite.aw.bits.id, Random.nextInt(0xFF))
      poke(c.io.axi4Lite.aw.valid, 1)
      poke(c.io.axi4Lite.w.valid, 1)
      poke(c.io.axi4Lite.w.bits.data, sendBuffer)
      poke(c.io.axi4Lite.w.bits.strobe, 0)
      poke(c.io.axi4Lite.w.bits.last, 1)

      //poke(c.io.axi4Lite.w.valid, 1)
      poke(c.io.axi4Lite.w.bits.last, 0)
      poke(c.io.axi4Lite.b.ready, 0)
      if (peek(c.io.axi4Lite.aw.ready) == 1 || peek(c.io.axi4Lite.w.ready) == 1) {
        AWState = 1
      }
    }
    else if(AWState == 1) {
      if(peek(c.io.axi4Lite.aw.ready) == 1) {
        chanAWDone = true
        poke(c.io.axi4Lite.aw.valid, 0)
      }
      if(peek(c.io.axi4Lite.w.ready) == 1) {
        chanWDone = true
        poke(c.io.axi4Lite.w.valid, 0)
        poke(c.io.axi4Lite.w.bits.last, 0)
        poke(c.io.axi4Lite.w.valid, 0)
      }
      if(chanAWDone && chanWDone) {
        chanAWDone = false
        chanWDone = false
        sendBuffer = Random.nextInt(0xFF)
        AWState = 2
        WritePointer += 1
        WritePointer %= WriteAddress.length
      }
    } else if(AWState == 2) {
      if(peek(c.io.axi4Lite.b.valid) == 1) {
        poke(c.io.axi4Lite.b.ready, 1)
        val BResp = peek(c.io.axi4Lite.b.bits.resp)
        println("Write Address " + WriteAddress(WritePointer) + " Write " + sendBuffer + s" Resp: $BResp")
        AWState = 0
      }
    } else {
      AWState = 0
    }
    val tx = peek(c.io.tx)
    poke(c.io.rx, tx)
    // println(s"AWS: $AWState  ARS: $ARState")
    step(1)
  }
}

class UARTAXI4Tester extends ChiselFlatSpec {
  behavior of "UARTAXI4Tester"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new UARTAXI4(
          baud = 115200,
          clockFreq = 10000000,
          byteLength = 8,
          axi4param = AXI4Parameter(
            idBits = 2,
            addrBits = 2,
            dataBits = 8,
            userBits = 0,
            wcorrupt = false,
            isLite = false,
            isMaster = false
          )), backend
      )(c => new UARTAXI4Tests(c)) should be (true)
    }
  }
}

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
