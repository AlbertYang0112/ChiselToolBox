package ipcores

import chisel3._
import chisel3.core.withClock
import chisel3.iotesters.{Driver, ChiselFlatSpec, PeekPokeTester}
import chisel3.util.Cat
import ipcores.ParityBit.ParityBit

class ClockDividerTests(c: ClockDivider) extends PeekPokeTester(c) {
  var clockIn = false
  var TEST_CYCLES = 10000
  var i = 0
  for(i <- 1 to TEST_CYCLES) {
    poke(c.io.clockIn, clockIn)
    clockIn = !clockIn
    step(1)
  }
}

class ClockDividerTester extends ChiselFlatSpec {
  behavior of "ClockDivider"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new ClockDivider(7), backend
      )(c => new ClockDividerTests(c)) should be (true)
    }
  }
}

class ClockDividerThreeTests(c: ClockDividerThree) extends PeekPokeTester(c) {
  var clockIn = false
  var TEST_CYCLES = 10000
  var i = 0
  for(i <- 1 to TEST_CYCLES) {
    poke(c.io.clockIn, clockIn)
    clockIn = !clockIn
    step(1)
  }
}

class ClockDividerThreeTester extends ChiselFlatSpec {
  behavior of "ClockDividerThree"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new ClockDividerThree, backend
      )(c => new ClockDividerThreeTests(c)) should be (true)
    }
  }
}

