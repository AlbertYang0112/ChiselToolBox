package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

import scala.util.Random

class PEAWrapperV2Tests(c: PEArrayWrapperV2) extends AdvTester(c) {
  val dataIn = DecoupledSource(c.io.dataIn)
  val weightIn = c.io.weightIn.map{peBundle => DecoupledSource(peBundle)}
  val resultOut = c.io.resultOut.map{peBundle => IrrevocableSink(peBundle)}

  val TEST_CYCLES = 20
  for(cycles <- 0 until TEST_CYCLES) {
    reg_poke(c.io.weightUpdate, 1)
    takesteps(2)()
    while(peek(c.io.weightUpdateReady) != 1) {
      takestep()
    }
    for(i <- 1 to 5) {
      for(chan <- weightIn.indices) {
        weightIn(chan).inputs.enqueue(i * (chan + 1))
        takestep()
      }
    }
    reg_poke(c.io.weightUpdate, 0)
    takestep()
    takesteps(2)()
    // Feed data
    for(j <- 0 until 10) {
      for(i <- 1 to 5) {
        dataIn.inputs.enqueue(i)
        //takesteps(Random.nextInt(4) + 1)()
        takestep()
      }
    }
    for(i <- 1 to 3) {
      dataIn.inputs.enqueue(i)
      takestep()
    }
    //takesteps(2)()
    //reg_poke(c.io.weightUpdate, 1)
    //takestep()
    //reg_poke(c.io.weightUpdate, 0)
    //takestep()
    takesteps(10)()
  }
  takesteps(100)()
}

class PEAWrapperV2Tester extends ChiselFlatSpec {
  behavior of "PEArray Wrapper"
  backends foreach { backend =>
    it should s"PEArrayWrapper $backend" in {
      Driver(
        () => new PEArrayWrapperV2(
          dataWidth = 8,
          weightWidth = 8,
          weightChanNum = 5,
          rows = 5,
          cols = 5,
          PEResultFIFODepth = 16,
          wrapFIFODepth = 16,
          chanFIFODepth = 16
        ), backend
      )(c => new PEAWrapperV2Tests(c)) should be (true)
    }
  }
}
