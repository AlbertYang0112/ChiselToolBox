package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

class PEAWrapperTests(c: PEArrayWrapper) extends AdvTester(c) with PEAControlBit {
  val dataIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.data)}
  val weightIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.weight)}
  val resultIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.result)}
  val controlIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.control)}
  val dataOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.data)}
  val weightOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.weight)}
  val resultOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.result)}
  val controlOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.control)}
  val TEST_CYCLES = 10
  reg_poke(c.io.fifoReset, 1)
  takestep()
  reg_poke(c.io.fifoReset, 0)
  takestep()
  for(chan <- dataIn.indices) {
    for (i <- 1 to 10)
      for (j <- 1 to 10) {
        dataIn(chan).inputs.enqueue(j)
        // resultIn(chan).inputs.enqueue(0)
      }
    if(chan != dataIn.size - 1)takestep()
  }
  //takestep()
  //takestep()
  for(chanScan <- 0 until 3) {
    for(i <- 1 to 5)
      for(j <- 1 to 3) {
        weightIn(2 - chanScan).inputs.enqueue(j)
        if(j == 1)
          controlIn(chanScan).inputs.enqueue(1)
        else
          controlIn(chanScan).inputs.enqueue(0)
      }
    //takestep()
  }
  controlIn(2).inputs.enqueue(1)
  controlIn(1).inputs.enqueue(1)
  controlIn(0).inputs.enqueue(1)
  controlIn(2).inputs.enqueue(0)
  controlIn(1).inputs.enqueue(0)
  controlIn(0).inputs.enqueue(0)
  takesteps(300)()

}

class PEAWrapperTester extends ChiselFlatSpec {
  behavior of "PEArray Wrapper"
  backends foreach { backend =>
    it should s"PEArrayWrapper $backend" in {
      Driver(
        () => new PEArrayWrapper(3, 3, 8, 2, 5), backend
      )(c => new PEAWrapperTests(c)) should be (true)
    }
  }
}
