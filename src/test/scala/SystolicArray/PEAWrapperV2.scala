package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

import scala.util.Random

class PEAWrapperV2Tests(c: PEArrayWrapperV2) extends AdvTester(c) {
  val dataIn = DecoupledSource(c.io.dataIn)
  val weightIn = c.io.weightIn.map{peBundle => DecoupledSource(peBundle)}
  val resultOut = c.io.resultOut.map{peBundle => IrrevocableSink(peBundle)}

  val TEST_CYCLES = 2
  resultOut.foreach(_.outputs.clear())
  for(cycles <- 0 until TEST_CYCLES) {
    resultOut.foreach(_.outputs.clear())
    reg_poke(c.io.weightUpdate, 1)
    reg_poke(c.io.strideX, 1)
    reg_poke(c.io.kernelSizeX, 5)
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
    for(j <- 0 until 3) {
      for(i <- 1 to 5) {
        dataIn.inputs.enqueue(i)
        //takesteps(Random.nextInt(4) + 1)()
        takesteps(2)()
      }
    }
    //for(i <- 1 to 3) {
    //  dataIn.inputs.enqueue(i)
    //  takestep()
    //}
    //takesteps(2)()
    reg_poke(c.io.weightUpdate, 1)
    takestep()
    reg_poke(c.io.weightUpdate, 0)
    takestep()
    takesteps(20)()
    val resultGet = List.tabulate(5){n => resultOut(n).outputs.toList}
    resultOut.foreach(_.outputs.clear())
    for(i <- 0 until 5 ) {
      println(s"RawChan$i:  " + resultGet(i))
    }
    for(i <- 0 until 5 ) {
      print(s"ResultChan$i: ")
      for(j <- 0 until resultGet.map(_.size).max if j % 5 ==i) {
        for(chan <- 0 until 5) {
          if(j >= resultGet(chan).size)
            print("A  ")
          else
            print(resultGet(chan)(j) + "  ")
        }
      }
      print("\n")
    }
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
