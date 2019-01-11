package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

import scala.util.Random

class PEAWrapperTests(c: PEArrayWrapper) extends AdvTester(c) {
  val dataIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.data)}
  val weightIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.weight)}
  val resultIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.result)}
  val controlIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.control)}
  //val dataOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.data)}
  //val weightOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.weight)}
  val resultOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.result)}
  //val controlOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.control)}
  val TEST_CYCLES = 10
  reg_poke(c.io.fifoReset, 1)
  takestep()
  reg_poke(c.io.fifoReset, 0)
  takestep()
  for(i <- 0 until 200) {
    val writeSequence = Random.shuffle(List.range(0, 6))
    for(chan <- writeSequence.indices) {
      val targetChan = writeSequence(chan) % 3
      if(writeSequence(chan) < 3) {
        dataIn(targetChan).inputs.enqueue((i + targetChan) % 3 + 1)
        //println("Data Channel " + targetChan + " <- " + ((targetChan + i) % 3 + 1))
      } else {
        weightIn(targetChan).inputs.enqueue((i + targetChan) % 3 + 1)
        //println("Weight Channel " + targetChan + " <- " + ((targetChan + i) % 3 + 1))
      }
      takesteps(1)()
      for(chan <- resultOut.indices) {
        if(resultOut(chan).outputs.nonEmpty && chan == 0) {
          println("Result Channel " + chan + "  " + resultOut(chan).outputs.dequeue())
        }
      }
    }
    /*
    for(chan <- dataIn.indices) {
      dataIn(chan).inputs.enqueue((i + chan) % 3 + 1)
      takesteps(1)()
    }
    for(chan <- weightIn.indices) {
      weightIn(chan).inputs.enqueue((i + chan) % 3 + 1)
      takesteps(1)()
    }
    */
    takesteps(5)()
  }
  takesteps(100)()
}

class PEAWrapperTester extends ChiselFlatSpec {
  behavior of "PEArray Wrapper"
  backends foreach { backend =>
    it should s"PEArrayWrapper $backend" in {
      Driver(
        () => new PEArrayWrapper(3, 3, 32, 2, 5), backend
      )(c => new PEAWrapperTests(c)) should be (true)
    }
  }
}
