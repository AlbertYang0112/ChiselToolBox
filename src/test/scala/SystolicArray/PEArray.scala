package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}
import chisel3.util.Decoupled

class PEArrayTests(c: PEArray) extends AdvTester(c){
  val dataIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.data)}
  val weightIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.weight)}
  val resultIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.result)}
  val controlIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.control)}
  //val dataOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.data)}
  //val weightOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.weight)}
  //val resultOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.result)}
  //val controlOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.control)}

  val TEST_CYCLE = 500
  reg_poke(c.io.ioArray.head.out.data.ready, 0)
  takestep()
  for(i <- 0 until 30) {
    dataIn.head.inputs.enqueue(i % 9 + 1)
  }
  takesteps(10)()
  var weightChannelSel = 0
  for(i <- 0 until TEST_CYCLE) {
    if(i % 4 == 0){
      weightIn(weightChannelSel).inputs.enqueue(i % 9 + 1)
      weightChannelSel = if(weightChannelSel == weightIn.size - 1) 0
                          else weightChannelSel + 1
    }
    if(c.io.ioArray.exists(ioElem => peek(ioElem.in.weight.valid) == 0)) {
      reg_poke(c.io.ioArray.head.out.data.ready, 0)
    } else {
      reg_poke(c.io.ioArray.head.out.data.ready, 1)
    }
    takestep()
  }
  takesteps(300)()
}

class PEArrayTester extends ChiselFlatSpec {
  behavior of "2x2 Matrix Multiplication"
  backends foreach { backend =>
    it should s"PEArray $backend" in {
      Driver(
        () => new PEArray(3, 3, 8, 2), backend
      )(c => new PEArrayTests(c)) should be (true)
    }
  }
}
