package top

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class TopExpTests(c: TopExp)
  extends PeekPokeTester(c) {
  step(200)
}

class TopExpTester extends ChiselFlatSpec {
  behavior of "SyncPulseGenerator"
  backends foreach {backend =>
    it should s"Generate a pulse every time a positive edge arrives. $backend" in {
      Driver(() => new TopExp, backend)(c => new TopExpTests(c)) should be (true)
    }
  }
}
