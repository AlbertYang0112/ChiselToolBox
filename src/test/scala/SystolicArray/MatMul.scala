package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}
import scala.collection.mutable.ListBuffer

class MatMulTests(c: MatMul) extends AdvTester(c) {
  val dataIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.data)}
  val weightIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.weight)}
  val resultIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.result)}
  val controlIn = c.io.ioArray.map{peBundle => DecoupledSource(peBundle.in.control)}
  val dataOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.data)}
  val weightOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.weight)}
  val resultOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.result)}
  val controlOut = c.io.ioArray.map{peBundle => IrrevocableSink(peBundle.out.control)}

  val TEST_CYCLES = 10

  for(cycle <- 0 until TEST_CYCLES) {
    // var inputArray = ListBuffer.fill(dataIn.size, dataIn.size)(0)
    // var weightArray = ListBuffer.fill(dataIn.size, dataIn.size)(0)
    val inputArray = List.tabulate(dataIn.size, dataIn.size)((row, col) => (row + 1) * (col + 1))
    val weightArray = List.tabulate(dataIn.size, dataIn.size)((row, col) => (row + 1) * (col + 1))
    for (row <- 0 until dataIn.size) {
      for (col <- 0 until dataIn.size) {
        print(inputArray(row)(col))
        step(1)
      }
    }
    val resultArray = List.fill(dataIn.size, dataIn.size)(0)
    val controlArray = List.fill(dataIn.size, dataIn.size)(1)
    for (chan <- dataIn.indices) {
      inputArray(chan).foreach(dataIn(chan).inputs.enqueue(_))
      weightArray(chan).foreach(weightIn(chan).inputs.enqueue(_))
      resultArray(chan).foreach(resultIn(chan).inputs.enqueue(_))
      controlArray(chan).foreach(controlIn(chan).inputs.enqueue(_))
    }
    reg_poke(c.io.run, 1)
    takesteps(100)()
  }
}

class MatMulTester extends ChiselFlatSpec {
  behavior of "2x2 Matrix Multiplication"
  backends foreach { backend =>
    it should s"MatMul $backend" in {
      Driver(
        () => new MatMul(32, 3), backend
      )(c => new MatMulTests(c)) should be (true)
    }
  }
}
