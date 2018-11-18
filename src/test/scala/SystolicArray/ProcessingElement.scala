package SystolicArray

import Chisel.iotesters.PeekPokeTester
import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

class ProcessingElementTests(pe: ProcessingElement) extends AdvTester(pe) {
  val dataIn = DecoupledSource(pe.io.dataIn)
  val dataOut = IrrevocableSink(pe.io.dataOut)
  val weightIn = DecoupledSource(pe.io.weightIn)
  val weightOut = IrrevocableSink(pe.io.weightOut)
  val controlIn = DecoupledSource(pe.io.controlIn)
  val controlOut = IrrevocableSink(pe.io.controlOut)
  val resultIn = DecoupledSource(pe.io.resultIn)
  val resultOut = IrrevocableSink(pe.io.resultOut)
  val input = List(1, 2, 3, 4, 5)
  val weight = List(1, 2, 3, 4, 5)
  val TEST_CYCLES = 100
  for(i <- 0 to TEST_CYCLES) {
    dataIn.inputs.enqueue(input(i % input.length))
    weightIn.inputs.enqueue(input(i % input.length))
    controlIn.inputs.enqueue(true)
    resultIn.inputs.enqueue(0)
    dataIn.process()
    weightIn.process()
    controlIn.process()
    resultIn.process()
    val weightGet = if(weightOut.outputs.nonEmpty)weightOut.outputs.dequeue() else -1
    val dataGet = if(dataOut.outputs.nonEmpty)dataOut.outputs.dequeue() else -1
    val ctrlGet = if(controlOut.outputs.nonEmpty)controlOut.outputs.dequeue() else -1
    val resultGet = if(resultOut.outputs.nonEmpty)resultOut.outputs.dequeue() else -1
    println(resultGet.toString + ' ' + weightGet.toString() + ' ' + dataGet.toString() + ' ' + ctrlGet.toString())
    takestep(1)
  }
}

class ProcessingElementTester extends ChiselFlatSpec {
  behavior of "Single Processing Element"
  backends foreach { backend =>
    it should s"PE $backend" in {
      Driver(
        () => new ProcessingElement(8, 5), backend
      )(c => new ProcessingElementTests(c)) should be (true)
    }
  }
}
