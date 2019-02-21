package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

import scala.util.Random

class PEAWrapperV2Tests(c: PEArrayWrapperV2) extends AdvTester(c) {
  val dataIn = DecoupledSource(c.io.dataIn)
  val weightIn = c.io.weightIn.map{peBundle => DecoupledSource(peBundle)}
  val resultOut = c.io.resultOut.map{peBundle => IrrevocableSink(peBundle)}

  val TEST_CYCLES = 2
  val KERNEL_SIZE_X = 4
  val KERNEL_SIZE_Y = 5
  val STRIDE_X = 1
  val STRIDE_Y = 1
  val DATA_SIZE = 40
  resultOut.foreach(_.outputs.clear())

  for(cycles <- 0 until TEST_CYCLES) {
    //val testData = List.tabulate(DATA_SIZE)(n => n % KERNEL_SIZE_X + 1)
    val testData = List.tabulate(DATA_SIZE)(n => Random.nextInt(50))
    //val testWeight = List.tabulate(KERNEL_SIZE_Y)(y => List.tabulate(KERNEL_SIZE_X)(x => x % KERNEL_SIZE_X + 1))
    val testWeight = List.tabulate(KERNEL_SIZE_Y)(y => List.tabulate(KERNEL_SIZE_X)(x => Random.nextInt(10)))
    // val testWeight = List.tabulate(KERNEL_SIZE_Y)(y => List.tabulate(KERNEL_SIZE_X)(x => 1))
    val resultSize = (KERNEL_SIZE_X + DATA_SIZE - 1) / STRIDE_X + (
      if((KERNEL_SIZE_X + DATA_SIZE - 1) % STRIDE_X != 0)
        1
      else
        0
      )
    val expectedResult = List.tabulate(KERNEL_SIZE_Y){y =>
      List.tabulate(resultSize){
        x => {
          val pos = x * STRIDE_X
          var sum = 0
          if (pos < KERNEL_SIZE_X - 1) {
            for (i <- 0 to pos) {
              sum += testData(i) * testWeight(y)(KERNEL_SIZE_X - 1 - pos + i)
            }
          } else {
            for (i <- 0 until KERNEL_SIZE_X if pos + i - KERNEL_SIZE_X + 1 < testData.size)
              sum += testData(pos + i - KERNEL_SIZE_X + 1) * testWeight(y)(i)
          }
          sum
        }
      }
    }
    resultOut.foreach(_.outputs.clear())
    reg_poke(c.io.weightUpdate, 1)
    reg_poke(c.io.strideX, STRIDE_X)
    reg_poke(c.io.strideY, STRIDE_Y)
    reg_poke(c.io.kernelSizeX, KERNEL_SIZE_X)
    reg_poke(c.io.kernelSizeY, KERNEL_SIZE_Y)
    takesteps(2)()
    while(peek(c.io.weightUpdateReady) != 1) {
      takestep()
    }
    for(i <- 0 until KERNEL_SIZE_X) {
      for(chan <- weightIn.indices) {
        //weightIn(chan).inputs.enqueue(i * (chan + 1))
        weightIn(chan).inputs.enqueue(testWeight(chan)(i))
        takestep()
      }
    }
    reg_poke(c.io.weightUpdate, 0)
    takestep()
    takesteps(2)()
    // Feed data
    for(i <- testData.indices) {
      dataIn.inputs.enqueue(testData(i))
      takesteps(2)()
    }
    //for(j <- 0 until 3) {
    //  for(i <- 1 to 5) {
    //    dataIn.inputs.enqueue(i)
    //    takesteps(2)()
    //  }
    //}
    //for(i <- 1 to 3) {
    //  dataIn.inputs.enqueue(i)
    //  takestep()
    //}
    takesteps(2)()
    reg_poke(c.io.weightUpdate, 1)
    takestep()
    reg_poke(c.io.weightUpdate, 0)
    takestep()
    takesteps(20)()
    val resultGet = List.tabulate(5){n => resultOut(n).outputs.toList}
    resultOut.foreach(_.outputs.clear())
    for(i <- 0 until c.cols ) {
      println(s"RawChan$i:  " + resultGet(i))
    }
    for(i <- 0 until KERNEL_SIZE_X ) {
      print(s"ExpectChan$i: ")
      for(index <- expectedResult(i).indices) {
        print(expectedResult(i)(index) + "  ")
      }
      print("\n")
      print(s"ResultChan$i: ")
      for(j <- 0 until resultGet.map(_.size).max if j % KERNEL_SIZE_X == i) {
        for(chan <- 0 until KERNEL_SIZE_X) {
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
