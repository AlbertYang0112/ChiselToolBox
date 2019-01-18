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
  val TEST_CYCLES = 10000
  def writeDataAndWeight(data :Int, weight: Int) = {
    if(peek(c.io.fifoResetting) == 0)
      for (chan <- 0 until c.rows) {
        dataIn(chan).inputs.enqueue(data)
        takesteps(1)()
      }
      //for (chan <- 0 until c.cols) {
      //  weightIn(chan).inputs.enqueue(weight)
      //  takesteps(4)()
      //}
  }
  for(cycles <- 0 until TEST_CYCLES) {
    val KERNEL_SIZE = Random.nextInt(10) + 1
    val DATA_SIZE = (1 + Random.nextInt(100)) * KERNEL_SIZE
    //val DATA_SIZE = KERNEL_SIZE
    val RESULT_SIZE = DATA_SIZE - KERNEL_SIZE + 1
    dataIn.foreach(channel => channel.inputs.clear())
    weightIn.foreach(channel => channel.inputs.clear())
    resultIn.foreach(channel => channel.inputs.clear())
    takestep()
    reg_poke(c.io.fifoReset, 1)
    reg_poke(c.io.kernelSize, KERNEL_SIZE)
    reg_poke(c.io.repeatWeight, 0)
    takestep()
    reg_poke(c.io.fifoReset, 0)
    takestep()
    while(peek(c.io.fifoResetting) == 1) {
      takestep()
    }
    reg_poke(c.io.stall, 1)
    takestep()
    // Generate the test data
    //val testData = List.tabulate(DATA_SIZE)(n => if(n % 10 == 0) 1 else 0)
    //val testData = List.tabulate(DATA_SIZE)( n => n % KERNEL_SIZE + 1)
    val testData = List.tabulate(DATA_SIZE)(n => Random.nextInt(100))
    //val testWeight = List.fill(3)(1)
    val testWeight = List.tabulate(KERNEL_SIZE)(n => Random.nextInt(100))
    //val testWeight = List.tabulate(KERNEL_SIZE)(n => 1)
    //val testWeight = List.tabulate(KERNEL_SIZE)(n => n % KERNEL_SIZE + 1)
    val expectedResult = List.tabulate(RESULT_SIZE)(
      n => {
        var sum = 0
        for(i <- 0 until KERNEL_SIZE)
          sum += testData(i + n) * testWeight(i)
        sum
      })
    println("Data:" + testData)
    println("Weight:" + testWeight)
    println("Expected:             " + expectedResult)

    for (chan <- 0 until c.cols) {
      weightIn(chan).inputs.enqueue(0)
      takestep()
      for (i <- 0 until KERNEL_SIZE) {
        weightIn(chan).inputs.enqueue(testWeight(i))
        takesteps(1)()
      }
    }
    takestep()
    reg_poke(c.io.repeatWeight, 1)
    takestep()

    dataIn(0).inputs.enqueue(0)
    takesteps(1)()
    for(i <- 0 until c.cols - 1) {
      //writeDataAndWeight(testData(i), 0)
      dataIn(0).inputs.enqueue(testData(i % DATA_SIZE))
      takesteps(4)()
    }
//    for(i <- 0 until c.cols) {
//      weightIn(i).inputs.enqueue(0)
//      takesteps(4)()
//    }
//    takestep()
//    writeDataAndWeight(testData(0), 0)
//    writeDataAndWeight(testData(1), 0)

    reg_poke(c.io.stall, 0)
    takestep()
    takesteps(20)()
    for(i <- c.cols - 1 until DATA_SIZE) {
      writeDataAndWeight(testData(i), testWeight((i - (c.cols - 1)) % KERNEL_SIZE))
//      if(i == 5 && cycles == 1) {
//        reg_poke(c.io.kernelSize, 3)
//        takestep()
//      }
//      if(i == 7 && cycles == 1) {
//        reg_poke(c.io.kernelSize, 2)
//      }
    }

    takesteps(20)()
    for(i <- 0 until c.cols + 1)
      writeDataAndWeight(0,testWeight((i+1) % KERNEL_SIZE))
    takesteps(20)()
    for(chan <- 0 until c.rows) {
      // Clear the invalid leading result
       for(pre <- 0 until KERNEL_SIZE if(resultOut(chan).outputs.nonEmpty))
         resultOut(chan).outputs.dequeue()

      val resultGet = resultOut(chan).outputs.toList
      resultOut(chan).outputs.clear()
      println(s"Result Channel $chan Get: " + (resultGet take RESULT_SIZE))
      if(expect(resultGet.size >= expectedResult.size, s"Cycle $cycles Result length doesn't match!"))
      for (i <- expectedResult.indices) {
        expect(resultGet(i) == expectedResult(i),
          msg = s"Cycle$cycles No.$i doesn't match, Expected " + expectedResult(i) + "Got " + resultGet(i))
      }
      //expect((resultGet take RESULT_SIZE) == expectedResult, s"Cycle $cycles")
    }
    takesteps(5)()
  }
  takesteps(100)()
}

class PEAWrapperTester extends ChiselFlatSpec {
  behavior of "PEArray Wrapper"
  backends foreach { backend =>
    it should s"PEArrayWrapper $backend" in {
      Driver(
        () => new PEArrayWrapper(1, 10, 32, 2, 16), backend
      )(c => new PEAWrapperTests(c)) should be (true)
    }
  }
}
