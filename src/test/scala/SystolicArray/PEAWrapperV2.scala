package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

import scala.util.Random

class PEAWrapperV2Tests(c: PEArrayWrapperV2) extends AdvTester(c) {
  val dataIn = DecoupledSource(c.io.dataIn)
  val weightIn = c.io.weightIn.map{peBundle => DecoupledSource(peBundle)}
  val resultOut = c.io.resultOut.map{peBundle => IrrevocableSink(peBundle)}

  val TEST_CYCLES = 100
  resultOut.foreach(_.outputs.clear())

  var KERNEL_SIZE_X = 5
  var KERNEL_SIZE_Y = 5
  var STRIDE_X = 5
  var STRIDE_Y = 1
  var KERNEL_SIZE_X_UPDATE = KERNEL_SIZE_X
  var KERNEL_SIZE_Y_UPDATE = KERNEL_SIZE_Y
  var STRIDE_X_UPDATE = 5
  var STRIDE_Y_UPDATE = 1
  var pass = true
  reg_poke(c.io.weightUpdate, 1)
  reg_poke(c.io.strideX, STRIDE_X)
  reg_poke(c.io.strideY, STRIDE_Y)
  reg_poke(c.io.kernelSizeX, KERNEL_SIZE_X)
  reg_poke(c.io.kernelSizeY, KERNEL_SIZE_Y)
  for(cycles <- 0 until TEST_CYCLES if pass) {
    KERNEL_SIZE_X = KERNEL_SIZE_X_UPDATE
    KERNEL_SIZE_Y = KERNEL_SIZE_Y_UPDATE
    STRIDE_X = STRIDE_X_UPDATE
    STRIDE_Y = STRIDE_Y_UPDATE
    val DATA_SIZE = 20
    val testData = List.tabulate(DATA_SIZE)(n => Random.nextInt(50))
    val testWeight = List.tabulate(KERNEL_SIZE_Y)(y => List.tabulate(KERNEL_SIZE_X)(x => Random.nextInt(10)))
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
    while(peek(c.io.weightUpdateReady) != 1) {
      takestep()
    }
    for(i <- 0 until KERNEL_SIZE_X) {
      for(chan <- 0 until c.cols) {
        weightIn(chan).inputs.enqueue(
          if(chan < KERNEL_SIZE_Y)
            testWeight(chan)(i)
          else
            0
        )
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
    KERNEL_SIZE_X_UPDATE = Random.nextInt(4) + 2
    KERNEL_SIZE_Y_UPDATE = Random.nextInt(4) + 2
    STRIDE_X_UPDATE = Random.nextInt(7) + 1
    STRIDE_Y_UPDATE = 1
    reg_poke(c.io.weightUpdate, 1)
    reg_poke(c.io.strideX, STRIDE_X_UPDATE)
    reg_poke(c.io.strideY, STRIDE_Y_UPDATE)
    reg_poke(c.io.kernelSizeX, KERNEL_SIZE_X_UPDATE)
    reg_poke(c.io.kernelSizeY, KERNEL_SIZE_Y_UPDATE)
    takesteps(20)()
    val resultGet = List.tabulate(resultOut.size){n => resultOut(n).outputs.toList}
    resultOut.foreach(_.outputs.clear())
    for(chan <- expectedResult.indices) {
      for(i <- expectedResult(chan).indices if i < resultGet(chan).size) {
        expect(resultGet(chan)(i) == expectedResult(chan)(i),
          msg = s"\nChannel $chan.$i " +
            s"Expect " + expectedResult(chan)(i) + " Get " + resultGet(chan)(i))
        if(resultGet(chan)(i) != expectedResult(chan)(i))
          pass = false
      }
    }
    if(true) {
      println("")
      println(s"In iteration $cycles")
      println("DATA " + testData)
      println("WEIGHT" + testWeight)
      println(s"Kernel Size: ($KERNEL_SIZE_X, $KERNEL_SIZE_Y)")
      println(s"Stride: ($STRIDE_X, $STRIDE_Y)")
      for(chan <- 0 until KERNEL_SIZE_Y) {
        println(s"ExpectChan$chan " + expectedResult(chan))
        println(s"ResultGet$chan  " + resultGet(chan))
      }
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
