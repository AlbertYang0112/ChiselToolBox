package SystolicArray

import chisel3.iotesters.{AdvTester, ChiselFlatSpec, Driver}

import scala.util.Random

class PEAWrapperV2Tests(c: PEArrayWrapperV2) extends AdvTester(c) {
  val dataIn = DecoupledSource(c.io.dataIn)
  val weightIn = c.io.weightIn.map{peBundle => DecoupledSource(peBundle)}
  val resultOut = c.io.resultOut.map{peBundle => IrrevocableSink(peBundle)}

  val TEST_CYCLES = 200000
  resultOut.foreach(_.outputs.clear())

  def Convolution1D(data: List[Int], weight: List[Int], stride: Int): List[Int] = {
    val kernelLength = weight.length
    val dataLength = data.length
    val resultLength = (kernelLength + dataLength - 1) / stride + (
      if((kernelLength + dataLength - 1) % stride != 0) 1 else 0
    )
    List.tabulate(resultLength){
      x => {
        val pos = x * stride
        var sum = 0
        if (pos < kernelLength - 1) {
          for (i <- 0 to pos) {
            sum += data(i) * weight(kernelLength - 1 - pos + i)
          }
        } else {
          for (i <- 0 until kernelLength if pos + i - kernelLength + 1 < data.length)
            sum += data(pos + i - kernelLength + 1) * weight(i)
        }
        sum
      }
    }
  }

  def performFullOneLineTest(): Boolean = {
    var KERNEL_SIZE_X = 1
    var KERNEL_SIZE_Y = 1
    var STRIDE_X = 1
    var STRIDE_Y = 1
    var KERNEL_SIZE_X_UPDATE = KERNEL_SIZE_X
    var KERNEL_SIZE_Y_UPDATE = KERNEL_SIZE_Y
    var STRIDE_X_UPDATE = 1
    var STRIDE_Y_UPDATE = 1
    var pass = true
    var done = false
    reg_poke(c.io.weightUpdate, 1)
    reg_poke(c.io.strideX, STRIDE_X)
    reg_poke(c.io.strideY, STRIDE_Y)
    reg_poke(c.io.kernelSizeX, KERNEL_SIZE_X)
    reg_poke(c.io.kernelSizeY, KERNEL_SIZE_Y)
    for(cycles <- 0 until TEST_CYCLES if pass & !done) {
      KERNEL_SIZE_X = KERNEL_SIZE_X_UPDATE
      KERNEL_SIZE_Y = KERNEL_SIZE_Y_UPDATE
      STRIDE_X = STRIDE_X_UPDATE
      STRIDE_Y = STRIDE_Y_UPDATE
      val DATA_SIZE = 10 + cycles % 5
      val testData = List.tabulate(DATA_SIZE)(n => Random.nextInt(50))
      val testWeight = List.tabulate(KERNEL_SIZE_Y)(y => List.tabulate(KERNEL_SIZE_X)(x => Random.nextInt(10)))
      val expectedResult = List.tabulate(KERNEL_SIZE_Y){y =>
        Convolution1D(testData, testWeight(y), STRIDE_X)
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
          //takestep()
        }
      }
      takesteps(10)()
      reg_poke(c.io.weightUpdate, 0)
      takestep()
      takesteps(2)()
      // Feed data
      for(i <- testData.indices) {
        dataIn.inputs.enqueue(testData(i))
        takesteps(1)()
      }
      takesteps(10)()
      //KERNEL_SIZE_X_UPDATE = Random.nextInt(4) + 2
      //KERNEL_SIZE_Y_UPDATE = Random.nextInt(4) + 2
      //STRIDE_X_UPDATE = Random.nextInt(7) + 1
      //STRIDE_Y_UPDATE = 1
      if(cycles % 5 == 0 && cycles != 0) {
        KERNEL_SIZE_X_UPDATE = KERNEL_SIZE_X + 1
        if (KERNEL_SIZE_X_UPDATE > 5) {
          KERNEL_SIZE_X_UPDATE = 1
          KERNEL_SIZE_Y_UPDATE = KERNEL_SIZE_Y + 1
          if (KERNEL_SIZE_Y_UPDATE > 5) {
            KERNEL_SIZE_Y_UPDATE = 1
            STRIDE_X_UPDATE = STRIDE_X + 1
            if (STRIDE_X_UPDATE > 7) {
              STRIDE_X_UPDATE = 1
              STRIDE_Y_UPDATE = STRIDE_Y + 1
              if (STRIDE_Y_UPDATE > 7) {
                STRIDE_Y_UPDATE = 1
                done = true
              }
            }
          }
        }
      }
      //KERNEL_SIZE_X_UPDATE = 1
      //KERNEL_SIZE_Y_UPDATE = 1
      //STRIDE_X_UPDATE = 1
      //STRIDE_Y_UPDATE = 1
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
          if(resultGet(chan)(i) != expectedResult(chan)(i) || resultGet(chan).size != expectedResult(chan).size)
            pass = false
        }
      }
      if(!pass) {
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
    pass
  }

  def performContinuousLineTest(): Boolean = {
    var KERNEL_SIZE_X = 1
    var KERNEL_SIZE_Y = 1
    var STRIDE_X = 1
    var STRIDE_Y = 1
    var KERNEL_SIZE_X_UPDATE = KERNEL_SIZE_X
    var KERNEL_SIZE_Y_UPDATE = KERNEL_SIZE_Y
    var STRIDE_X_UPDATE = 1
    var STRIDE_Y_UPDATE = 1
    var pass = true
    var done = false

    var testWeight: List[ List[Int] ] = null

    for(cycles <- 0 until TEST_CYCLES if pass & !done) {
      KERNEL_SIZE_X = KERNEL_SIZE_X_UPDATE
      KERNEL_SIZE_Y = KERNEL_SIZE_Y_UPDATE
      STRIDE_X = STRIDE_X_UPDATE
      STRIDE_Y = STRIDE_Y_UPDATE
      if(cycles % 5 == 0) {
        println(s"Cycle: $cycles")
        println(s"KX = $KERNEL_SIZE_X; KY = $KERNEL_SIZE_Y")
        println(s"SX = $STRIDE_X; SY = $STRIDE_Y")
        // Reset the parameters
        testWeight = List.tabulate(KERNEL_SIZE_Y)(y => List.tabulate(KERNEL_SIZE_X)(x => Random.nextInt(10)))
        println(testWeight.toString())
        reg_poke(c.io.weightUpdate, 1)
        reg_poke(c.io.strideX, STRIDE_X)
        reg_poke(c.io.strideY, STRIDE_Y)
        reg_poke(c.io.kernelSizeX, KERNEL_SIZE_X)
        reg_poke(c.io.kernelSizeY, KERNEL_SIZE_Y)
        reg_poke(c.io.continuous, 0)
        takestep()
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
            //takestep()
          }
        }
        takesteps(10)()
        reg_poke(c.io.weightUpdate, 0)
        takestep()
        takesteps(2)()

      }

      val DATA_SIZE = 10 + cycles % 5
      val testData = List.tabulate(DATA_SIZE)(n => Random.nextInt(50))
      val expectedResult = List.tabulate(KERNEL_SIZE_Y){y =>
        Convolution1D(testData,
          testWeight(y),
          STRIDE_X)
      }
      resultOut.foreach(_.outputs.clear())
      // Feed data
      for(i <- testData.indices) {
        dataIn.inputs.enqueue(testData(i))
        takesteps(1)()
      }
      takesteps(10)()
      if(cycles % 5 == 4) {
        KERNEL_SIZE_X_UPDATE = KERNEL_SIZE_X + 1
        if (KERNEL_SIZE_X_UPDATE > 5) {
          KERNEL_SIZE_X_UPDATE = 1
          KERNEL_SIZE_Y_UPDATE = KERNEL_SIZE_Y + 1
          if (KERNEL_SIZE_Y_UPDATE > 5) {
            KERNEL_SIZE_Y_UPDATE = 1
            STRIDE_X_UPDATE = STRIDE_X + 1
            if (STRIDE_X_UPDATE > 7) {
              STRIDE_X_UPDATE = 1
              STRIDE_Y_UPDATE = STRIDE_Y + 1
              if (STRIDE_Y_UPDATE > 7) {
                STRIDE_Y_UPDATE = 1
                done = true
              }
            }
          }
        }
      }
      reg_poke(c.io.weightUpdate, 1)
      reg_poke(c.io.strideX, STRIDE_X_UPDATE)
      reg_poke(c.io.strideY, STRIDE_Y_UPDATE)
      reg_poke(c.io.kernelSizeX, KERNEL_SIZE_X_UPDATE)
      reg_poke(c.io.kernelSizeY, KERNEL_SIZE_Y_UPDATE)
      if(cycles % 5 != 4) {
        reg_poke(c.io.continuous, 1)
        while (peek(c.io.weightUpdateReady) != 1) {
          takestep()
        }
        reg_poke(c.io.weightUpdate, 0)
      } else {
        reg_poke(c.io.continuous, 0)
      }
      takesteps(20)()
      val resultGet = List.tabulate(resultOut.size){n => resultOut(n).outputs.toList}
      resultOut.foreach(_.outputs.clear())
      for(chan <- expectedResult.indices) {
        for(i <- expectedResult(chan).indices if i < resultGet(chan).size) {
          expect(resultGet(chan)(i) == expectedResult(chan)(i),
            msg = s"\nChannel $chan.$i " +
              s"Expect " + expectedResult(chan)(i) + " Get " + resultGet(chan)(i))
          if(resultGet(chan)(i) != expectedResult(chan)(i) || resultGet(chan).size != expectedResult(chan).size)
            pass = false
        }
      }
      if(!pass) {
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
    pass
  }

  performContinuousLineTest()
  //performFullOneLineTest()

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
