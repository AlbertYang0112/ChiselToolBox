package SystolicArray

import chisel3._
import chisel3.util._
import chisel3.util.EnqIO

trait PEAWState {
  val STATE_WIDTH =       3

  val WEIGHT_CLEAR =      0
  val WEIGHT_QUEUE_FILL = 1
  val WEIGHT_REFRESH =    2
  val DATA_FLOW =         3
  val DATA_CLEAR =        4
  val WEIGHT_REFLOW =     5
}

// Code Relating TodoList
// Todo: Modularization
// Todo: Add the type annotation
// Todo: Remove the redundancy

// Function Relating TodoList
// Main:
// Todo: Continuous processing between lines.
// Todo: Weight rolling.
// Todo: Stretch pad memory for weight storage.
// Todo: SIMD.
// Todo: MIMD.
// Todo: Result buffer.
// Todo: Interconnection unit and global scheduler between systolic array blocks.
// Todo: DMA / PCIE (May be implemented in Verilog with Xilinx IP cores)
// Auxiliary:
// Todo: Add padding control(the module now is with padding).


class PEArrayWrapperV2(
                        val dataWidth: Int,
                        val weightWidth: Int,
                        val weightChanNum: Int,
                        val rows: Int,
                        val cols: Int,
                        val PEResultFIFODepth: Int,
                        val wrapFIFODepth: Int,
                        val chanFIFODepth: Int
                      ) extends Module with PEAWState {

  val resultWidth: Int = dataWidth + weightWidth
  val io = IO(new Bundle{
    // Data
    val dataIn = DeqIO(UInt(dataWidth.W))
    val weightIn = Vec(weightChanNum, DeqIO(UInt(weightWidth.W)))
    val resultOut = Vec(rows, EnqIO(UInt(resultWidth.W)))
    val lineLength = Input(UInt(8.W))
    // Control
    val weightUpdate = Input(Bool())
    val weightUpdateReady = Output(Bool())
    val kernelSizeX = Input(UInt(8.W))
    val kernelSizeY = Input(UInt(8.W))
    val strideX = Input(UInt(8.W))
    val strideY = Input(UInt(8.W))
    val flush = Input(Bool())
    val continuous = Input(Bool())
  })

  // Components
  val PEA = Module(new PEArrayV2(
    rows = rows,
    cols = cols,
    dataWidth = dataWidth,
    weightWidth = weightWidth,
    resultFIFODepth = PEResultFIFODepth))
  val rowController = Module(new PEARowController(rows = rows, spikeAt = -1))
  val colController = Module(new PEAColController(cols = cols))
  val weightBuffer = Module(new PEAWeightBuffer(
    cols = cols,
    weightWidth = weightWidth,
    bufferDepth = wrapFIFODepth))

  // IO Buffer
  val dataInQueueInput = Wire(EnqIO(UInt(dataWidth.W)))
  val dataInQueue = Queue(dataInQueueInput, wrapFIFODepth)
  private val resultOutQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, wrapFIFODepth))

  // States
  val flowCounter = RegInit(0.U(5.W))           //  Todo: Parameterize the width
  val resultOutCounter = RegInit(0.U(3.W))
  val state: UInt = RegInit(WEIGHT_CLEAR.U(STATE_WIDTH.W))
  val dataCounter = RegInit(0.U(8.W))
  val weightFlowConter = RegInit(0.U(3.W))
  val weightCount = RegInit(0.U(3.W))

  // State Indicator
  private val resultAllReady = Cat(PEA.io.ioArray.map(_.out.result.ready)).andR()
  private val weightAllValid = Cat(PEA.io.ioArray.map(_.in.weight.valid)).andR()
  private val allChannelReady = dataInQueue.valid & weightAllValid & resultAllReady
  private val anyDataChannelFire = Cat(PEA.io.ioArray.map(_.in.data.fire())).orR()
  private val anyWeightChannelFire = Cat(PEA.io.ioArray.map(_.in.weight.fire())).orR()

  // PEA Controlling Signal
  private val weightFlow = List.fill(cols)(Wire(Bool()))
  private val dataFlow = List.fill(rows)(Wire(Bool()))
  private val controlFlow = List.fill(rows)(Wire(Bool()))
  private val controlOutputSum = List.fill(cols)(Wire(Bool()))
  private val controlCalculate = List.fill(cols)(Wire(Bool()))
  private val controlClearSum = List.fill(cols)(Wire(Bool()))
  val weightFlowEnable = Mux(state === DATA_FLOW.U, allChannelReady, true.B)
  val dataFlowEnable = Mux(state === DATA_FLOW.U, allChannelReady, false.B)
  val activeDataChannel = List.fill(cols)(Wire(Bool()))
  val activeCol = List.fill(cols)(Wire(Bool()))

  private val anyDataFlow = Cat(dataFlow).orR()

  // Parameters
  val strideX = RegInit(1.U(3.W))               // Todo: Parameterize the width
  val strideY = RegInit(1.U(3.W))
  val kernelSizeX = RegInit((rows - 1).U(3.W))  // Todo: Parameterize the width
  val kernelSizeY = RegInit((cols - 1).U(3.W))


  val weightRefreshPrev = RegNext(io.weightUpdate)
  private val weightRefreshReq = io.weightUpdate & !weightRefreshPrev
  private val weightRefreshDone = !io.weightUpdate & weightRefreshPrev
  val repeat = RegInit(false.B)

  def setAllChannelControl(calculate: Boolean, outputSum: Boolean, clearSum: Boolean): Unit = {
    for (chan <- 0 until cols) {
      controlClearSum(chan) := clearSum.B
      controlCalculate(chan) := calculate.B
      controlOutputSum(chan) := outputSum.B
    }
  }
  def setAllWeightFlow(flow: Boolean): Unit = {
    weightFlow.foreach(_ := flow.B)
  }
  def setAllDataFlow(flow: Boolean): Unit = {
    dataFlow.foreach(_ := flow.B)
  }
  def setAllControlFlow(flow: Boolean): Unit = {
    controlFlow.foreach(_ := flow.B)
  }
  def enableControl(chan: Int): Unit = {
    PEA.io.ioArray(chan).in.control.valid := true.B
  }
  def disableControl(chan: Int): Unit = {
    PEA.io.ioArray(chan).in.control.valid := false.B
  }
  def enableAllControl(force: Boolean = false): Unit = {
    PEA.io.ioArray.foreach(_.in.control.valid := true.B)
  }
  def disableAllControl(): Unit = {
    PEA.io.ioArray.foreach(_.in.control.valid := false.B)
  }
  for(chan <- 0 until cols) {
    PEA.io.ioArray(chan).in.colActivate := activeCol(chan)
  }
  io.weightUpdateReady := state === WEIGHT_QUEUE_FILL.U | state === WEIGHT_REFLOW.U

  when(weightBuffer.io.weightOut.head.fire()) {
    weightFlowConter := Mux(weightFlowConter === weightCount - 1.U, 0.U, weightFlowConter + 1.U)
  }

  def dataChannelEnq(cond: Bool): Bool= {
    when(cond) {
      dataFlow.foreach(_ := true.B)
      for(col <- 0 until cols) {
        weightFlow(col) := col.U <= flowCounter | state =/= DATA_FLOW.U
      }
      //weightFlow.foreach(_ := true.B)
      enableAllControl()
      setAllControlFlow(true)
      when(PEA.io.ioArray.head.in.data.ready) {
      //when(Mux(state === DATA_FLOW.U, anyDataChannelFire, PEA.io.ioArray.head.in.data.ready)) {
        when(flowCounter < Mux(kernelSizeY > kernelSizeX, kernelSizeY, kernelSizeX) - 0.U) {
          flowCounter := flowCounter + 1.U
        }
        dataCounter := dataCounter + 1.U
        //flowCounter := Mux(flowCounter === kernelSizeX - 1.U, 0.U, flowCounter + 1.U)
      }
      for (row <- 0 until rows) {
        controlCalculate(row) := activeDataChannel(row)
      }
      //for(row <- 0 until rows) {
      //  controlClearSum(row) := rowController.io.activeSpike(row)
      //  controlOutputSum(row) := rowController.io.activeSpike(row)
      //}
    } .otherwise {
      setAllDataFlow(false)
      setAllWeightFlow(false)
      controlCalculate.foreach(_ := false.B)
      //for(row <- 0 until rows) {
      //  controlClearSum(row) := rowController.io.activeSpike(row)
      //  controlOutputSum(row) := rowController.io.activeSpike(row)
      //}
      setAllControlFlow(false)
      disableAllControl()
    }
    cond
  }
  for(row <- 0 until rows) {
    controlClearSum(row) := rowController.io.activeSpike(row)
    controlOutputSum(row) := rowController.io.activeSpike(row)
  }

  // State Machine Uints
  def stateWeightClear(): Unit = {
    repeat := false.B
    setAllWeightFlow(flow = true)
    //when(Cat(weightInQueue.map(_.valid)).orR()) {
    when(Cat(weightBuffer.io.weightOut.map(_.valid)).orR()) {
      flowCounter := 0.U
    } .otherwise {
      flowCounter := Mux(flowCounter === (cols - 1).U, 0.U, flowCounter + 1.U)
      when(flowCounter === (cols - 1).U) {
        // Todo: Change the parameter update method
        state := WEIGHT_QUEUE_FILL.U
        strideX := io.strideX     // Todo: Check the strideX
        kernelSizeX := io.kernelSizeX // Todo: Check the kernelSizeX
        kernelSizeY := io.kernelSizeY
      }
    }

    weightCount := 0.U

    controlCalculate.foreach(_ := false.B)
    controlClearSum.foreach(_ := false.B)
    for(row <- 0 until rows) {
      controlOutputSum(row) := rowController.io.activeSpike(row)
    }
    setAllControlFlow(true)
    enableAllControl(force = true)

    setAllDataFlow(false)
  }

  def stateWeightQueueFill(): Unit = {
    // Refresh the weight in the array
    when(flowCounter === kernelSizeX - 1.U) {
      state := DATA_FLOW.U
      weightFlowConter := 0.U
      //flowCounter := 0.U
    } .elsewhen(weightAllValid) {
      flowCounter := Mux(flowCounter === kernelSizeX - 1.U, 0.U, flowCounter + 1.U)
    }

    when(weightAllValid) {
      // All weights are ready to fire.
      for(col <- 0 until cols) {
        when(col.U < flowCounter) {
          weightFlow(col) := true.B
        } .otherwise {
          weightFlow(col) := false.B
        }
      }
    } .otherwise {
      setAllWeightFlow(false)
    }
    setAllDataFlow(false)
    setAllChannelControl(calculate = false, outputSum = false, clearSum = false)
    setAllControlFlow(true)
    disableAllControl()
  }

  def stateDataFlow(): Unit = {
    when(weightRefreshReq) {
      state := DATA_CLEAR.U
      flowCounter := 0.U
      setAllDataFlow(false)
      setAllWeightFlow(false)
      setAllChannelControl(calculate = false, outputSum = false, clearSum = false)
      setAllControlFlow(false)
      disableAllControl()
    } .otherwise {
      dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
    }
  }

  def stateDataClear(): Unit = {
    when(dataInQueue.valid) {
      dataChannelEnq(cond = resultAllReady)
      flowCounter := 0.U
    } .elsewhen(flowCounter =/= Mux(kernelSizeY > kernelSizeX, kernelSizeY, kernelSizeX) - 0.U | Cat(rowController.io.active).orR()) {
      dataChannelEnq(cond = resultAllReady)
    } .otherwise {
      when(io.continuous) {
        state := WEIGHT_REFLOW.U
      } .otherwise {
        state := WEIGHT_CLEAR.U
      }
      dataChannelEnq(cond = resultAllReady)
    }
  }

  def stateWeightReflow(): Unit = {
    when(weightFlowConter === 0.U) {
      setAllWeightFlow(false)
    } .otherwise {
      setAllWeightFlow(true)
    }
    when(weightFlowConter === 0.U & rowController.io.presetDone) {
      state := DATA_FLOW.U
    }
    setAllDataFlow(false)
    setAllChannelControl(calculate = false, outputSum = false, clearSum = false)
    setAllControlFlow(true)
    disableAllControl()
  }

  when(state === WEIGHT_CLEAR.U) {
    stateWeightClear()
  } .elsewhen(state === WEIGHT_QUEUE_FILL.U) {
    when(weightRefreshDone) {
      state := WEIGHT_REFRESH.U
      flowCounter := 0.U
      repeat := true.B
    } .otherwise {
      repeat := false.B
    }
    when(io.weightIn.head.fire()) {
      weightCount := weightCount + 1.U
    }
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllControlFlow(true)
    enableAllControl(false)
    setAllDataFlow(false)
    setAllWeightFlow(false)
  } .elsewhen(state === WEIGHT_REFRESH.U) {
    stateWeightQueueFill()
  } .elsewhen(state === DATA_FLOW.U) {
    stateDataFlow()
  } .elsewhen(state === DATA_CLEAR.U) {
    stateDataClear()
  } .elsewhen(state === WEIGHT_REFLOW.U) {
    stateWeightReflow()
  } .otherwise {
    state := DATA_CLEAR.U
    // Todo: Fix the bug in data clear stage
    setAllDataFlow(false)
    setAllWeightFlow(false)
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllControlFlow(true)
    disableAllControl()
  }

  // IO <- Result Buffer
  for(row <- 0 until rows) {
    io.resultOut(row).bits := resultOutQueue(row).bits
    io.resultOut(row).valid := resultOutQueue(row).valid
    resultOutQueue(row).ready := io.resultOut(row).ready
    //io.resultOut(row) <> resultOutQueue(row)
  }

  // Data Buffer <- IO
  dataInQueueInput.bits := io.dataIn.bits
  dataInQueueInput.valid := Mux(state === DATA_FLOW.U, io.dataIn.valid, false.B)
  io.dataIn.ready := Mux(state === DATA_FLOW.U, dataInQueueInput.ready, false.B)

  // Data Channel <- Data Buffer
  for(row <- 0 until rows) {
    PEA.io.ioArray(row).in.data.bits := Mux(
      state === DATA_CLEAR.U & !dataInQueue.valid,
      0.U(dataWidth.W),
      dataInQueue.bits)
    PEA.io.ioArray(row).in.data.valid := Mux(
      state === DATA_CLEAR.U & !dataInQueue.valid,
      false.B,
      dataInQueue.valid
    ) & activeDataChannel(row)
    PEA.io.ioArray(row).out.data.ready := dataFlow(row) //& activeDataChannel(row)
  }
  dataInQueue.ready := Cat(PEA.io.ioArray.map(_.in.data.ready)).orR()

  // Weight Channel <- Weight Buffer
  for(col <- 0 until cols) {
    //PEA.io.ioArray(col).in.weight <> weightInQueue(col)
    PEA.io.ioArray(col).in.weight <> weightBuffer.io.weightOut(col)
    PEA.io.ioArray(col).out.weight.ready := weightFlow(col) & weightFlowEnable
  }

  // Control Signal <- PEA Control Signals
  for(col <- 0 until cols) {
    PEA.io.ioArray(col).in.control.bits.outputSum := controlOutputSum(col)
    PEA.io.ioArray(col).in.control.bits.calculate := controlCalculate(col)
    PEA.io.ioArray(col).in.control.bits.clearSum := controlClearSum(col)
    //PEA.io.ioArray(col).in.control.valid := true.B
    PEA.io.ioArray(col).out.control.ready := controlFlow(col)
  }

  for(col <- 0 until cols) {
    weightBuffer.io.weightIn(col) <> io.weightIn(col)
  }
  weightBuffer.io.weightRepeat := repeat
  weightBuffer.io.weightInputEnable := state === WEIGHT_QUEUE_FILL.U
  weightBuffer.io.weightShift := false.B

  rowController.io.kernelSizeX := kernelSizeX
  rowController.io.strideX := strideX
  rowController.io.flow := anyDataFlow | state === DATA_CLEAR.U
  rowController.io.clear := state === DATA_CLEAR.U & !dataInQueue.valid
  rowController.io.controlValid := PEA.io.ioArray.head.in.control.valid
  /*
  when(state === DATA_FLOW.U) {
    rowController.io.outputEnable := true.B
  } .elsewhen(state === DATA_CLEAR.U & flowCounter < kernelSizeX - 1.U) {
    rowController.io.outputEnable := true.B
  } .otherwise {
    rowController.io.outputEnable := false.B
  }
  */
  rowController.io.outputEnable := state === DATA_FLOW.U | state === DATA_CLEAR.U
  rowController.io.presetRequest := state === WEIGHT_QUEUE_FILL.U | state === WEIGHT_REFLOW.U
  rowController.io.stopReallocate := state === DATA_CLEAR.U
  //rowController.io.clear := state === WEIGHT_CLEAR.U
  for(chan <- 0 until rows) {
    activeDataChannel(chan) := rowController.io.active(chan)
  }
  colController.io.kernelSize := kernelSizeY
  colController.io.stride := strideY
  colController.io.outputEnable := state === DATA_FLOW.U | state === WEIGHT_CLEAR.U | anyDataFlow
  for(col <- 0 until cols) {
    activeCol(col) := colController.io.active(col)
  }

  // Unused IO
  for(row <- 0 until rows) {
    //PEA.io.ioArray(row).in.result.enq(0.U(resultWidth.W))
    PEA.io.ioArray(row).in.result.valid := false.B
    PEA.io.ioArray(row).in.result.bits := 0.U(resultWidth.W)
  }
}

class PEAWeightBuffer(
                     val cols: Int,
                     val weightWidth: Int,
                     val bufferDepth: Int
                     ) extends Module {
  val io = IO(new Bundle{
    val weightIn = Vec(cols, DeqIO(UInt(weightWidth.W)))
    val weightOut = Vec(cols, EnqIO(UInt(weightWidth.W)))
    val weightRepeat = Input(Bool())
    val weightShift = Input(Bool())
    val weightInputEnable = Input(Bool())
  })
  val weightBufferInput = List.fill(cols)(Wire(EnqIO(UInt(weightWidth.W))))
  val weightBuffer = List.tabulate(cols)(n => Queue(weightBufferInput(n), bufferDepth))
  for(col <- 0 until cols) {
    when(io.weightShift) {
      weightBufferInput(col).valid := weightBuffer(if(col == cols - 1) 0 else col + 1).fire()
      weightBufferInput(col).bits := weightBuffer(if(col == cols - 1) 0 else col + 1).bits
      io.weightIn(col).ready := false.B
    } .elsewhen(io.weightRepeat) {
      weightBufferInput(col).valid := weightBuffer(col).fire()
      weightBufferInput(col).bits := weightBuffer(col).bits
      io.weightIn(col).ready := false.B
    } .otherwise {
      weightBufferInput(col).valid := io.weightIn(col).valid
      weightBufferInput(col).bits := io.weightIn(col).bits
      io.weightIn(col).ready := weightBufferInput(col).ready
    }
    io.weightOut(col) <> weightBuffer(col)
  }
}

class PEARowController(
                    val rows: Int,
                    val spikeAt: Int
                    ) extends Module {
  val io = IO(new Bundle {
    val outputEnable = Input(Bool())
    val kernelSizeX = Input(UInt(3.W))
    //val kernelSizeY = Input(UInt(3.W))
    val strideX = Input(UInt(3.W))
    val flow = Input(Bool())

    val presetRequest = Input(Bool())
    val presetDone = Output(Bool())

    val clear = Input(Bool())
    val stopReallocate = Input(Bool())

    val active = Vec(rows, Output(Bool()))
    val activeSpike = Vec(rows, Output(Bool()))
    val controlValid = Input(Bool())
  })
  val rowFlowCounter:List[UInt] = List.fill(rows)(RegInit(1.U(3.W)))
  val nextActiveChannel = RegInit(0.U(4.W))
  val reallocateCounter = RegInit(0.U(4.W))

  /* Controller Presetting Logic */
  val presetRequestPrev = RegNext(io.presetRequest)
  val presetting = RegInit(false.B)
  val presetCounter = RegInit(0.U(3.W))

  private def preset(): UInt = {
    when(presetCounter < io.kernelSizeX) {
      /* Update the flow counter */
      for(chan <- 0 until rows) {
        when(nextActiveChannel === chan.U) {
          rowFlowCounter(chan) := 0.U
        } .elsewhen(rowFlowCounter(chan) =/= io.kernelSizeX) {
          rowFlowCounter(chan) := rowFlowCounter(chan) + 1.U
        }
      }
      reallocateCounter := Mux(reallocateCounter === io.strideX - 1.U, 0.U, reallocateCounter + 1.U)
      nextActiveChannel := nextActiveChannel + io.strideX -
        Mux(nextActiveChannel + io.strideX >= io.kernelSizeX, io.kernelSizeX, 0.U)
    }
    presetCounter := presetCounter + 1.U
    presetCounter
  }

  val nextActiveChannelStep = RegInit(0.U(3.W))

  private def step() = {
    for(chan <- 0 until rows) {
      when(reallocateCounter === io.strideX - 1.U & nextActiveChannel === chan.U /*& !io.stopReallocate*/) {
        /* Refresh the flow counter and re-active the channel */
        rowFlowCounter(chan) := 0.U
      } .elsewhen(rowFlowCounter(chan) =/= io.kernelSizeX & !io.clear) {
        rowFlowCounter(chan) := rowFlowCounter(chan) + 1.U
      }
    }
    /* Update the reallocate counter and the channel to be actived */
    when(reallocateCounter === io.strideX - 1.U | io.clear) {
      reallocateCounter := 0.U
      nextActiveChannel := nextActiveChannel + nextActiveChannelStep -
        Mux(nextActiveChannel + nextActiveChannelStep >= io.kernelSizeX, io.kernelSizeX, 0.U)
    } .otherwise {
      reallocateCounter := reallocateCounter + 1.U
    }
  }

  io.presetDone := !presetting
  when(io.presetRequest & !presetRequestPrev) {
    presetting := true.B
    presetCounter := 0.U
    nextActiveChannel := 0.U
    reallocateCounter := io.strideX - 1.U
    rowFlowCounter.head := 0.U
    nextActiveChannelStep := io.strideX
    for (chan <- 1 until rows) {
      rowFlowCounter(chan) := io.kernelSizeX
    }
  }
  when(presetting) {
    when(rowFlowCounter.head =/= io.kernelSizeX - 1.U) {
      step()
    }
    when(nextActiveChannelStep > io.kernelSizeX) {
      nextActiveChannelStep := nextActiveChannelStep - io.kernelSizeX
    }
    when(rowFlowCounter.head === io.kernelSizeX - 1.U & nextActiveChannelStep <= io.kernelSizeX) {
      presetting := false.B
      when(io.strideX > io.kernelSizeX) {
        nextActiveChannel := nextActiveChannelStep
      }
      when(io.kernelSizeX === 1.U) {
        reallocateCounter := 0.U
      }
    }
  }

  /* Run */
  when(!presetting & io.flow) {
    step()
  }
  for(chan <- 0 until rows) {
    io.active(chan) := (rowFlowCounter(chan) =/= io.kernelSizeX) & !presetting & io.outputEnable
  }

  /* Active Spike Generator */
  val activeSpike = List.fill(rows)(RegInit(false.B))
  //val activeSpike = List.fill(rows)(Wire(Bool()))
  for(chan <- 0 until rows) {
    when(!presetting) {
      // Normal Run
      when(!io.clear) {
        when(io.flow & rowFlowCounter(chan) === (if (spikeAt >= 0) spikeAt.U else io.kernelSizeX - (-spikeAt).U)) {
          activeSpike(chan) := true.B
        }.elsewhen(io.flow) {
          activeSpike(chan) := false.B
        }
      } .otherwise {
        when(chan.U === nextActiveChannel & rowFlowCounter(chan) < io.kernelSizeX) {
          when(rowFlowCounter(chan) =/= 0.U) {
            activeSpike(chan) := true.B
          } .otherwise {
            activeSpike(chan) := false.B
          }
          rowFlowCounter(chan) := io.kernelSizeX
        } .otherwise {
          when(io.controlValid) {
            activeSpike(chan) := false.B
          }
        }
      }
    } .otherwise {
      activeSpike(chan) := false.B
    }
    io.activeSpike(chan) := activeSpike(chan) & io.outputEnable
  }
}

class PEAColController(
                      val cols: Int
                      ) extends Module {
  val io = IO(new Bundle{
    val kernelSize = Input(UInt(3.W))
    val stride = Input(UInt(3.W))
    val outputEnable = Input(Bool())

    val active = Vec(cols, Output(Bool()))
  })

  val activeChain = List.fill(cols)(RegInit(false.B))

  activeChain.head := io.outputEnable
  for(col <- 1 until cols) {
    activeChain(col) := activeChain(col - 1)
  }

  for(col <- 0 until cols) {
    io.active(col) := col.U < io.kernelSize & activeChain(col)
  }
}
