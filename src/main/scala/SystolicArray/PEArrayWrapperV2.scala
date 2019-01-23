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
}

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

  val resultWidth = dataWidth + weightWidth
  val io = IO(new Bundle{
    // Data
    val dataIn = DeqIO(UInt(dataWidth.W))
    val weightIn = Vec(weightChanNum, DeqIO(UInt(weightWidth.W)))
    val resultOut = Vec(rows, EnqIO(UInt(resultWidth.W)))
    // Control
    val weightUpdate = Input(Bool())
    val weightUpdateReady = Output(Bool())
    val kernelSizeX = Input(UInt(8.W))
    val kernelSizeY = Input(UInt(8.W))
    val strideX = Input(UInt(8.W))
    val strideY = Input(UInt(8.W))
    val flush = Input(Bool())
    val activeChan = Output(UInt(rows.W))
  })

  val PEA = Module(new PEArrayV2(
    rows = rows,
    cols = cols,
    dataWidth = dataWidth,
    weightWidth = weightWidth,
    resultFIFODepth = PEResultFIFODepth))

  val state = RegInit(WEIGHT_CLEAR.U(STATE_WIDTH.W))

  private val weightInQueueInput = List.fill(cols)(Wire(EnqIO(UInt(weightWidth.W))))
  val dataInQueueInput = Wire(EnqIO(UInt(dataWidth.W)))
  //val resultOutQueueInput = Wire(EnqIO(UInt(resultWidth.W)))
  val dataInQueue = Queue(dataInQueueInput, wrapFIFODepth)
  //val dataChanQueue = List.fill(rows)(Queue(dataInQueue, chanFIFODepth))
  private val weightInQueue = List.tabulate(cols)(col => Queue(weightInQueueInput(col), chanFIFODepth))
  private val resultOutQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, wrapFIFODepth))
  //val resultOutQueue = Queue(resultOutQueueInput, wrapFIFODepth)
  //val resultChanQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, chanFIFODepth))

  for(row <- 0 until rows) {
    io.resultOut(row).bits := resultOutQueue(row).bits
    io.resultOut(row).valid := resultOutQueue(row).valid
    resultOutQueue(row).ready := io.resultOut(row).ready
    //io.resultOut(row) <> resultOutQueue(row)
  }

  private val weightFlow = List.fill(cols)(Wire(Bool()))
  private val dataFlow = List.fill(rows)(Wire(Bool()))
  private val controlFlow = List.fill(rows)(Wire(Bool()))
  private val controlOutputSum = List.fill(cols)(Wire(Bool()))
  private val controlCalculate = List.fill(cols)(Wire(Bool()))
  private val controlClearSum = List.fill(cols)(Wire(Bool()))
  val firstFire = RegInit(false.B)
  private val resultAllReady = Cat(PEA.io.ioArray.map(_.out.result.ready)).andR()
  private val weightAllValid = Cat(PEA.io.ioArray.map(_.in.weight.valid)).andR()
  private val allChannelReady = dataInQueue.valid & weightAllValid & resultAllReady
  private val anyDataChannelFire = Cat(PEA.io.ioArray.map(_.in.data.fire())).orR()
  private val anyWeightChannelFire = Cat(PEA.io.ioArray.map(_.in.weight.fire())).orR()
  val weightFlowEnable = Mux(state === DATA_FLOW.U, allChannelReady, true.B)
  val dataFlowEnable = Mux(state === DATA_FLOW.U, allChannelReady, false.B)
  val weightRefreshPrev = RegNext(io.weightUpdate)
  private val weightRefreshReq = io.weightUpdate & !weightRefreshPrev
  private val weightRefreshDone = !io.weightUpdate & weightRefreshPrev
  val repeat = RegInit(false.B)

  val flowCounter = RegInit(0.U(5.W))           //  Todo: Parameterize the width

  // Data allocation
  val dataChanFlowCounter = Mem(cols, UInt(3.W))   // Todo: Parameterize the width
  val dataChanLastActive = RegInit(0.U(3.W))
  val activeDataChannel = List.fill(cols)(Wire(Bool()))
  val dataReallocateCounter = RegInit(0.U(3.W))

  val strideX = RegInit(1.U(3.W))               // Todo: Parameterize the width
  val kernelSizeX = RegInit((cols - 1).U(3.W))  // Todo: Parameterize the width

  dataInQueueInput.bits := io.dataIn.bits
  dataInQueueInput.valid := Mux(state === DATA_FLOW.U, io.dataIn.valid, false.B)
  io.dataIn.ready := Mux(state === DATA_FLOW.U, dataInQueueInput.ready, false.B)

  def dataChannelEnq(cond: Bool) = {
    when(cond) {
      dataFlow.foreach(_ := true.B)
      weightFlow.foreach(_ := true.B)
      when(Mux(state === DATA_FLOW.U, PEA.io.ioArray.head.in.data.fire(), PEA.io.ioArray.head.in.data.ready)) {
        flowCounter := Mux(flowCounter === (cols - 1).U, 0.U, flowCounter + 1.U)
        enableAllControl
        setAllControlFlow(true)
      } .otherwise {
        flowCounter := flowCounter
        disableAllControl
        setAllControlFlow(false)
      }
      controlCalculate.foreach(_ := true.B)
      for(col <- 0 until cols - 1) {
        controlClearSum(col) := Mux((col + 1).U === flowCounter, true.B, false.B)
        controlOutputSum(col) := Mux((col + 1).U === flowCounter, true.B, false.B)
      }
      controlClearSum(cols - 1) := Mux(flowCounter === 0.U, true.B, false.B) & !firstFire
      controlOutputSum(cols - 1) := Mux(0.U === flowCounter, true.B, false.B) & !firstFire
    } .otherwise {
      setAllDataFlow(false)
      setAllWeightFlow(false)
      setAllChannelControl(calculate = false, outputSum = false, clearSum = false)
      setAllControlFlow(false)
      disableAllControl
    }
    cond
  }

  def setAllChannelControl(calculate: Boolean, outputSum: Boolean, clearSum: Boolean) = {
    controlClearSum.foreach(_ := clearSum.B)
    controlCalculate.foreach(_ := calculate.B)
    controlOutputSum.foreach(_ := outputSum.B)
  }
  def setAllWeightFlow(flow: Boolean) = {
    weightFlow.foreach(_ := flow.B)
  }
  def setAllDataFlow(flow: Boolean) = {
    dataFlow.foreach(_ := flow.B)
  }
  def setAllControlFlow(flow: Boolean) = {
    controlFlow.foreach(_ := flow.B)
  }
  def enableControl(chan: Int) = {
    PEA.io.ioArray(chan).in.control.valid := true.B
  }
  def disableControl(chan: Int) = {
    PEA.io.ioArray(chan).in.control.valid := false.B
  }
  def enableAllControl = {
    PEA.io.ioArray.foreach(_.in.control.valid := true.B)
  }
  def disableAllControl = {
    PEA.io.ioArray.foreach(_.in.control.valid := false.B)
  }
  io.weightUpdateReady := state === WEIGHT_QUEUE_FILL.U

  when(state === WEIGHT_CLEAR.U) {
    repeat := false.B
    setAllWeightFlow(true)
    when(Cat(weightInQueue.map(_.valid)).orR()) {
      flowCounter := 0.U
    } .otherwise {
      flowCounter := Mux(flowCounter === (cols - 1).U, 0.U, flowCounter + 1.U)
      when(flowCounter === (cols - 1).U) {
        state := WEIGHT_QUEUE_FILL.U
      }
    }

    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllControlFlow(true)
    enableAllControl

    setAllDataFlow(false)
  } .elsewhen(state === WEIGHT_QUEUE_FILL.U) {
    when(weightRefreshDone) {
      state := WEIGHT_REFRESH.U
      strideX := io.strideX     // Todo: Check the strideX
      kernelSizeX := io.kernelSizeX // Todo: Check the kernelSizeX
      flowCounter := 0.U
      repeat := true.B
    } .otherwise {
      repeat := false.B
    }
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllControlFlow(true)
    enableAllControl
    setAllDataFlow(false)
    setAllWeightFlow(false)
  } .elsewhen(state === WEIGHT_REFRESH.U) {
    // Refresh the weight in the array
    when(flowCounter === (cols - 1).U) {
      state := DATA_FLOW.U
      flowCounter := 0.U
      firstFire := true.B
    } .elsewhen(weightAllValid) {
      flowCounter := Mux(flowCounter === (cols - 1).U, 0.U, flowCounter + 1.U)
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
    setAllChannelControl(calculate = true, outputSum = false, clearSum = false)
    setAllControlFlow(true)
    disableAllControl
  } .elsewhen(state === DATA_FLOW.U) {
    when(weightRefreshReq) {
      state := DATA_CLEAR.U
      setAllDataFlow(false)
      setAllWeightFlow(false)
      setAllChannelControl(calculate = false, outputSum = false, clearSum = false)
      setAllControlFlow(false)
      disableAllControl
      firstFire := false.B
    } .otherwise {
      dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
      when(dataInQueue.valid & resultAllReady & flowCounter === (cols - 1).U) {
        firstFire := false.B
      }
    }
  } .elsewhen(state === DATA_CLEAR.U) {
    when(Cat(PEA.io.ioArray.map(_.out.data.valid)).orR() | flowCounter =/= 0.U) {
      dataChannelEnq(cond = resultAllReady)
    } .otherwise {
      state := WEIGHT_CLEAR.U
      setAllDataFlow(false)
      setAllWeightFlow(false)
      //setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
      for(col <- 0 until cols - 1) {
        controlCalculate(col) := false.B
        controlOutputSum(col) := false.B
        controlClearSum(col) := false.B
        //disableControl(col)
      }
      controlCalculate(cols - 1) := false.B
      controlOutputSum(cols - 1) := true.B
      controlClearSum(cols - 1) := false.B
      //enableControl(cols - 1)
      setAllControlFlow(true)
      enableAllControl
    }
    /*when(!firstFire) {
      // Clear the data remained in the queue
      when(!Cat(PEA.io.ioArray.map(_.out.data.valid)).orR) {
        when(flowCounter === 0.U) {
          state := WEIGHT_CLEAR.U
        } .otherwise {
          firstFire := true.B
        }
        setAllDataFlow(false)
        setAllWeightFlow(false)
        setAllChannelControl(calculate = false, outputSum = false, clearSum = false)
        disableAllControl
        setAllControlFlow(false)
      } .otherwise {
        dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
      }
    } .elsewhen(flowCounter === 0.U) {
      state := WEIGHT_CLEAR.U
      setAllDataFlow(false)
      setAllWeightFlow(false)
      setAllChannelControl(calculate = false, outputSum = true, clearSum = true)
      setAllControlFlow(true)
      disableAllControl
    } .otherwise {
      // Push 0 into the dataChannel
      dataChannelEnq(cond = resultAllReady)
    } */
  } .otherwise {
    state := DATA_CLEAR.U
    firstFire := false.B
    setAllDataFlow(false)
    setAllWeightFlow(false)
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllControlFlow(true)
    disableAllControl
  }

  // Link the weight channel
  for(col <- 0 until cols) {
    PEA.io.ioArray(col).in.weight <> weightInQueue(col)
    PEA.io.ioArray(col).out.weight.ready := weightFlow(col) & weightFlowEnable
    PEA.io.ioArray(col).in.control.bits.outputSum := controlOutputSum(col)
    PEA.io.ioArray(col).in.control.bits.calculate := controlCalculate(col)
    PEA.io.ioArray(col).in.control.bits.clearSum := controlClearSum(col)
    //PEA.io.ioArray(col).in.control.valid := true.B
    PEA.io.ioArray(col).out.control.ready := controlFlow(col)
  }

  // Weight Repeat
  for(col <- 0 until cols) {
    weightInQueueInput(col).valid := Mux(repeat,
      weightInQueue(col).fire(), io.weightIn(col).valid & state === WEIGHT_QUEUE_FILL.U)
    weightInQueueInput(col).bits := Mux(repeat,
      weightInQueue(col).bits, io.weightIn(col).bits)
    io.weightIn(col).ready := Mux(repeat,
      false.B, weightInQueueInput(col).ready & state === WEIGHT_QUEUE_FILL.U
    )
  }
  for(row <- 0 until rows) {
    PEA.io.ioArray(row).in.data.bits := Mux(state === DATA_CLEAR.U & !dataInQueue.valid, 0.U(dataWidth.W), dataInQueue.bits)
    PEA.io.ioArray(row).in.data.valid := Mux(state === DATA_CLEAR.U & !dataInQueue.valid, false.B, dataInQueue.valid)
    PEA.io.ioArray(row).out.data.ready := dataFlow(row) //& activeDataChannel(row)
  }
  dataInQueue.ready := Cat(PEA.io.ioArray.map(_.in.data.ready)).orR()

  // Data allocation
  // Global re-allocation counter
  when(state === WEIGHT_REFRESH.U) {
    for(row <- 0 until rows) {
      dataChanFlowCounter(row) := kernelSizeX
    }
  }
  when(state === WEIGHT_REFRESH.U | state === DATA_FLOW.U | state === DATA_CLEAR.U) {
    when(anyDataChannelFire | anyWeightChannelFire) {
      dataReallocateCounter := Mux(dataReallocateCounter === strideX - 1.U, 0.U, dataReallocateCounter + 1.U)
      when(dataReallocateCounter === 0.U) {
        // Reallocate
        dataChanFlowCounter(dataChanLastActive) := 0.U
        dataChanLastActive := dataChanLastActive + strideX -
          Mux(dataChanLastActive +  strideX >= kernelSizeX, kernelSizeX, 0.U)
      }
    }
    for(row <- 0 until rows) {
      when(PEA.io.ioArray(row).in.data.fire() & dataChanFlowCounter(row) =/= kernelSizeX) {
        dataChanFlowCounter(row) := dataChanFlowCounter(row) + 1.U
      }
    }
  }
  for(row <- 0 until rows) {
    activeDataChannel(row) := dataChanFlowCounter(row) =/= kernelSizeX
  }
  io.activeChan := Cat(activeDataChannel)

  // Unused IO
  for(row <- 0 until rows) {
    //PEA.io.ioArray(row).in.result.enq(0.U(resultWidth.W))
    PEA.io.ioArray(row).in.result.valid := false.B
    PEA.io.ioArray(row).in.result.bits := 0.U(resultWidth.W)
  }

}
