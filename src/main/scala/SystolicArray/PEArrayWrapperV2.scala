package SystolicArray

import chisel3._
import chisel3.util._
import chisel3.util.EnqIO

trait PEAWState {
  val STATE_WIDTH =       3

  val IDLE =              0
  val WEIGHT_CLEAR =      1
  val WEIGHT_QUEUE_FILL = 2
  val WEIGHT_REFRESH =    3
  val DATA_FLOW =         4
  val DATA_CLEAR =        5
}

class PEArrayWrapperV2(
                        val dataWidth: Int,
                        val weightChanNum: Int,
                        val rows: Int,
                        val cols: Int,
                        val PEResultFIFODepth: Int,
                        val wrapFIFODepth: Int,
                        val chanFIFODepth: Int
                      ) extends Module with PEAWState {

  val io = IO(new Bundle{
    // Data
    val dataIn = DeqIO(UInt(dataWidth.W))
    val weightIn = Vec(weightChanNum, DeqIO(UInt(dataWidth.W)))
    val resultOut = Vec(rows, EnqIO(UInt(dataWidth.W)))
    // Control
    val weightUpdate = Input(Bool())
    val weightUpdateReady = Output(Bool())
    val clearData = Input(Bool())
    val kernelSizeX = Input(UInt(8.W))
    val kernelSizeY = Input(UInt(8.W))
    val strideX = Input(UInt(8.W))
    val strideY = Input(UInt(8.W))
    val flush = Input(Bool())
    val repeat = Input(Bool())
    val stall = Input(Bool())
  })

  val PEA = Module(new PEArrayV2(
    rows = rows, cols = cols,
    dataBits = dataWidth, resultFIFODepth = PEResultFIFODepth))

  val state = RegInit(IDLE.U(STATE_WIDTH.W))

  val weightInQueueInput = List.fill(cols)(Wire(EnqIO(UInt(dataWidth.W))))
  val dataInQueueInput = Wire(EnqIO(UInt(dataWidth.W)))
  //val resultOutQueueInput = Wire(EnqIO(UInt(dataWidth.W)))
  val dataInQueue = Queue(dataInQueueInput, wrapFIFODepth)
  //val dataChanQueue = List.fill(rows)(Queue(dataInQueue, chanFIFODepth))
  val weightInQueue = List.tabulate(cols)(col => Queue(weightInQueueInput(col), chanFIFODepth))
  val resultOutQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, wrapFIFODepth))
  //val resultOutQueue = Queue(resultOutQueueInput, wrapFIFODepth)
  //val resultChanQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, chanFIFODepth))

  for(row <- 0 until rows) {
    io.resultOut(row).bits := resultOutQueue(row).bits
    io.resultOut(row).valid := resultOutQueue(row).valid
    resultOutQueue(row).ready := io.resultOut(row).ready
    //io.resultOut(row) <> resultOutQueue(row)
  }

  //val weightFlow = List.fill(cols)(RegInit(false.B))
  //val dataFlow = List.fill(rows)(RegInit(false.B))
  //val controlOutputSum = List.fill(cols)(RegInit(false.B))
  //val controlCalculate = List.fill(cols)(RegInit(false.B))
  //val controlClearSum = List.fill(cols)(RegInit(false.B))
  val weightFlow = List.fill(cols)(Wire(Bool()))
  val dataFlow = List.fill(rows)(Wire(Bool()))
  val controlOutputSum = List.fill(cols)(Wire(Bool()))
  val controlCalculate = List.fill(cols)(Wire(Bool()))
  val controlClearSum = List.fill(cols)(Wire(Bool()))
  val firstFire = RegInit(false.B)
  val resultAllReady = Cat(PEA.io.ioArray.map(_.out.result.ready)).andR()
  val weightAllValid = Cat(PEA.io.ioArray.map(_.in.weight.valid)).andR()
  val allChannelReady = dataInQueue.valid & weightAllValid & resultAllReady
  val weightFlowEnable = Mux(state === DATA_FLOW.U, allChannelReady, true.B)
  val dataFlowEnable = Mux(state === DATA_FLOW.U, allChannelReady, false.B)
  val weightRefreshPrev = RegNext(io.weightUpdate)
  val weightRefreshReq = io.weightUpdate & !weightRefreshPrev
  val weightRefreshDone = !io.weightUpdate & weightRefreshPrev
  val repeat = RegInit(false.B)

  val flowCounter = RegInit(0.U(5.W))

  dataInQueueInput.bits := io.dataIn.bits
  dataInQueueInput.valid := Mux(state === DATA_FLOW.U, io.dataIn.valid, false.B)
  io.dataIn.ready := Mux(state === DATA_FLOW.U, dataInQueueInput.ready, false.B)

  def dataChannelEnq(cond: Bool) = {
    when(cond) {
      dataFlow.foreach(_ := true.B)
      weightFlow.foreach(_ := true.B)
      when(PEA.io.ioArray.head.in.data.fire()) {
        flowCounter := Mux(flowCounter === (cols - 1).U, 0.U, flowCounter + 1.U)
      }
      for(col <- 0 until cols) {
        controlCalculate(col) := true.B
        controlClearSum(col) := Mux(col.U === flowCounter, true.B, false.B)
        controlOutputSum(col) := Mux(col.U === flowCounter, true.B, false.B) & !firstFire
      }
    } .otherwise {
      controlCalculate.foreach(_ := false.B)
      controlOutputSum.foreach(_ := false.B)
      controlClearSum.foreach(_ := false.B)
      dataFlow.foreach(_ := false.B)
      weightFlow.foreach(_ := false.B)
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
  io.weightUpdateReady := state === WEIGHT_QUEUE_FILL.U

  // Todo: Block the weight input in other states except WEIGHT_QUEUE_FILL.
  when(state === IDLE.U(5.W)) {
    when(weightRefreshReq) {
      state := WEIGHT_CLEAR.U
      flowCounter := 0.U
      repeat := false.B
    } .otherwise {
      repeat := true.B
    }
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllDataFlow(false)
    setAllWeightFlow(false)
  } .elsewhen(state === WEIGHT_CLEAR.U) {
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
    setAllDataFlow(false)
  } .elsewhen(state === WEIGHT_QUEUE_FILL.U) {
    when(weightRefreshDone) {
      state := WEIGHT_REFRESH.U
      flowCounter := 0.U
      repeat := true.B
    } .otherwise {
      repeat := false.B
    }
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
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
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
    setAllDataFlow(false)
  } .elsewhen(state === DATA_FLOW.U) {
    when(weightRefreshReq | io.clearData) {
      state := DATA_CLEAR.U
      setAllDataFlow(false)
      setAllWeightFlow(false)
      setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
      firstFire := false.B
    } .otherwise {
      dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
      when(dataInQueue.valid & resultAllReady & flowCounter === (cols - 1).U) {
        firstFire := false.B
      }
    }
  } .elsewhen(state === DATA_CLEAR.U) {
    when(!firstFire) {
      // Clear the data remained in the queue
      dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
      when(!dataInQueue.valid) {
        firstFire := true.B
      }
    } .elsewhen(flowCounter === 0.U) {
      state := IDLE.U
      setAllDataFlow(false)
      setAllWeightFlow(false)
      setAllChannelControl(calculate = false, outputSum = true, clearSum = true)
    } .otherwise {
      // Push 0 into the dataChannel
      dataChannelEnq(cond = resultAllReady)
    }
  } .otherwise {
    state := DATA_CLEAR.U
    firstFire := false.B
    setAllDataFlow(false)
    setAllWeightFlow(false)
    setAllChannelControl(calculate = false, outputSum = false, clearSum = true)
  }

  // Link the weight channel
  for(col <- 0 until cols) {
    PEA.io.ioArray(col).in.weight <> weightInQueue(col)
    PEA.io.ioArray(col).out.weight.ready := weightFlow(col) & weightFlowEnable
    PEA.io.ioArray(col).in.control.bits.outputSum := controlOutputSum(col)
    PEA.io.ioArray(col).in.control.bits.calculate := controlCalculate(col)
    PEA.io.ioArray(col).in.control.bits.clearSum := controlClearSum(col)
    PEA.io.ioArray(col).in.control.valid := true.B
    PEA.io.ioArray(col).out.control.ready := true.B
  }

  // Weight Repeat
  for(col <- 0 until cols) {
    weightInQueueInput(col).valid := Mux(repeat,
      weightInQueue(col).fire(), io.weightIn(col).valid)
    weightInQueueInput(col).bits := Mux(repeat,
      weightInQueue(col).bits, io.weightIn(col).bits)
    io.weightIn(col).ready := Mux(repeat,
      false.B, weightInQueueInput(col).ready
    )
  }
  for(row <- 0 until rows) {
    PEA.io.ioArray(row).in.data.bits := Mux(state === DATA_CLEAR.U & firstFire, 0.U(dataWidth.W), dataInQueue.bits)
    PEA.io.ioArray(row).in.data.valid := Mux(state === DATA_CLEAR.U & firstFire, true.B, dataInQueue.valid)
    PEA.io.ioArray(row).out.data.ready := dataFlow(row)
  }

}
