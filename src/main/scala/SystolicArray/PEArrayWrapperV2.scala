package SystolicArray

import chisel3._
import chisel3.util._
import chisel3.util.EnqIO

trait PEAWState {
  val STATE_WIDTH = 2
  val IDLE = 0
  val WEIGHT_REFRESH = 1
  val DATA_FLOW = 2
  val CLEAR_DATA = 3
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
    val resultOut = EnqIO(UInt(dataWidth.W))
    // Control
    val weightUpdate = Input(Bool())
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
  val resultOutQueueInput = Wire(EnqIO(UInt(dataWidth.W)))
  val dataInQueue = Queue(io.dataIn, wrapFIFODepth)
  //val dataChanQueue = List.fill(rows)(Queue(dataInQueue, chanFIFODepth))
  val weightInQueue = List.tabulate(cols)(col => Queue(weightInQueueInput(col), chanFIFODepth))
  val resultOutQueue = Queue(resultOutQueueInput, wrapFIFODepth)
  val resultChanQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, chanFIFODepth))



  val weightFlow = Vec(cols, RegInit(false.B))
  val dataFlow = Vec(rows, RegInit(false.B))
  val controlBits = Vec(cols, RegInit(0.U(CONTROL_WIDTH)))
  val firstFire = RegInit(false.B)
  val resultAllReady = Cat(PEA.io.ioArray.map(_.out.result.ready)).andR()
  val weightAllValid = Cat(PEA.io.ioArray.map(_.in.weight.valid)).andR()

  val flowCounter = RegInit("b100".U(5.W))

  def dataChannelEnq(cond: Bool) = {
    when(cond) {
      dataFlow.foreach(_ := true.B)
      weightFlow.foreach(_ := true.B)
      flowCounter := Mux(flowCounter === (cols - 1).U, 0.U, flowCounter + 1.U)
      for(col <- 0 until cols) {
        controlBits(col)(CALCULATE) := true.B
        controlBits(col)(CLEAR_DATA) := Mux(col.U === flowCounter, true.B, false.B)
        controlBits(col)(OUTPUT_SUM) := Mux(col.U === flowCounter & !firstFire, true.B, false.B)
      }
      firstFire := false.B
    } .otherwise {
      controlBits.foreach(_ := 0.U)
      dataFlow.foreach(_ := false.B)
      weightFlow.foreach(_ := false.B)
    }
    cond
  }

  // Todo: Merge the flush into the state machine
  when(state === IDLE.U) {
    when(io.weightUpdate) {
      state := WEIGHT_REFRESH.U
      flowCounter := 0.U
    }
    controlBits.foreach(_ := "b100".U)
  } .elsewhen(state === WEIGHT_REFRESH.U) {
    // Refresh the weight in the array
    when(flowCounter === (cols - 1).U) {
      state := DATA_FLOW.U
      flowCounter := 0.U
      firstFire := true.B
    } .elsewhen(Cat(weightInQueue.map(_.valid)).orR()) {
      flowCounter := flowCounter + 1.U
    }

    when(Cat(weightInQueue.map(_.valid)).orR()) {
      // All weights are ready to fire.
      for(col <- 0 until cols)
        when(col.U >= flowCounter) {
          PEA.io.ioArray(col).out.weight.ready := true.B
        }
    } .otherwise {
      weightFlow.foreach(_ := false.B)
    }
    controlBits.foreach(_ := "b001".U)
  } .elsewhen(state === DATA_FLOW.U) {
    when(io.weightUpdate | io.clearData) {
      state := CLEAR_DATA.U
      weightFlow.foreach(_ := false.B)
      dataFlow.foreach(_ := false.B)
      controlBits.foreach(_ := "b001".U)
      firstFire := false.B
    } .otherwise {
      dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
    }
  } .elsewhen(state === CLEAR_DATA.U) {
    when(!firstFire) {
      dataChannelEnq(cond = dataInQueue.valid & resultAllReady)
      when(!dataInQueue.valid) {
        firstFire := false.B
      }
    } .elsewhen(flowCounter === 0.U) {
      state := IDLE.U
      dataFlow.foreach(_ := false.B)
      weightFlow.foreach(_ := false.B)
      controlBits.foreach(_ := "b001".U)
    } .otherwise {
      // Push 0 into the dataChannel
      dataChannelEnq(cond = resultAllReady)
    }
  }

  // Link the weight channel
  for(col <- 0 until cols) {
    PEA.io.ioArray(col) <> weightInQueue(col)
    PEA.io.ioArray(col).out.weight.ready := weightFlow(col)
    PEA.io.ioArray(col).in.control.bits := controlBits(col)
    PEA.io.ioArray(col).in.control.valid := true.B
  }

  // Weight Repeat
  for(col <- 0 until cols) {
    weightInQueueInput(col).valid := Mux(io.repeat,
      PEA.io.ioArray(col).out.weight.fire(), io.weightIn(col).valid)
    weightInQueueInput(col).bits := Mux(io.repeat,
      PEA.io.ioArray(col).out.weight.bits, io.weightIn(col).bits)
    io.weightIn(col).ready := Mux(io.repeat,
      false.B, weightInQueueInput(col).ready
    )
  }
  for(row <- 0 until rows) {
    PEA.io.ioArray(row).in.data.bits := Mux(state === CLEAR_DATA.U & !firstFire, 0.U, dataInQueue.bits)
    PEA.io.ioArray(row).in.data.valid := Mux(state === CLEAR_DATA.U & !firstFire, true.B, dataInQueue.valid)
    dataInQueue.ready := Mux(state === CLEAR_DATA.U & !firstFire, false.B, PEA.io.ioArray(row).in.data.ready)
    PEA.io.ioArray(row).out.data.ready := dataFlow(row)
  }

}
