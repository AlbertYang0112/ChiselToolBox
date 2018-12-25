package SystolicArray
import chisel3._
import chisel3.util.{Cat, Queue}

trait PEAControlBit {
  val RESET = 0
  val DATA_IN_FULL = 1
  val DATA_IN_EMPTY = 2
  val WEIGHT_IN_FULL = 3
  val WEIGHT_IN_EMPTY = 4
  val RESULT_FULL = 5
  val RESULT_EMPTY = 6
}

class PEArrayWrapper(val rows: Int,
                     val cols: Int,
                     val dataBits: Int,
                     val resultFIFODepth: Int,
                     val wrapFIFODepth: Int) extends Module{
  private val channelNum = if(rows > cols) rows else cols
  private val PEA = Module(new PEArray(rows, cols, dataBits, resultFIFODepth))

  val io = IO(new Bundle{
    val ioArray       = Vec(channelNum, new PEBundle(dataBits))
    val fifoReset     = Input(Bool())
    val dataInFull    = Output(UInt(rows.W))
    val dataInEmpty   = Output(UInt(rows.W))
    val weightInFull  = Output(UInt(cols.W))
    val weightInEmpty = Output(UInt(cols.W))
    val resultFull    = Output(UInt(rows.W))
    val resultEmpty   = Output(UInt(rows.W))
  })

  private val dataInQueue = List.tabulate(rows)(row => Queue(io.ioArray(row).in.data, wrapFIFODepth))
  private val weightInQueue = List.tabulate(cols)(col => Queue(io.ioArray(col).in.weight, wrapFIFODepth))
  private val resultOutQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, wrapFIFODepth))

  val counter = RegInit(0.U(2.W))

  when(io.fifoReset) {
    counter := 0.U
  } .elsewhen(dataInQueue.head.valid & weightInQueue.head.valid) {
    when(counter < 2.U) {
      counter := counter + 1.U
    } .otherwise {
      counter := 0.U
    }
  }

  for (row <- dataInQueue.indices) {
    PEA.io.ioArray(row).in.data <> dataInQueue(row)
    PEA.io.ioArray(row).out.data.ready := true.B
    io.ioArray(row).out.data.bits := 0.U(dataBits.W)
    io.ioArray(row).out.data.valid := false.B
  }
  for (col <- weightInQueue.indices) {
    PEA.io.ioArray(col).in.weight <> weightInQueue(col)
    PEA.io.ioArray(col).out.weight.ready := true.B
    io.ioArray(col).out.weight.bits := 0.U(dataBits.W)
    io.ioArray(col).out.weight.valid := false.B
  }
  for (row <- resultOutQueue.indices) {
    io.ioArray(row).out.result <> resultOutQueue(row)
    io.ioArray(row).in.result.ready := true.B
    PEA.io.ioArray(row).in.result.valid := false.B
    PEA.io.ioArray(row).in.result.bits := 0.U
  }
  for (col <- 0 until cols) {
    PEA.io.ioArray(col).out.control.ready := true.B
    PEA.io.ioArray(col).in.control.valid := dataInQueue.head.valid & weightInQueue.head.valid
    PEA.io.ioArray(col).in.control.bits := counter === 0.U
    io.ioArray(col).out.control.bits := false.B
    io.ioArray(col).out.control.valid := true.B
    io.ioArray(col).in.control.ready := true.B
  }

  io.dataInFull := Cat(io.ioArray.map(!_.in.data.ready).reverse)
  io.dataInEmpty := Cat(dataInQueue.map(!_.valid).reverse)

  io.weightInFull := Cat(io.ioArray.map(!_.in.weight.ready).reverse)
  io.weightInEmpty := Cat(weightInQueue.map(!_.valid).reverse)

  io.resultFull := Cat(PEA.io.ioArray.map(!_.out.result.ready).reverse)
  io.resultEmpty := Cat(resultOutQueue.map(!_.valid).reverse)
}
