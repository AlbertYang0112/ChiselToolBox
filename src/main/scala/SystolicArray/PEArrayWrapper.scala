package SystolicArray
import chisel3._
import chisel3.util.{Cat, EnqIO, Queue}

class PEArrayWrapper(val rows: Int,
                     val cols: Int,
                     val dataBits: Int,
                     val resultFIFODepth: Int,
                     val wrapFIFODepth: Int
                     ) extends Module{
  private val channelNum = if(rows > cols) rows else cols
  private val PEA = Module(new PEArray(rows, cols, dataBits, resultFIFODepth))

  val io = IO(new Bundle{
    val ioArray       = Vec(channelNum, new PEBundle(dataBits))
    val fifoReset     = Input(Bool())
    val fifoResetting = Output(Bool())
    val dataInFull    = Output(UInt(rows.W))
    val dataInEmpty   = Output(UInt(rows.W))
    val weightInFull  = Output(UInt(cols.W))
    val weightInEmpty = Output(UInt(cols.W))
    val resultFull    = Output(UInt(rows.W))
    val resultEmpty   = Output(UInt(rows.W))
    val repeatWeight  = Input(Bool())
    val stall         = Input(Bool())
    val kernelSize    = Input(UInt(8.W))
  })

  val arrayResetRowCounter = RegInit(rows.U)
  val arrayResetColCounter = RegInit(cols.U)
  val fifoResetting = RegInit(false.B)


  val repeat = io.repeatWeight & (!fifoResetting)
  val weightInQueueInput = List.fill(cols)(Wire(EnqIO(UInt(dataBits.W))))
  val kernelSize = RegInit(0.U(8.W))


  private val dataInQueue = List.tabulate(rows)(row => Queue(io.ioArray(row).in.data, wrapFIFODepth))
  //private val weightInQueue = List.tabulate(cols)(col => Queue(io.ioArray(col).in.weight, wrapFIFODepth))
  private val weightInQueue = List.tabulate(cols)(col => Queue(weightInQueueInput(col), wrapFIFODepth))
  private val resultOutQueue = List.tabulate(rows)(row => Queue(PEA.io.ioArray(row).out.result, wrapFIFODepth))

  val fifoResetPrev = RegNext(io.fifoReset)
  val fifoResetReq = !fifoResetPrev & io.fifoReset
  io.fifoResetting := fifoResetting
  when(fifoResetting) {
    when(Cat(dataInQueue.map(_.valid)).orR() |
      Cat(weightInQueue.map(_.valid)).orR() |
      Cat(resultOutQueue.map(_.valid)).orR() |
      arrayResetRowCounter =/= 0.U | arrayResetColCounter =/= 0.U
    ) {
      fifoResetting := true.B
    } .otherwise {
      fifoResetting := false.B
    }
    kernelSize := io.kernelSize
  }
  when(fifoResetReq) {
    arrayResetColCounter := cols.U
    arrayResetRowCounter := rows.U
    fifoResetting := true.B
  }
  when(fifoResetting) {
    when(!Cat(weightInQueue.map(_.valid)).orR()) {
      arrayResetColCounter := Mux(arrayResetColCounter > 0.U, arrayResetColCounter - 1.U, 0.U)
    }
    when(!Cat(dataInQueue.map(_.valid)).orR()) {
      arrayResetRowCounter := Mux(arrayResetRowCounter > 0.U, arrayResetRowCounter - 1.U, 0.U)
    }
  }

  for(col <- 0 until cols) {
    weightInQueueInput(col).valid := Mux(repeat,
      weightInQueue(col).valid & weightInQueue(col).ready, io.ioArray(col).in.weight.valid)
    weightInQueueInput(col).bits := Mux(repeat,
      weightInQueue(col).bits, io.ioArray(col).in.weight.bits)
    //weightInQueueInput(col).ready := Mux(io.repeatWeight,
    //  weightInQueue(col).ready, io.ioArray(col).in.weight.ready)
    io.ioArray(col).in.weight.ready := Mux(repeat,
      false.B, weightInQueueInput(col).ready)
  }


  val counter = RegInit(0.U(4.W))
  val counterLast = RegNext(counter)

  val fire = !io.stall & Cat(PEA.io.ioArray.map{peIO => peIO.in.data.valid & peIO.in.weight.valid & peIO.in.control.valid}).andR()
  val refreshSpike = counter === 0.U && fire

  val PEAValid = Wire(Bool())
  PEAValid := Cat(PEA.io.ioArray.map{
    peIO => peIO.out.data.valid &
      peIO.out.weight.valid &
      peIO.out.control.valid}).andR()

  when(fifoResetting) {
    counter := 0.U
  } .elsewhen(fire) {
    when(counter < kernelSize - 1.U) {
      counter := counter + 1.U
    } .otherwise {
      counter := 0.U
    }
  }

  // Connection for data channels
  for (row <- dataInQueue.indices) {
    PEA.io.ioArray(row).in.data <> dataInQueue(row)
    PEA.io.ioArray(row).out.data.ready := fire | fifoResetting
    io.ioArray(row).out.data.bits := 0.U(dataBits.W)
    io.ioArray(row).out.data.valid := false.B
  }

  // Connection for weight channels
  for (col <- weightInQueue.indices) {
    PEA.io.ioArray(col).in.weight <> weightInQueue(col)
    PEA.io.ioArray(col).out.weight.ready := fire | fifoResetting
    io.ioArray(col).out.weight.bits := 0.U(dataBits.W)
    io.ioArray(col).out.weight.valid := false.B
  }

  // Connection for result channels
  for (row <- resultOutQueue.indices) {
    io.ioArray(row).out.result.valid := resultOutQueue(row).valid & !fifoResetting
    io.ioArray(row).out.result.bits := resultOutQueue(row).bits
    resultOutQueue(row).ready := io.ioArray(row).out.result.ready | fifoResetting

    io.ioArray(row).in.result.ready := true.B
    PEA.io.ioArray(row).in.result.valid := false.B
    PEA.io.ioArray(row).in.result.bits := 0.U
  }

  // Connection for control channels
  for (col <- 0 until cols) {
    PEA.io.ioArray(col).out.control.ready := PEAValid
    PEA.io.ioArray(col).in.control.valid := dataInQueue.head.valid & weightInQueue.head.valid
    PEA.io.ioArray(col).in.control.bits := refreshSpike & (col.U >= (cols.U - kernelSize))
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

  // Unused data and result channel
  for (row <- rows until channelNum) {
    io.ioArray(row).out.data.noenq()
    io.ioArray(row).out.result.noenq()
    io.ioArray(row).in.data.nodeq()
    io.ioArray(row).in.result.nodeq()
    PEA.io.ioArray(row).in.data.enq(0.U)
    PEA.io.ioArray(row).in.result.enq(0.U)
    PEA.io.ioArray(row).out.data.deq()
    PEA.io.ioArray(row).out.result.deq()
  }

  // Unused weight and control channel
  for (col <- cols until channelNum) {
    io.ioArray(col).out.weight.noenq()
    io.ioArray(col).out.control.noenq()
    io.ioArray(col).in.weight.nodeq()
    io.ioArray(col).in.control.nodeq()
    PEA.io.ioArray(col).in.weight.enq(0.U)
    PEA.io.ioArray(col).in.control.enq(false.B)
    PEA.io.ioArray(col).out.weight.deq()
    PEA.io.ioArray(col).out.control.deq()
  }
}
