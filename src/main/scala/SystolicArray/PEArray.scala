package SystolicArray

import chisel3._

class PEArray(rows: Int, cols: Int, dataBits: Int, resultFIFODepth: Int) extends Module {
  private val channelNum = if(rows > cols) rows else cols
  val io = IO(new Bundle{
    val ioArray = Vec(channelNum, new PEBundle(dataBits))
  })
  private val peArray = List.fill(rows, cols)(Module(new PEV2(dataBits, resultFIFODepth)))
  for(row <- 1 until rows) {
    for(col <- 1 until cols){
      peArray(row)(col).io.in.data <> peArray(row)(col - 1).io.out.data
      peArray(row)(col).io.in.result <> peArray(row)(col - 1).io.out.result

      peArray(row)(col).io.in.control <> peArray(row - 1)(col).io.out.control
      peArray(row)(col).io.in.weight <> peArray(row - 1)(col).io.out.weight
    }
  }
  for(row <- peArray.indices) {
    peArray(row).head.io.in.data <> io.ioArray(row).in.data
    peArray(row).head.io.in.result <> io.ioArray(row).in.result
    io.ioArray(row).out.data <> peArray(row).last.io.out.data
    io.ioArray(row).out.result <> peArray(row).last.io.out.result
  }
  for(col <- 1 until cols) {
    peArray.head(col).io.in.data <> peArray.head(col - 1).io.out.data
    peArray.head(col).io.in.result <> peArray.head(col - 1).io.out.result
  }
  // Weight--Vertical
  for(col <- 0 until cols) {
    peArray.head(col).io.in.weight <> io.ioArray(col).in.weight
    peArray.head(col).io.in.control <> io.ioArray(col).in.control
    io.ioArray(col).out.weight <> peArray.last(col).io.out.weight
    io.ioArray(col).out.control <> peArray.last(col).io.out.control
  }
  for(row <- 1 until cols) {
    peArray(row).head.io.in.weight <> peArray(row - 1).head.io.out.weight
    peArray(row).head.io.in.control <> peArray(row - 1).head.io.out.control
  }
  if(rows >= cols) {
    for(chan <- cols until rows) {
      io.ioArray(chan).out.weight.noenq()
      io.ioArray(chan).out.control.noenq()
    }
  } else {
    for(chan <- rows until cols) {
      io.ioArray(chan).out.data.noenq()
      io.ioArray(chan).out.result.noenq()
    }
  }
}
