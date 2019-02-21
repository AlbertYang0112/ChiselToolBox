package SystolicArray

import chisel3._
import chisel3.util.Counter

class PEArrayV2(
                 rows: Int,
                 cols: Int,
                 dataWidth: Int,
                 weightWidth: Int,
                 resultFIFODepth: Int
               ) extends Module {
  private val channelNum = if(rows > cols) rows else cols
  private val resultWidth = dataWidth + weightWidth
  val io = IO(new Bundle{
    val ioArray = Vec(channelNum, new PEBundleV2(dataWidth, weightWidth))
  })
  private val peArray = List.fill(rows, cols)(Module(new PEV3(
    dataWidth = dataWidth,
    weightWidth = weightWidth,
    resultBufferDepth = resultFIFODepth,
    bufferedControl = true
  )))
  for(row <- 1 until rows; col <- 1 until cols) {
    peArray(row)(col).io.in.data <> peArray(row)(col - 1).io.out.data
    peArray(row)(col).io.in.control <> peArray(row)(col - 1).io.out.control

    peArray(row)(col).io.in.result <> peArray(row - 1)(col).io.out.result
    peArray(row)(col).io.in.weight <> peArray(row - 1)(col).io.out.weight
    peArray(row)(col).io.in.colActivate := peArray(row - 1)(col).io.out.colActivate
  }
  for(row <- 0 until rows) {
    peArray(row).head.io.in.data <> io.ioArray(row).in.data
    peArray(row).head.io.in.control <> io.ioArray(row).in.control
    io.ioArray(row).out.data <> peArray(row).last.io.out.data
    io.ioArray(row).out.control <> peArray(row).last.io.out.control
  }
  for(col <- 1 until cols) {
    peArray.head(col).io.in.data <> peArray.head(col - 1).io.out.data
    peArray.head(col).io.in.control <> peArray.head(col - 1).io.out.control
  }
  // Weight--Vertical
  for(col <- 0 until cols) {
    peArray.head(col).io.in.weight <> io.ioArray(col).in.weight
    peArray.head(col).io.in.result <> io.ioArray(col).in.result
    io.ioArray(col).out.weight <> peArray.last(col).io.out.weight
    io.ioArray(col).out.result <> peArray.last(col).io.out.result
    peArray.head(col).io.in.colActivate := io.ioArray(col).in.colActivate
    io.ioArray(col).out.colActivate := peArray.last(col).io.out.colActivate
  }
  for(row <- 1 until rows) {
    peArray(row).head.io.in.weight <> peArray(row - 1).head.io.out.weight
    peArray(row).head.io.in.result <> peArray(row - 1).head.io.out.result
    peArray(row).head.io.in.colActivate := peArray(row - 1).head.io.out.colActivate
  }

  for(chan <- cols until channelNum) {
    io.ioArray(chan).in.control.nodeq()
    io.ioArray(chan).in.weight.nodeq()
    io.ioArray(chan).out.weight.noenq()
    io.ioArray(chan).out.control.noenq()
  }
  for(chan <- rows until channelNum) {
    io.ioArray(chan).in.data.nodeq()
    io.ioArray(chan).in.result.nodeq()
    io.ioArray(chan).out.data.noenq()
    io.ioArray(chan).out.result.noenq()
  }
}
