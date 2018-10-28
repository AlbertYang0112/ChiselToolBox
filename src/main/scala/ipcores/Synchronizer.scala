package ipcores

import chisel3._
import chisel3.core.withClock

class SyncFromLowFreq(level: Int, width: Int) extends Module {
  val io = IO(new Bundle{
    val asyncIn = Input(UInt(width.W))
    val syncOut = Output(UInt(width.W))
  })
  val flipflopVec = Vec(RegInit(0.U(width.W)))
  var i:Int = 0
  flipflopVec(0) := io.asyncIn
  for(i <- 0 to level - 2) {
    flipflopVec(i + 1) := flipflopVec(i)
  }
  io.syncOut := flipflopVec(level - 1)
}

class SyncFromHighFreq(level: Int, width: Int) extends Module {
  val io = IO(new Bundle{
    val asyncIn = Input(UInt(width.W))
    val syncOut = Output(UInt(width.W))
  })
  // Todo: Finish this synchronizer.
  var i:Int = 0
  val clear = ~io.asyncIn & io.syncOut
  for(i <- 0 to width) {
    withClock(io.asyncIn(i).asClock()) {
      when(clear(i)){
      }
    }
  }
}


