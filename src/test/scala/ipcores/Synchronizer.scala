package ipcores

import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}
import chisel3._
import scala.collection.mutable.Queue
import scala.util.Random

class SyncFromLowFreqTests(c: SyncFromLowFreq) extends PeekPokeTester(c) {
  var clockA = false
  var clockB = false
  val TOTAL_CYCLES = 10000
  var cycle_counter = 0
  val signalList = new Queue[Int]
  var state = 0
  for (cycle_counter <- 0 until TOTAL_CYCLES) {
    println(cycle_counter + " SignalList" +  signalList)
    if(cycle_counter % 3 == 0) {
      if(!clockA) {
        // Sample the synchronous signal.
        if(signalList.nonEmpty) {
          if(state != 2)
            state += 1
          else if(state >= 2) {
            var i = peek(c.io.dst)
            var j = signalList.dequeue()
            expect(c.io.dst, j)
            println("Expect:" + j +" Got: " + i)
            state = 0
          }
        }
      }
      clockA = !clockA
      poke(c.io.highFreqDstClock, clockA)
    }
    if(cycle_counter % 30 == 0) {
      if(!clockB) {
        // Update the asynchronous signal.
        var sig = Random.nextInt(0xF)
        poke(c.io.src, sig)
        signalList += sig
      }
      clockB = !clockB
    }
    step(1)
  }
}

class SyncFromLowFreqTester extends ChiselFlatSpec {
  behavior of "SyncFromLowFreq"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new SyncFromLowFreq(level = 3, width = 4), backend
      )(c => new SyncFromLowFreqTests(c)) should be (true)
    }
  }
}

class SyncFromHighFreqTests(c: SyncFromHighFreq) extends PeekPokeTester(c) {
  var clockA = false
  var clockB = false
  val TOTAL_CYCLES = 10000
  var cycle_counter = 0
  val signalList = new Queue[Int]
  var state = 0
  var sendSignal = 0
  signalList += 0
  for(cycle_counter <- 0 to TOTAL_CYCLES) {
    if(cycle_counter % 100 == 0) {
      if(!clockA) {
        if (state == 1) state = 2
        //else if (state == 2) state = 3
        else if (state == 2) {
          var getSig = peek(c.io.syncOut)
          var expectSig = signalList.dequeue()
          println("At" + cycle_counter + " Get:" + getSig)
          expect(getSig == expectSig,
            msg=s"Expect: $expectSig Get: $getSig")
          state = 0
        }
      }
      clockA = !clockA
      poke(c.io.lowFreqDstClock, clockA)
    }
    if(cycle_counter % 3 == 0) {
      if(!clockB) {
        if(state == 0) {
          sendSignal = Random.nextInt(0xF)
          println("At" + cycle_counter + " Set:" + sendSignal)
          poke(c.io.asyncIn, sendSignal)
          signalList += sendSignal
          state = 1
        }
      }
      clockB = !clockB
    }
    step(1)
  }
}

class SyncFromHighFreqTester extends ChiselFlatSpec {
  behavior of "SyncFromHighFreq"
  backends foreach { backend =>
    it should s"Sync Signal $backend" in {
      Driver(
        () => new SyncFromHighFreq(level = 3, width = 4), backend
      )(c => new SyncFromHighFreqTests(c)) should be (true)
    }
  }
}
