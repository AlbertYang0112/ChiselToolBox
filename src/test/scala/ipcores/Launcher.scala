package ipcores

import chisel3.iotesters.{Driver, TesterOptionsManager}
import utils.CircuitRunner

object Launcher {
  val circuits = Map(
    "SyncFromLowFreq" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new SyncFromLowFreq(level = 3, width = 4), manager) {
        c => new SyncFromLowFreqTests(c)
      }
    },
    "SyncFromHighFreq" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new SyncFromHighFreq(level = 2, width = 4), manager) {
        c => new SyncFromHighFreqTests(c)
      }
    },
    "UARTReceiver" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new UARTReceiver(8, ParityBit.OddParityCheck), manager) {
        c => new UARTReceiverTests(c)
      }
  }
  )
  def main(args: Array[String]): Unit = {
    CircuitRunner(circuits, args)
  }
}
