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
    },
    "UARTTransmitter" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new UARTTransmitter(8, ParityBit.OddParityCheck), manager) {
        c => new UARTTransmitterTests(c)
      }
    },
    "UART" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new UART(9600, 10000000), manager) {
        c => new UARTTests(c)
      }
    },
    "ClockDividerThree" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new ClockDividerThree, manager) {
        c => new ClockDividerThreeTests(c)
      }
    },
    "ClockDivider" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new ClockDivider(7), manager) {
        c => new ClockDividerTests(c)
      }
    }
  )
  def main(args: Array[String]): Unit = {
    CircuitRunner(circuits, args)
  }
}
