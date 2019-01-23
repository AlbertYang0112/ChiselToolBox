package SystolicArray

import AXI4.AXI4Parameter
import chisel3.iotesters.{Driver, TesterOptionsManager}
import utils.CircuitRunner

object Launcher {
  val circuits = Map(
    "ProcessingElement" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new ProcessingElement(8,5), manager) {
        c => new ProcessingElementTests(c)
      }
    },
    "MatMul" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new MatMul(32, 3), manager) {
        c => new MatMulTests(c)
      }
    },
    "PEArray" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new PEArray(3, 3, 8, 2), manager) {
        c => new PEArrayTests(c)
      }
    },
    "PEArrayWrapper" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new PEArrayWrapper(1, 10, 32, 2, 16), manager) {
        c => new PEAWrapperTests(c)
      }
    },
    "PEArrayWrapperV2" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new PEArrayWrapperV2(
        dataWidth = 8,
        weightWidth = 8,
        weightChanNum = 5,
        rows = 5,
        cols = 5,
        PEResultFIFODepth = 16,
        wrapFIFODepth = 16,
        chanFIFODepth = 16
      ), manager) {
        c => new PEAWrapperV2Tests(c)
      }
    }
  )
  def main(args: Array[String]): Unit = {
    CircuitRunner(circuits, args)
  }
}
