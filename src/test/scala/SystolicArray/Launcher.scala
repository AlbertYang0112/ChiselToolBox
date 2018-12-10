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
    }
  )
  def main(args: Array[String]): Unit = {
    CircuitRunner(circuits, args)
  }
}
