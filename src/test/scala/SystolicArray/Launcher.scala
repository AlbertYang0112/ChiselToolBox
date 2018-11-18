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
    }
  )
  def main(args: Array[String]): Unit = {
    CircuitRunner(circuits, args)
  }
}
