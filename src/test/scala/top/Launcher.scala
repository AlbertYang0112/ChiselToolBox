package top

import chisel3.iotesters.{Driver, TesterOptionsManager}
import utils.CircuitRunner

object Launcher {
  val circuits = Map(
    "TopExp" -> { manager: TesterOptionsManager =>
      Driver.execute(() => new TopExp, manager) {
        c => new TopExpTests(c)
      }
    }
  )
  def main(args: Array[String]): Unit = {
    CircuitRunner(circuits, args)
  }
}
