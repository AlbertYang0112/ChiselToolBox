package utils

import scala.collection.mutable.ArrayBuffer
import chisel3.iotesters._

object CircuitRunner {
  def apply(circuitMap: Map[String, TesterOptionsManager => Boolean], args: Array[String]): Unit = {
    var successful = 0
    val errors = new ArrayBuffer[String]

    val optionsManager = new TesterOptionsManager()
    optionsManager.doNotExitOnHelp()

    optionsManager.parse(args)

    val programArgs = optionsManager.commonOptions.programArgs

    if(programArgs.isEmpty) {
      println("Available circuits")
      for(x <- circuitMap.keys) {
        println(x)
      }
      println("all")
      System.exit(0)
    }

    val problemsToRun = if(programArgs.exists(x => x.toLowerCase() == "all")) {
      circuitMap.keys
    }
    else {
      programArgs
    }

    for(testName <- problemsToRun) {
      circuitMap.get(testName) match {
        case Some(test) =>
          println(s"Starting circuit $testName")
          try {
            optionsManager.setTopName(testName)
            optionsManager.setTargetDirName(s"test_run_dir/$testName")
            if(test(optionsManager)) {
              successful += 1
            }
            else {
              errors += s"Circuit $testName: test error occurred"
            }
          }
          catch {
            case exception: Exception =>
              exception.printStackTrace()
              errors += s"Circuit $testName: exception ${exception.getMessage}"
            case t : Throwable =>
              errors += s"Circuit $testName: throwable ${t.getMessage}"
          }
        case _ =>
          errors += s"Bad circuit name: $testName"
      }

    }
    if(successful > 0) {
      println(s"Circuits passing: $successful")
    }
    if(errors.nonEmpty) {
      println("=" * 80)
      println(s"Errors: ${errors.length}: in the following circuits")
      println(errors.mkString("\n"))
      println("=" * 80)
      System.exit(1)
    }
  }
}
