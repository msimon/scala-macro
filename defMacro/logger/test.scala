object Test extends App {
  Logger.setLoggerState(args(0).toBoolean);
  LoggerMacro.setLoggerState(args(0).toBoolean);

  def hugeComputation() = {
    var c = 0
    var c2 = 0
    var n = 60000;
    for (i <- 0 to n) {
      for (j <- 0 to n) {
        c = c + i
        c2 = c + i
      }
    }
    ""
  }

  Logger.log("My first log");
  Logger.log("My second log" + hugeComputation);

  // LoggerMacro.log("My first log");
  // LoggerMacro.log("My second log" + hugeComputation);
}
