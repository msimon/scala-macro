object Test extends App {
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

  LoggerMacro.log("My first log");
  LoggerMacro.warn("My second log" + hugeComputation);
}
