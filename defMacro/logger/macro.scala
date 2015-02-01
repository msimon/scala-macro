import language.experimental.macros
import reflect.macros.whitebox._

object Logger {
  var loggerOn = true
  def setLoggerState(b: Boolean) = loggerOn = b;

  def log(x : String) : Unit = {
    if (loggerOn)
      println("Loggin: " + x)
  }
}


object LoggerMacro {
  var loggerOn = true
  def setLoggerState(b: Boolean) = loggerOn = b;

  def log(x : String) : Unit = macro logImpl
  def logImpl(c: Context)(x: c.Tree) : c.Tree = {
    import c.universe._

    q"""
     if (LoggerMacro.loggerOn) {
       println("Loggin: " + $x)
     }
     """
  }
}
