import language.experimental.macros
import reflect.macros.blackbox._

class LoggerMacroImpl(val c: Context) {
  import c.universe._

  def log(x: c.Tree) : c.Tree = {
    q"""
     if (LoggerMacro.loggerOn) {
       println("INFO: " + $x)
     }
     """
  }

  def warn(x: c.Tree) : c.Tree = {
    q"""
     if (LoggerMacro.loggerOn) {
       println("WARN: " + $x)
     }
     """
  }
}

object LoggerMacro {
  var loggerOn = true
  def setLoggerState(b: Boolean) = loggerOn = b;

  def log(x : String) : Unit = macro LoggerMacroImpl.log
  def warn(x : String) : Unit = macro LoggerMacroImpl.warn
}
