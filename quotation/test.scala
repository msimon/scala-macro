import scala.language.experimental.macros
import reflect.runtime.universe._

object Qhelper {
  implicit class QHelper(val sc: StringContext) {
    def q_(args: Any*): Tree = ???
  }
}

object Test extends App {

  import Qhelper._

  def basic = debug {
    q_"""I am {a quasiquote}"""
  }

  def quoting = debug {
    val a = "Hello";
    val b = 10;

    q_"""a says { ${a.toUpperCase} }; b says ($b)"""
  }

  def lifting = debug {
    val b = "hel";
    val l = List(1,2,3);

    q_"""f(..$l, $b)"""
  }

  def lifting2 = debug {
    val l = List(1,2,3);
    val l2 = List("hel", "abc");

    q_"""f(..$l, ..$l2)"""
  }

  def lifting3 = debug {
    val l = List(1,2,3);
    val l2 = List(4, 5);

    val lc = List(l,l2);

    q_"""f(..$lc)"""
  }

  def lifting4 = debug {
    val l = List(1,2,3);
    val l2 = List(4, 5);

    val lc = List(l,l2);

    q_"""f(...$lc)"""
  }

  def unlifting = {
    val q"""f(${a:String},${b : Int})""" = q"""f("toto", 10)"""
    println("1> " + a + " " + b);

    val q"""f($c,$d)""" = q"""f("toto", 10)"""
    println("2> " + c + " " + d);
  }

  def unlifting2 = {
    val q"""f(..${a : List[String]})""" = q"""f("toto","lol")"""
    println(a);

    val q"""f(..$b)""" = q"""f("toto","lol")"""
    println(b);
  }

  def unlifting3 = {
    val q"""f(...${a : List[List[String]]})""" = q"""f("toto","lol")("test")"""
    println(a);

    val q"""f(...$b)""" = q"""f("toto","lol")("test")"""
    println(b);
  }

  args(0).toInt match {
    case 1 => basic
    case 2 => quoting
    case 3 => lifting
    case 4 => lifting2
    case 5 => lifting3
    case 6 => lifting4
    case 7 => unlifting
    case 8 => unlifting2
    case 9 => unlifting3
  }
}
