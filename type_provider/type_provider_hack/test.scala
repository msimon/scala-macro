// import scala.language.reflectiveCalls
import scala.language.experimental.macros

object Test extends App {
  import Macros._

  val db = h2db("db:user")

  db.add("av", 13)
  db.users.foreach({a => println(a)})
  println("v = " + db.v)
}
