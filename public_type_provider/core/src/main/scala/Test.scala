@h2db("coffee") object DbCoffee

object Test extends App {
  DbCoffee.coffees.add(new DbCoffee.Coffee("Arabica", 10))
  DbCoffee.coffees.foreach (println)
}
