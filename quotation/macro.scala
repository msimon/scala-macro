import language.experimental.macros
import reflect.macros.whitebox._

object debug {
  def apply[T](x: => T) : Any = macro implPrintThenExecute

  def implPrintThenExecute(c: Context)(x: c.Tree) : c.Tree = {
    import c.universe._

    val q"..$stats" = x
    val (msgs,execs) = stats.foldLeft (List[Tree](),List[Tree]()) {
      case ((msgs, execs), stat_) => {

        val (stat,msg) = stat_ match {
          case Apply( // for lifting
            Select(Apply(
              Select(Ident(TermName("Qhelper")), TermName("QHelper")),
              List(Apply(Select(Select(Ident(TermName("scala")), TermName("StringContext")), TermName("apply")), lconstant))
            ), TermName("q_")), lparam) =>
            val stat = Apply(Select(
              Apply(Select(New(Ident(TypeName("StringContext"))), termNames.CONSTRUCTOR), lconstant),
              TermName("q")), lparam);

            def concatMsg(lcons : List[Tree], lpar: List[Tree], acc: String= "") : String = {
              (lcons.headOption, lpar.headOption) match {
                case (Some(Literal(Constant(cons))), Some(Ident(TermName(par)))) => concatMsg(lcons.tail, lpar.tail, acc + cons + "$" + par)
                case (Some(cons), Some(par)) => concatMsg(lcons.tail, lpar.tail, acc + cons + "${" + par + "}")
                case (Some(Literal(Constant(cons))), None) => acc + cons
                case _ => ""
              }
            }

            val preMsg = concatMsg(lconstant, lparam);

            val msg = "q\"\"\"%s\"\"\"".format(preMsg);

            (stat,msg)
          case _ => (stat_,showCode(stat_))
        }
        ((q"println($msg)":: msgs), (stat :: execs))
      }
    }
    q"""println("Wrote:")
        val msgs = { ..${msgs.reverse} };
        val res = { ..${execs.reverse} } ;
        println("Got:\n%s".format(showCode(res)))
        res
     """
  }
}
