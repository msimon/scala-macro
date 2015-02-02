import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class h2db(s:String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) : Any = macro Macro.h2db_impl
}

object Macro {
  def h2db_impl(c: Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val q"""new h2db(${annot: String})""" = c.prefix.tree

    annottees.toList match {
      case q"object $name extends ..$parents { ..$body }" :: Nil =>
        val TermName(nam) = name ;
        val traitName = TypeName(annot.capitalize);
        val valueName = TermName(annot.toLowerCase + "s");


        q"""
            object $name extends ..$parents {
              case class ${traitName}(val name : String, val price : Int);
              def add (name: String, price: Int) = this.${valueName}.add(new ${traitName}(name, price))
              val ${valueName}: scala.collection.mutable.Set[${traitName}] = scala.collection.mutable.Set(new ${traitName}("Robustas", 30))
              ..$body
            }
          """
    }
  }
}
