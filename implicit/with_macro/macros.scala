import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait Showable[T] { def show(x: T): String }
object Showable {
  implicit def materializeShowable[T]: Showable[T] = macro Macros.impl[T]
}

object Macros {
  def impl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    import definitions._

    val tpe = weakTypeOf[T]
    val sym = tpe.typeSymbol

    def simpleType = {
      q"""
        new Showable[$tpe] {
          def prim = "";
          def show(x: $tpe) = x.toString
        }
      """
    }

    def listType(subTpe : Type) = {
      q"""
        new Showable[$tpe] {
          def list = "";
          def show(x: $tpe) = ${tpe.toString} + "(" + x.map({ f => implicitly[Showable[$subTpe]].show(f) }).mkString(",") + ")"
        }
      """
    }

    (sym.asClass.isPrimitive, tpe) match {
      case (true, _) => simpleType
      case (false, _) if tpe =:= typeOf[String] => simpleType
      case (false, TypeRef(_,_,t)) if (
        (tpe.typeConstructor =:= typeOf[List[_]].typeConstructor) ||
          (tpe.typeConstructor =:= typeOf[Array[_]].typeConstructor)
      ) => listType(t.head)
      case _ => {
        val l : c.Tree = tpe.decls.sorted.foldLeft(EmptyTree:c.Tree) ({
          case (acc, f)  =>
            val opt = f match {
              case f2 : TermSymbol if (f2.isVal || f2.isVar) && f2.getter.isPublic => Some(f2.getter)
              case f2 : TermSymbol => None
              case f2  => None
            };

            opt.fold(acc) {
              t =>
              if (acc == EmptyTree) {
                q"""${f.fullName} + " => " + implicitly[Showable[${f.info}]].show(x.${t})"""
              } else {
                q"""$acc + " | " + ${f.fullName} + " => " + implicitly[Showable[${f.info}]].show(x.${t})"""
              }
            }
        })

        q"""
          new Showable[$tpe] {
            def cla = "";
            def show(x: $tpe) = ${tpe.toString.replaceAll("\\.", "_")} + "(" + $l + ")"
          }
        """
      }
    }
  }
}
