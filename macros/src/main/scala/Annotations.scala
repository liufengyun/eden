import scala.meta._
import scala.meta.dialects.Dotty

class main extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta.apply {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def stub(args: Any): Any = { ..$stats }
    """
    q"object $name { $main }"
  }
}

class data extends scala.annotation.StaticAnnotation  {
  inline def apply(defn: Any): Any = meta.apply {
    defn match {
      case q"class $name(..$params)" =>
        val params1 = params.map(_.copy(mods = List(mod"valparam")))
        val class1 = q"class $name(..$params1)"

        val applyParams = params.map(_.copy(mods = Nil))
        val applyArgs = params.map(p => Term.Name(p.name.syntax))
        val apply = q"def apply(..$applyParams) = new ${Ctor.Name(name.syntax)}(..$applyArgs)"
        val object1 = q"object ${Term.Name(name.syntax)} { $apply }"

        q"$class1; $object1"
      case _ =>
        abort("@data can only annotate classes")
    }
  }
}

