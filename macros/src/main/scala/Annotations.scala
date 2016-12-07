import scala.`meta`._

class main extends scala.annotation.MacrosAnnotation {
  def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def stub(args: Any): Any = { ..$stats }
    """
    q"object $name { $main }"
  }
}

