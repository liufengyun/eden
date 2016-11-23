import scala.annotation.MacrosAnnotation
import scala.`meta`._

class param(some: String) extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"new $_($string)" = this
    defn
  }
}
