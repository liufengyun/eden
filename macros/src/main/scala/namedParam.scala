import scala.annotation.MacrosAnnotation
import scala.`meta`._

class namedParam(some: String) extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"new $_(some = $string)" = this
    defn
  }
}
