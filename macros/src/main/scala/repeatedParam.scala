import scala.annotation.MacrosAnnotation

class repeatedParam(foos: Any*) extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn
  }
}
