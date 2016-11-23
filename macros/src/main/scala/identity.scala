import scala.annotation.MacrosAnnotation

class identity extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn
  }
}
