import scala.annotation.MacrosAnnotation
import scala.`meta`._

class helloWorld extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" = defn
    q"""..$mods def $name[..$tparams](...$paramss): $tpeopt = "hello world""""
  }
}
