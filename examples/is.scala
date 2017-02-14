import scala.meta._
import scala.meta.dialects.Dotty

object scope {
  inline def is[T](a: Any): Any = meta {
    q"$a.isInstanceOf[$T]"
  }

  inline def both[S, T](a: Any): Any = meta {
    q"$a.isInstanceOf[$S] && $a.isInstanceOf[$T]"
  }
}
