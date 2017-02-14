import scala.annotation.StaticAnnotation
import scala.meta._
import scala.meta.dialects.Dotty

import scala.collection.immutable.Seq

class main extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def main(args: Array[String]): Unit = { ..$stats }
    """
    q"object $name { $main }"
  }
}