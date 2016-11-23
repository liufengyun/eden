import scala.annotation.MacrosAnnotation
import scala.`meta`._

class appendA extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    AppendHelpers.appendStat(defn, "letters += 'a'")
  }
}

class appendB extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    AppendHelpers.appendStat(defn, "letters += 'b'")

  }
}

class appendC extends MacrosAnnotation {
  inline def apply(defn: Any): Any = meta {
    AppendHelpers.appendStat(defn, "letters += 'c'")
  }
}

object AppendHelpers {
  def appendStat(defn: Tree, stat: String) = {
    val parsedStat = stat.parse[Stat].get

    defn match {
      case q"""..$mods def $name[..$tparams](...$paramss): $tpeopt = {
                 ..${ stats: scala.collection.immutable.Seq[Stat] }
               }""" =>
        q"..$mods def $name[..$tparams](...$paramss): $tpeopt = { ..$stats;  $parsedStat}"
      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
        q"..$mods def $name[..$tparams](...$paramss): $tpeopt = { $expr; $parsedStat  }"
    }

  }
}

package placebo {
  class appendA extends MacrosAnnotation
}
