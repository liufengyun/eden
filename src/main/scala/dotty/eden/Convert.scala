package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts.Context
import dotty.eden.{UntpdMapping => u}
import dotty.tools.dotc.core.StdNames._


// outer context of an AST
sealed trait Loc
case object PatLoc extends Loc
case object TypeLoc extends Loc
case object TermLoc extends Loc
case object ParamLoc extends Loc
case object SelfLoc extends Loc

object Convert {
  def toMTreeUntpd[T <: m.Tree](tree: untpd.Tree)(implicit ctx: Context): T = {
    new UntpdConvert(TermLoc).toMTree(tree)
  }

  def toMTreeTpd(tree: tpd.Tree): m.Tree = ???
}

