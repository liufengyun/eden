package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts.Context
import dotty.eden.{UntpdMapping => u}
import dotty.tools.dotc.core.StdNames._


// outer context of an AST
sealed trait Loc
case object ExprLoc extends Loc
case object PatLoc extends Loc
case object SuperCallLoc extends Loc
case object ParamLoc extends Loc
case object ImportLoc extends Loc

sealed trait Mode
case object TermMode extends Mode
case object TypeMode extends Mode

object Convert {
  def toMTreeUntpd[T <: m.Tree](tree: untpd.Tree)(implicit ctx: Context): T = {
    new UntpdConvert(TermMode, ExprLoc).toMTree(tree)
  }

  def toMTreeTpd(tree: tpd.Tree): m.Tree = ???
}
