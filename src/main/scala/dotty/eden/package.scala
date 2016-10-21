package dotty

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context

package object eden {
  // outer context of an AST
  sealed trait Loc
  case object ExprLoc extends Loc
  case object PatLoc extends Loc
  case object SuperCallLoc extends Loc
  case object ParamLoc extends Loc
  case object ImportLoc extends Loc

  // mode of current AST
  sealed trait Mode
  case object TermMode extends Mode
  case object TypeMode extends Mode

  implicit def toMetaUntyped(tree: untpd.Tree)(implicit ctx: Context): m.Tree = {
    new UntpdConvert(TermMode, ExprLoc).toMTree(tree)
  }

  implicit def toMetaTyped(tree: tpd.Tree)(implicit ctx: Context): m.Tree = ???
}

