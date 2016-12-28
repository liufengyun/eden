package dotty.eden

package object convert {
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
}
