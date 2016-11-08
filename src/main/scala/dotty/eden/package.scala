package dotty

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.parsing.Parsers.Parser

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

  // meta placeholder
  // def meta(arg: Any): Stat = ???

  def expand(module: AnyRef, impl: java.lang.reflect.Method, args: List[untpd.Tree], ctx: Context): untpd.Tree = {
    val margs = args.map(arg => if (arg != null) toMetaUntyped(arg)(ctx) else null)
    val metaResult = impl.invoke(module, margs.asInstanceOf[List[AnyRef]].toArray: _*).asInstanceOf[m.Tree]

    val parser = new Parser(new SourceFile("<meta>", metaResult.syntax.toArray))(ctx)
    val (_, stats) = parser.templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }
}

