package dotty

import scala.{meta => m}
import dotty.tools.dotc._
import ast.Trees._
import ast.{tpd, untpd}
import ast.untpd.modsDeco
import core.Contexts.Context
import util.SourceFile
import parsing.Parsers.Parser
import core.Symbols._
import core.SymDenotations._
import core.Decorators

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

  /** Expand annotation macros */
  def expandAnnot(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.Tree = {
    val ann = mdef.mods.annotations.filter(isAnnotMacros).headOption
    val expansion = ann.flatMap {
      case ann@Apply(Select(New(tpt), init), Nil) => // TODO: support constant params
        val classpath = ctx.platform.classPath.asURLs.toArray
        val classloader = new java.net.URLClassLoader(classpath, getClass.getClassLoader)
        // reflect macros definition
        val moduleClass = classloader.loadClass(tpt.show + "$inline$") // TODO: fully-qualified name
        val module = moduleClass.getField("MODULE$").get(null)
        val impl = moduleClass.getDeclaredMethods().find(_.getName == "apply").get
        impl.setAccessible(true)
        val callback = (margs: List[AnyRef]) => impl.invoke(module, margs: _*)

        val prefix = null
        val expandee = {
          val mods1 = mdef.mods.withAnnotations(mdef.mods.annotations.filter(_ ne ann))
          mdef.withMods(mods1)
        }
        val mtree = toMetaUntyped(expandee)
        val mResult = impl.invoke(module, prefix, mtree).asInstanceOf[m.Tree]
        val result = parse(mResult.syntax)(ctx)
        Some(result)
      case _ =>
        None
    }

    expansion.getOrElse(mdef)
  }

  def parse(code: String)(implicit ctx: Context): untpd.Tree = {
    val parser = new Parser(new SourceFile("<meta>", code.toArray))
    val (_, stats) = parser.templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  def isQuasiquote(symbol: Symbol)(implicit ctx: Context): Boolean = {
    symbol.enclosingPackageClass.showFullName == "scala.meta.quasiquotes"
  }

  /** An annotation is macros iff it extends `scala.annotation.MacrosAnnotation` */
  def isAnnotMacros(ann: untpd.Tree)(implicit ctx: Context): Boolean = {
    import Decorators._
    val symbol = ctx.typer.typedAheadAnnotation(ann)
    if (!symbol.exists) return false

    val macrosAnnotType = ctx.requiredClassRef("scala.annotation.MacrosAnnotation")
    symbol.typeRef <:< macrosAnnotType
  }
}

