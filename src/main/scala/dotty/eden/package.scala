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
import core.{Decorators, StdNames}

package object eden {
  // TODO: move to internal
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

  implicit def toMeta(tree: untpd.Tree, isTerm: Boolean = true)(implicit ctx: Context): m.Tree = {
    new UntpdConvert(if (isTerm) TermMode else TypeMode, ExprLoc).toMTree(tree)
  }

  implicit def toMeta(tree: untpd.Tree)(implicit ctx: Context): m.Tree = toMeta(tree, isTerm = true)

  private var _classloader: ClassLoader = null
  private var _validForRun: Int = -1
  private def classloader(implicit ctx: Context): ClassLoader =
    if (_classloader != null && ctx.runId == _validForRun) _classloader
    else {
      val classpath = ctx.platform.classPath.asURLs.toArray
      _classloader = new java.net.URLClassLoader(classpath, getClass.getClassLoader)
      _validForRun = ctx.runId
      _classloader
    }

  /** Expand annotation macros */
  def expandAnnot(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.Tree = {
    val ann = mdef.mods.annotations.filter(isAnnotMacros).headOption
    val expansion = ann.flatMap {
      case ann @ Apply(Select(New(tpt), init), _) =>
        val tpdClass = ctx.typer.typedAheadType(tpt)

        val className = tpdClass.symbol.fullName + "$inline$"
        // reflect macros definition
        val moduleClass = classloader.loadClass(className)
        val module = moduleClass.getField("MODULE$").get(null)
        val impl = moduleClass.getDeclaredMethods().find(_.getName == "apply").get
        impl.setAccessible(true)

        val expandee = {
          val mods1 = mdef.mods.withAnnotations(mdef.mods.annotations.filter(_ ne ann))
          mdef.withMods(mods1)
        }
        val mtree   = toMeta(expandee)
        val mprefix = toMeta(ann)
        val mResult = impl.invoke(module, mprefix, mtree).asInstanceOf[m.Tree]
        val result  = parse(mResult.syntax)(ctx)  // TODO: loss of position
        Some(result)
      case _ =>
        None
    }

    expansion.getOrElse(mdef)
  }

  private object ExtractApply {
    def unapply(tree: untpd.Tree): Option[(untpd.Tree, List[untpd.Tree], List[List[untpd.Tree]])] = tree match {
      case TypeApply(fun, targs) =>
        val Some((f, _, argss)) = unapply(fun)
        Some((f, targs, argss))
      case Apply(fun, args) =>
        val Some((f, targs, argss)) = unapply(fun)
        Some((f, targs, argss :+ args))
      case _ =>
        Some((tree, Nil, Nil))
    }
  }

  /** Expand def macros */
  def expandDef(tree: tpd.Tree)(implicit ctx: Context): untpd.Tree = tree match {
    case ExtractApply(Select(obj, method), targs, argss) =>
      val className = obj.symbol.info.classSymbol.fullName + "$inline$"
      // reflect macros definition
      val moduleClass = classloader.loadClass(className)
      val module = moduleClass.getField("MODULE$").get(null)
      val impl = moduleClass.getDeclaredMethods().find(_.getName == method.toString).get
      impl.setAccessible(true)

      val mtrees  = toMeta(obj) :: targs.map(tp => toMeta(tp, isTerm = false)) ++ argss.flatten.map(arg => toMeta(arg))
      val mResult = impl.invoke(module, mtrees: _*).asInstanceOf[m.Tree]
      parse(mResult.syntax)(ctx)  // TODO: loss of position
    case _ =>
      tree
  }

  def parse(code: String)(implicit ctx: Context): untpd.Tree = {
    val parser = new Parser(new SourceFile("<meta>", code.toArray))
    val (_, stats) = parser.templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  def isAnnotMacros(ann: untpd.Tree)(implicit ctx: Context): Boolean = {
    import Decorators._
    import StdNames._

    val symbol = ctx.typer.typedAheadAnnotation(ann)
    if (!symbol.exists) return false

    val annMethod = symbol.info.decl(nme.apply)
    val annImplMethod = symbol.owner.info
      .decl((symbol.name + "$inline").toTermName)
      .info
      .decl(nme.apply)

    val macrosAnnotType = ctx.requiredClassRef("scala.annotation.StaticAnnotation")

    symbol.typeRef <:< macrosAnnotType && annMethod.exists && annImplMethod.exists
  }
}

