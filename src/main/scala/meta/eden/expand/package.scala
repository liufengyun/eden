package scala.meta.eden

import scala.{meta => m}
import dotty.tools.dotc._
import ast.Trees._
import ast.{tpd, untpd}
import ast.untpd.modsDeco
import core.Contexts.Context
import core.Symbols._
import core.{Decorators, StdNames}

package object expand {
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
  def expandAnnotMacro(mdef: untpd.MemberDef)(implicit ctx: Context): untpd.Tree = {
    val ann = mdef.mods.annotations.filter(macros.isAnnotMacro).headOption
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
        Some(toScala(mResult))
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
  def expandDefMacro(tree: tpd.Tree)(implicit ctx: Context): untpd.Tree = tree match {
    case ExtractApply(Select(obj, method), targs, argss) =>
      val className = obj.symbol.info.classSymbol.fullName + "$inline$"
      // reflect macros definition
      val moduleClass = classloader.loadClass(className)
      val module = moduleClass.getField("MODULE$").get(null)
      val impl = moduleClass.getDeclaredMethods().find(_.getName == method.toString).get
      impl.setAccessible(true)

      val mtrees  = toMeta(obj) :: targs.map(tp => toMeta(tp, isTerm = false)) ++ argss.flatten.map(arg => toMeta(arg))
      val mResult = impl.invoke(module, mtrees: _*).asInstanceOf[m.Tree]
      toScala(mResult)
    case _ =>
      tree
  }
}
