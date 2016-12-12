package dotty.eden

import dotty.tools.dotc._
import ast._
import Trees._
import core.Contexts._
import core.Symbols._
import core.Decorators._
import core.Constants._

import scala.{meta => m}
import m.dialects.Dotty


/** Bootstrap quasiquotes
 *
 *  1. q"class A { ... }"
 *
 *     StringContext("object ", " { ", " }").q(name, main)
 *
 *  2. val q"class A { ... }" = ...
 *
 *     case StringContext("object ", " { ..", " }").q((name @ _), (stats @ _))
 *
 * Credit: https://github.com/densh/joyquote
 *
 */
object Quasiquote {
  import tpd._

  type MetaParser = (m.Input, m.Dialect) => m.Tree
  type QuoteLabel = String

  // term dialect suffices the purpose, no need for pattern dialect
  val quasiquoteTermDialect = m.Dialect.forName("QuasiquoteTerm(Dotty, Multi)")

  private val StringContextName = "StringContext".toTermName
  private val ApplyName = "apply".toTermName
  private val UnApplyName = "unapply".toTermName

  private val parserMap = Map(
    "q"          ->    "XtensionQuasiquoteTerm",
    "arg"        ->    "XtensionQuasiquoteTermArg",
    "param"      ->    "XtensionQuasiquoteTermParam",
    "t"          ->    "XtensionQuasiquoteType",
    "targ"       ->    "XtensionQuasiquoteTypeArg",
    "tparam"     ->    "XtensionQuasiquoteTypeParam",
    "p"          ->    "XtensionQuasiquoteCaseOrPattern",
    "parg"       ->    "XtensionQuasiquotePatternArg",
    "pt"         ->    "XtensionQuasiquotePatternType",
    "ctor"       ->    "XtensionQuasiquoteCtor",
    "template"   ->    "XtensionQuasiquoteTemplate",
    "mod"        ->    "XtensionQuasiquoteMod",
    "enumerator" ->    "XtensionQuasiquoteEnumerator",
    "importer"   ->    "XtensionQuasiquoteImporter",
    "importee"   ->    "XtensionQuasiquoteImportee",
    "source"     ->    "XtensionQuasiquoteSource"
  )


  private[eden] object Hole {
    val pat = java.util.regex.Pattern.compile("^placeholder(\\d+)$")
    def apply(i: Int) = s"$$placeholder$i"
    def unapply(s: String): Option[Int] = {
      val m = pat.matcher(s)
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }

  // TODO: problem passing Boolean values
  def expand(tree: untpd.Tree, isTerm: Boolean)(implicit ctx: Context): untpd.Tree = {
    // println("<-------------")
    // println(tree + "@" + tree.pos.line())
    // println("------------->")
    val (tag, code, args) = reifyInput(tree)
    // println("<------------")
    // println("quoted:" + code)
    // println("------------->")
    val parser = instantiateParser(parserMap(tag))
    val mTree = parser(m.Input.String(code), quasiquoteTermDialect)
    // println("<------------")
    // println("mTree:" + mTree.structure)
    // println("------------->")
    val res = new Quote(tree, args, isTerm).lift(mTree)
    // println("<------------")
    // println("res:" + res)
    // println("syntax:" + res.show)
    // println("------------->")

    res
  }

  private def instantiateParser(parserName: String)(implicit ctx: Context): MetaParser = {
    val parsersModuleClass = Class.forName("scala.meta.quasiquotes.package$", true, this.getClass.getClassLoader)
    val parsersModule = parsersModuleClass.getField("MODULE$").get(null)
    val parserModuleGetter = parsersModule.getClass.getDeclaredMethod(parserName)
    val parserModuleInstance = parserModuleGetter.invoke(parsersModule)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: m.Input, dialect: m.Dialect) => {
      try parserMethod.invoke(parserModuleInstance, input, dialect).asInstanceOf[m.Tree]
      catch { case ex: java.lang.reflect.InvocationTargetException => throw ex.getTargetException }
    }
  }
  /**
   *    val q"class A { ... }" =
   *
   *    UnApply(
   *      Select(
   *        Select(
   *          Apply(Select(Ident(StringContext), apply), List(
   *            Typed(
   *              SeqLiteral(
   *                List(Literal("class "), Literal(" { def "), Literal(" }")),
   *                TypeTree()
   *              ),
   *              TypeTree())
   *          ))
   *        , q)
   *      , unapply)
   *    , List(), List(Bind(name, Ident(_)),Bind(main, Ident(_))))
   *
   *
   *    q"class A { ... }"
   *
   *    Apply(
   *      Select(
   *        Apply(Select(Ident(StringContext), apply), List(
   *          Typed(
   *            SeqLiteral(List(Literal("class "), Literal(" { def "), Literal(" }")),
   *              TypeTree()
   *            )
   *          , TypeTree())
   *        ))
   *      , q)
   *    , List(Ident(name),Ident(main)))
   *
   */

  private def reifyInput(tree: untpd.Tree): (QuoteLabel, String, List[untpd.Tree]) = tree match {
    case Apply(Select(Apply(Ident(StringContextName) , parts), name), args) =>
      val quoted = resugar(parts.asInstanceOf[List[Literal]])
      (name.toString, quoted, args)
    case UnApply(Select(Select(Apply(Select(Ident(StringContextName), ApplyName), List(Typed(SeqLiteral(parts, _), _))), name), UnApplyName), _, pats) =>
      val quoted = resugar(parts.asInstanceOf[List[Literal]])
      (name.toString, quoted, pats)
  }

  def isQuasiquote(symbol: Symbol, tree: Tree)(implicit ctx: Context): Boolean = {
    symbol.exists && symbol.enclosingPackageClass.showFullName == "scala.meta.quasiquotes" &&
      parserMap.contains(symbol.owner.sourceModule.name.toString) &&
      (symbol.name.toString == "apply" || symbol.name.toString == "unapply" )
  }

  /** Resugar tree into string interpolation */
  private def resugar(parts: List[Literal]): String = {
    parts.init.zipWithIndex.map { case (Literal(Constant(part)), i) =>
      s"$part${Hole(i)}"
    }.mkString("", "", parts.last.const.value.toString)
  }
}
