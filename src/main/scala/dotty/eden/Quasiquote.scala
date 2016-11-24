package dotty.eden

import dotty.tools.dotc._
import ast.Trees._
import core.Contexts._
import core.Symbols._
import core.Decorators._
import core.Names._
import util.SourceFile
import parsing.Parsers.Parser


import java.lang.{ StringBuilder => JLSBuilder }
import scala.{meta => m}


/** Bootstrap quasiquotes
 *
 *  1. q"class A { ... }"
 *
 *     StringContext("object ", " { ", " }").q(name, main)
 *     ~~>
 *     scala.meta.Defn.Object.apply(scala.collection.immutable.Seq.apply[Nothing](), name, scala.meta.Template.apply(scala.collection.immutable.Seq.apply[Nothing](), scala.collection.immutable.Seq.apply[Nothing](), scala.meta.Term.Param.apply(scala.collection.immutable.Seq.apply[Nothing](), scala.meta.Name.Anonymous.apply(), scala.None, scala.None), scala.Some.apply[scala.collection.immutable.Seq[meta.Defn.Def]](scala.collection.immutable.Seq.apply[meta.Defn.Def](main))))
 *
 *  2. val q"class A { ... }" = ...
 *
 *     case StringContext("object ", " { ..", " }").q((name @ _), (stats @ _))
 *     ~~>
 *     case {
 *       final class $anon extends scala.AnyRef {
 *         def <init>(): <$anon: AnyRef> = {
 *           $anon.super.<init>();
 *           ()
 *         };
 *         def unapply(input: scala.meta.Tree): Option[(scala.meta.Term.Name, scala.collection.immutable.Seq[scala.meta.Stat])] = input match {
 *           case scala.meta.Defn.Object.unapply(<unapply-selector>) <unapply> (scala.collection.immutable.Seq.unapplySeq[scala.meta.Mod](<unapply-selector>) <unapply> (), (quasiquote$macro$1$hole$0 @ _), scala.meta.Template.unapply(<unapply-selector>) <unapply> (scala.collection.immutable.Seq.unapplySeq[scala.meta.Stat](<unapply-selector>) <unapply> (), scala.collection.immutable.Seq.unapplySeq[scala.meta.Ctor.Call](<unapply-selector>) <unapply> (), scala.meta.Term.Param.unapply(<unapply-selector>) <unapply> (scala.collection.immutable.Seq.unapplySeq[scala.meta.Mod](<unapply-selector>) <unapply> (), scala.meta.Name.Anonymous.unapply(<unapply-selector>) <unapply> (), scala.None, scala.None), (quasiquote$macro$1$hole$1 @ _))) => scala.Tuple2.apply[Some[scala.meta.Term.Name], Option[scala.collection.immutable.Seq[scala.meta.Stat]]](scala.Some.apply[scala.meta.Term.Name]((quasiquote$macro$1$hole$0: scala.meta.Term.Name)), scala.meta.internal.quasiquotes.Flatten.unapply[scala.meta.Stat](quasiquote$macro$1$hole$1).flatMap[scala.collection.immutable.Seq[scala.meta.Stat]](((tree: scala.collection.immutable.Seq[scala.meta.Stat]) => scala.Some.apply[scala.collection.immutable.Seq[scala.meta.Stat]]((tree: scala.collection.immutable.Seq[scala.meta.Stat]))))) match {
 *             case (_1: Some[scala.meta.Term.Name], _2: Option[scala.collection.immutable.Seq[scala.meta.Stat]])(Some[scala.meta.Term.Name], Option[scala.collection.immutable.Seq[scala.meta.Stat]])((x: scala.meta.Term.Name)Some[scala.meta.Term.Name]((quasiquote$macro$1$result$0 @ _)), (x: scala.collection.immutable.Seq[scala.meta.Stat])Some[scala.collection.immutable.Seq[scala.meta.Stat]]((quasiquote$macro$1$result$1 @ _))) => scala.Some.apply[(scala.meta.Term.Name, scala.collection.immutable.Seq[scala.meta.Stat])](scala.Tuple2.apply[scala.meta.Term.Name, scala.collection.immutable.Seq[scala.meta.Stat]](quasiquote$macro$1$result$0, quasiquote$macro$1$result$1))
 *             case _ => scala.None
 *           }
 *           case _ => scala.None
 *         }
 *       };
 *       new $anon()
 *     }.unapply(<unapply-selector>) <unapply> ((name @ _), (stats @ _))
 *
 */
object Quasiquote {
  import ast.untpd._

  type MetaParser = (m.Input, m.Dialect) => m.Tree
  type QuoteLabel = String

  val quasiquoteTermDialect = m.Dialect.forName("QuasiquoteTerm(Dotty, Multi)")
  val quasiquotePatDialect = m.Dialect.forName("QuasiquotePat(Dotty, Multi)")

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

  def apply(tree: Tree)(implicit ctx: Context): Tree = {
    println("<-------------")
    println(tree)
    println("------------->")
    val (tag, code) = reifyInput(tree)
    println("<------------")
    println("quoted:" + code)
    println("------------->")
    val parser = instantiateParser(parserMap(tag))
    val mTree = parser(m.inputs.Input.String(code), quasiquoteTermDialect)
    println("<------------")
    println("mTree:" + mTree.structure)
    println("------------->")
    val res = quote(mTree)
    // reifySkeleton(mTree)
    println("<------------")
    println("res:" + res)
    println("------------->")

    res
  }

  def unapply(tree: Tree)(implicit ctx: Context): Tree = {
    val (tag, code) = reifyInput(tree)
    val parser = instantiateParser(parserMap(tag))
    val mTree = parser(m.inputs.Input.String(code), quasiquotePatDialect)
    // TODO: make tree for unapply
    parse(mTree.syntax)
    // reifySkeleton(mTree)
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

  private def reifyInput(tree: Tree): (QuoteLabel, String) = tree match {
    case Apply(Select(Apply(Ident(StringContextName) , parts), name), args) =>
      val quoted = resugar(parts.asInstanceOf[List[Literal]], args.asInstanceOf[List[Ident]].map(_.name))
      (name.toString, quoted)
    case UnApply(Select(Select(Apply(Select(Ident(StringContextName), ApplyName), List(Typed(SeqLiteral(parts, _), _))), name), UnApplyName), _, pats) =>
      val quoted = resugar(parts.asInstanceOf[List[Literal]], parts.asInstanceOf[List[Bind]].map(_.name))
      (name.toString, quoted)
  }

  def isQuasiquote(symbol: Symbol, tree: Tree)(implicit ctx: Context): Boolean = {
    symbol.exists && symbol.enclosingPackageClass.showFullName == "scala.meta.quasiquotes" &&
      parserMap.contains(symbol.owner.name.toString.dropRight(1)) &&
      (symbol.name.toString == "apply" || symbol.name.toString == "unapply" )
  }

  /** Resugar tree into string interpolation */
  private def resugar(parts: List[Literal], args: List[Name]): String = {
    val pi = parts.iterator
    val ai = args.iterator
    val bldr = new JLSBuilder(pi.next.const.value.toString)
    while (ai.hasNext) {
      bldr append "$" + ai.next
      bldr append pi.next.const.value
    }
    bldr.toString
  }

  // TODO: handle Quasi
  private def reifySkeleton(meta: m.Tree)(implicit ctx: Context): Tree = {
    EmptyTree
  }
}
