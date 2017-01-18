package scala.meta.eden
package quasiquote

import dotty.tools.dotc._
import core._
import ast._
import Contexts._
import Names._
import Decorators._
import Constants._
import Types._
import Symbols._
import Trees.Typed

import scala.{meta => m}
import scala.compat.Platform.EOL


object Quote {
  type Quasi = m.internal.ast.Quasi

  implicit class TreeOps(val tree: untpd.Tree) extends AnyVal {
    def select(name: Name): untpd.Select = untpd.Select(tree, name)

    def appliedTo(args: untpd.Tree*): untpd.Apply = untpd.Apply(tree, args.toList)

    def appliedToType(args: untpd.Tree*): untpd.TypeApply = untpd.TypeApply(tree, args.toList)
  }

  private def select(path: String, isTerm: Boolean = true): untpd.Tree = {
    val parts = path.split('.')
    val name = if (isTerm) parts.last.toTermName else parts.last.toTypeName

    parts.init.foldLeft[untpd.Tree](untpd.Ident("_root_".toTermName)) { (prefix, name) =>
      prefix.select(name.toTermName)
    }.select(name)
  }

  private def literal(value: Any): untpd.Tree = untpd.Literal(Constant(value))
}

/** Lift scala.meta trees as Dotty trees */
class Quote(tree: untpd.Tree, args: List[untpd.Tree], isTerm: Boolean = true)(implicit ctx: Context) {

  import Quote._

  val metaTreeType = ctx.requiredClassRef("scala.meta.Tree")
  val seqType = ctx.requiredClassRef("scala.collection.immutable.Seq")
  val optionType = ctx.requiredClassRef("scala.Option")
  val metaLiftType = ctx.requiredClassRef("scala.meta.quasiquotes.Lift")
  val metaUnliftType = ctx.requiredClassRef("scala.meta.quasiquotes.Unlift")

  def seqTypeOf(T: Type) = seqType.appliedTo(T)


  def liftSeq(trees: Seq[m.Tree]): untpd.Tree = {
    def loop(trees: List[m.Tree], acc: untpd.Tree, prefix: List[m.Tree]): untpd.Tree = trees match {
      case (quasi: Quasi) +: rest if quasi.rank == 1 =>
        if (acc.isEmpty) {
          if (prefix.isEmpty) loop(rest, liftQuasi(quasi), Nil)
          else loop(rest, prefix.foldRight(liftQuasi(quasi))((curr, acc) => {
            val currElement = lift(curr)
            untpd.InfixOp(currElement, "+:".toTermName, acc)
          }), Nil)
        } else {
          require(prefix.isEmpty)
          if (isTerm) loop(rest, untpd.InfixOp(acc, "++".toTermName, liftQuasi(quasi)), Nil)
          else {
            ctx.error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank), tree.pos)
            untpd.EmptyTree
          }
        }
      case other +: rest =>
        if (acc.isEmpty) loop(rest, acc, prefix :+ other)
        else {
          require(prefix.isEmpty)
          loop(rest, untpd.InfixOp(acc, ":+".toTermName, lift(other)), Nil)
        }
      case Nil =>
        if (acc.isEmpty)
          select("scala.collection.immutable.List").appliedTo(prefix.map(lift): _*)
        else acc
    }

    loop(trees.toList, untpd.EmptyTree, Nil)
  }

  def liftSeqSeq(treess: Seq[Seq[m.Tree]]): untpd.Tree = {
    val tripleDotQuasis = treess.flatten.collect { case quasi: Quasi if quasi.rank == 2 => quasi }
    if (tripleDotQuasis.length == 0) {
      val list = select("scala.collection.immutable.List")
      val args = treess.map(liftSeq)
      list.appliedTo(args: _*)
    } else if (tripleDotQuasis.length == 1) {
      if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
      else {
        ctx.error("implementation restriction: can't mix ...$ with anything else in parameter lists." +
          EOL + "See https://github.com/scalameta/scalameta/issues/406 for details.", tree.pos)
        untpd.EmptyTree
      }
    } else {
      ctx.error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(2), tree.pos)
      untpd.EmptyTree
    }
  }

  def liftOpt(treeOpt: Option[m.Tree]): untpd.Tree = treeOpt match {
    case Some(quasi: Quasi) =>
      liftQuasi(quasi, optional = true)
    case Some(tree) =>
      select("scala.Some").appliedTo(lift(tree))
    case None =>
      select("scala.None")
  }

  def liftOptSeq(treesOpt: Option[Seq[m.Tree]]): untpd.Tree = treesOpt match {
    case Some(Seq(quasi: Quasi)) if quasi.rank > 0 && !isTerm =>
      select("scala.meta.internal.quasiquotes.Flatten").appliedTo(liftQuasi(quasi))
    case Some(trees) =>
      select("scala.Some").appliedTo(liftSeq(trees))
    case None =>
      select("scala.None")
  }

  def liftQuasi(quasi: Quasi, expectedRank: Int = 0, optional: Boolean = false): untpd.Tree = {
    // credit: https://github.com/scalameta/scalameta/blob/master/scalameta/quasiquotes/src/main/scala/scala/meta/internal/quasiquotes/ReificationMacros.scala#L179
    implicit class XtensionClazz(clazz: Class[_]) {
      def toTpe: Type = {
        def loop(owner: Symbol, parts: List[String]): Symbol = parts match {
          case part :: Nil =>
            if (clazz.getName.endsWith("$")) owner.info.decl(part.toTermName).symbol
            else owner.info.decl(part.toTypeName).symbol
          case part :: rest =>
            loop(owner.info.decl(part.toTermName).symbol, rest)
          case Nil => ??? // unlikely
        }

        val name = dotty.tools.dotc.util.NameTransformer.decode(clazz.getName)
        val result = loop(ctx.definitions.RootClass, name.stripSuffix("$").split(Array('.', '$')).toList)
        if (result.is(Flags.ModuleVal)) result.termRef else result.typeRef
      }
    }

    implicit class XtensionType(tpe: Type) {
      def wrap(rank: Int): Type = {
        if (rank == 0) tpe
        else seqTypeOf(tpe.wrap(rank - 1))
      }
    }

    def quasiType: Type = {
      var inferred = quasi.pt.toTpe.wrap(expectedRank)
      if (optional) inferred = optionType.appliedTo(inferred)
      inferred
    }

    // type of the pattern
    def patternType(arg: untpd.Tree): Type = {
      arg match {
        case Typed(_, tp) => ctx.typer.typedType(tp).tpe
        case _ =>
          var inferred = metaTreeType.wrap(expectedRank)
          if (optional) inferred = optionType.appliedTo(inferred)
          inferred
      }
    }

    def convert(arg: untpd.Tree, from: Type, to: Type, base: TypeRef): untpd.Tree = {
      val conv = ctx.typer.inferImplicitArg(
         base.appliedTo(from, to), msgFun => ctx.error(msgFun(""), arg.pos), arg.pos
      )

      if (conv.isEmpty) arg
      else untpd.TypedSplice(conv).appliedTo(arg)
    }

    def unliftImplicitly(arg: untpd.Tree): untpd.Tree = {
      // shortcut
      val fromType = quasiType
      val toType = patternType(arg)
      if (fromType <:< toType || toType <:< fromType) return arg

      convert(arg, fromType, toType, metaUnliftType)
    }

    def liftImplicitly(arg: untpd.Tree): untpd.Tree = {
      // shortcut
      val toType = quasiType
      val fromType = ctx.typer.typedExpr(arg).tpe
      if (fromType <:< toType) return arg

      convert(arg, fromType.widen, toType, metaLiftType)
    }

    if (quasi.rank > 0) return liftQuasi(quasi.tree.asInstanceOf[Quasi], quasi.rank, optional)

    quasi.tree match {
      case m.Term.Name(Hole(i)) =>
        if (isTerm) liftImplicitly(args(i))
        else unliftImplicitly(args(i))
      case m.Type.Name(Hole(i)) =>
        if (isTerm) liftImplicitly(args(i))
        else unliftImplicitly(args(i))
    }
  }

  def liftCommon(obj: Any): untpd.Tree = obj match {
    case seq: Seq[m.Tree] => liftSeq(seq)
    case opt: Option[m.Tree] => liftOpt(opt)
    case tree: m.Tree => lift(tree)
    case _: String => literal(obj)
  }

  def lift(tree: m.Tree): untpd.Tree = (tree match {
    case quasi: Quasi =>
      liftQuasi(quasi)

    case m.Lit(v) =>
      select("scala.meta.Lit").appliedTo(literal(v))

    case m.Term.Apply(fun, args) =>
      // magic happens here with ...$args
      args match {
        case Seq(quasi: Quasi) if quasi.rank == 2 =>
          select("scala.meta.internal.ast.Helpers.TermApply").appliedTo(lift(fun), liftQuasi(quasi))
        case _ =>
          select("scala.meta.Term.Apply").appliedTo(lift(fun), liftSeq(args))
      }
    case m.Term.Update(fun, argss, rhs) =>
      select("scala.meta.Term.Update").appliedTo(lift(fun), liftSeqSeq(argss), lift(rhs))

    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      select("scala.meta.Decl.Def").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), lift(tpe))

    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Def").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))
    case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Macro").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))

    case m.Ctor.Primary(mods, name, paramss) =>
      select("scala.meta.Ctor.Primary").appliedTo(liftSeq(mods), lift(name), liftSeqSeq(paramss))
    case m.Ctor.Secondary(mods, name, paramss, body) =>
      select("scala.meta.Ctor.Secondary").appliedTo(liftSeq(mods), lift(name), liftSeqSeq(paramss), lift(body))

    case m.Template(early, parents, self, stats) =>
      select("scala.meta.Template").appliedTo(liftSeq(early), liftSeq(parents), lift(self), liftOptSeq(stats))

    // We cannot handle Seq[Seq[_]] or Opt[Seq[_]] because of erasure
    case tree: m.Tree =>
      val name = "scala.meta." + tree.productPrefix
      val args = tree.productIterator.toList.map(liftCommon)
      select(name).appliedTo(args: _*)


  }).withPos(this.tree.pos)
}
