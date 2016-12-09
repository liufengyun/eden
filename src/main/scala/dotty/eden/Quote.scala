package dotty.eden

import dotty.tools.dotc._
import ast._
import core.Contexts._
import core.Names._
import core.StdNames._
import core.Decorators._
import core.Constants._
import typer.Implicits._

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

  // TODO: move these two methods to scala.meta
  // seqSeqApply
  def apply(fun: m.Term, argss: List[List[m.Term]]): m.Term = argss match {
    case args :: rest => rest.foldLeft(m.Term.Apply(fun, args)) { (acc, args) => m.Term.Apply(acc, args) }
    case _ => m.Term.Apply(fun, Nil)
  }

  // seqSeqUnapply
  def unapply(call: m.Term.Apply): Option[(m.Term, Seq[Seq[m.Term.Arg]])] = {
    def recur(acc: Seq[Seq[m.Term.Arg]], term: m.Term): (m.Term, Seq[Seq[m.Term.Arg]])  = term match {
      case m.Term.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
      case fun => (fun, acc)
    }

    Some(recur(Nil, call))
  }
}

/** Lift scala.meta trees as Dotty trees */
class Quote(tree: untpd.Tree, args: List[untpd.Tree], isTerm: Boolean = true)(implicit ctx: Context) {
  import Quote._

  val metaTreeType = ctx.requiredClassRef("scala.meta.Tree")


  def liftSeq(trees: Seq[m.Tree]): untpd.Tree =  {
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
          loop(rest,  untpd.InfixOp(acc, ":+".toTermName, lift(other)), Nil)
        }
      case Nil =>
        if (acc.isEmpty)
          select("scala.collection.immutable.List").appliedTo(prefix.map(lift): _*)
        else acc
    }

    loop (trees.toList, untpd.EmptyTree, Nil)
  }

  def liftSeqSeq(treess: Seq[Seq[m.Tree]]): untpd.Tree = {
    val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
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
      liftQuasi(quasi)
    case Some(tree) =>
      select("scala.Some").appliedTo(lift(tree))
    case None =>
      select("scala.None")
  }

  def liftOptSeq(treesOpt: Option[Seq[m.Tree]]): untpd.Tree = treesOpt match {
    case Some(Seq(quasi: Quasi)) if quasi.rank > 0 && !isTerm =>
      select("scala.meta.internal.quasiquotes.Flatten").appliedTo(liftQuasi(quasi))
    // case Some(Seq(quasi: Quasi)) if quasi.rank == 0 && !isTerm =>
    //  liftQuasi(quasi)
    case Some(trees) =>
      select("scala.Some").appliedTo(liftSeq(trees))
    case None =>
      select("scala.None")
  }

  def liftQuasi(quasi: Quasi): untpd.Tree = {
    if (quasi.rank > 0) return liftQuasi(quasi.tree.asInstanceOf[Quasi])

    quasi.tree match {
      case m.Term.Name(Quasiquote.Hole(i)) => args(i)
      case m.Type.Name(Quasiquote.Hole(i)) => args(i)
    }
  }

  def liftImplicitly(arg: tpd.Tree): untpd.Tree = {
    // shortcut
    if (arg.tpe <:< metaTreeType) return arg

    val result = ctx.typer.inferImplicitArg(arg.tpe.widen, msgFun => ctx.error(msgFun(""), arg.pos), arg.pos)
    result appliedTo arg

    /*
    val result = ctx.typer.inferView(arg, metaTreeType)
    result match {
      case SearchSuccess(tree, ref, tstate) =>
        tree appliedTo arg
      case _ =>
        ctx.error(s"couldn't find implicit value of type Lift[${arg.tpe}]", arg.pos)
        arg
    } // */
  }

  def lift(tree: m.Tree): untpd.Tree = (tree match {
    case quasi: Quasi  =>
      liftQuasi(quasi)

    case m.Lit(value) => select("scala.meta.Lit").appliedTo(literal(value))

    case m.Name.Anonymous() =>
      select("scala.meta.Name.Anonymous").appliedTo()
    case m.Name.Indeterminate(name) =>
      select("scala.meta.Name.Indeterminate").appliedTo(literal(name))

    case m.Term.This(qual)  =>
      select("scala.meta.Term.This").appliedTo(lift(qual))
    case m.Term.Super(thisp, superp) =>
      select("scala.meta.Term.Super").appliedTo(lift(thisp), lift(superp))
    case m.Term.Name(name) =>
      select("scala.meta.Term.Name").appliedTo(literal(name))
    case m.Term.Select(qual, name) =>
      select("scala.meta.Term.Select").appliedTo(lift(qual), lift(name))
    case m.Term.Interpolate(prefix, parts, args) =>
      select("scala.meta.Term.Interpolate").appliedTo(
        lift(prefix), liftSeq(parts), liftSeq(args)
      )
    case m.Term.Xml(parts, args) =>
      select("scala.meta.Term.Xml").appliedTo(liftSeq(parts), liftSeq(args))
    case m.Term.Apply(fun, args) =>
      // magic happens here with ...$args
      args match {
        case Seq(quasi: Quasi) if quasi.rank == 2 =>
          select("dotty.eden.Quote").appliedTo(lift(fun), liftQuasi(quasi))
        case _ =>
          select("scala.meta.Term.Apply").appliedTo(lift(fun), liftSeq(args))
      }
    case m.Term.ApplyInfix(lhs, op, targs, args) =>
      select("scala.meta.Term.ApplyInfix").appliedTo(lift(lhs), lift(op), liftSeq(targs), liftSeq(args))
    case m.Term.ApplyType(fun, targs) =>
      select("scala.meta.Term.ApplyType").appliedTo(lift(fun), liftSeq(targs))
    case m.Term.ApplyUnary(op, arg) =>
      select("scala.meta.Term.ApplyUnary").appliedTo(lift(op), lift(arg))
    case m.Term.Assign(lhs, rhs) =>
      select("scala.meta.Term.Assign").appliedTo(lift(lhs), lift(rhs))
    case m.Term.Update(fun, argss, rhs) =>
      select("scala.meta.Term.Update").appliedTo(lift(fun), liftSeqSeq(argss), lift(rhs))
    case m.Term.Return(expr) =>
      select("scala.meta.Term.Return").appliedTo(lift(expr))
    case m.Term.Throw(expr) =>
      select("scala.meta.Term.Throw").appliedTo(lift(expr))
    case m.Term.Ascribe(expr, tpe) =>
      select("scala.meta.Term.Ascribe").appliedTo(lift(expr), lift(tpe))
    case m.Term.Annotate(expr, annots) =>
      select("scala.meta.Term.Annotate").appliedTo(lift(expr), liftSeq(annots))
    case m.Term.Tuple(args) =>
      select("scala.meta.Term.Tuple").appliedTo(liftSeq(args))
    case m.Term.Block(stats) =>
      select("scala.meta.Term.Block").appliedTo(liftSeq(stats))
    case m.Term.If(cond, thenp, elsep) =>
      select("scala.meta.Term.If").appliedTo(lift(cond), lift(thenp), lift(elsep))
    case m.Term.Match(expr, cases) =>
      select("scala.meta.Term.Match").appliedTo(lift(expr), liftSeq(cases))
    case m.Term.TryWithCases(expr, catchp, finallyp) =>
      select("scala.meta.Term.TryWithCases").appliedTo(lift(expr), liftSeq(catchp), liftOpt(finallyp))
    case m.Term.TryWithTerm(expr, catchp, finallyp) =>
      select("scala.meta.Term.TryWithTerm").appliedTo(lift(expr), lift(catchp), liftOpt(finallyp))
    case m.Term.Function(params, body) =>
      select("scala.meta.Term.Function").appliedTo(liftSeq(params), lift(body))
    case m.Term.PartialFunction(cases) =>
      select("scala.meta.Term.PartialFunction").appliedTo(liftSeq(cases))
    case m.Term.While(expr, body) =>
      select("scala.meta.Term.While").appliedTo(lift(expr), lift(body))
    case m.Term.Do(body, expr) =>
      select("scala.meta.Term.Do").appliedTo(lift(body), lift(expr))
    case m.Term.For(enums, body) =>
      select("scala.meta.Term.For").appliedTo(liftSeq(enums), lift(body))
    case m.Term.ForYield(enums, body) =>
      select("scala.meta.Term.ForYield").appliedTo(liftSeq(enums), lift(body))
    case m.Term.New(templ) =>
      select("scala.meta.Term.New").appliedTo(lift(templ))
    case m.Term.Placeholder() =>
      select("scala.meta.Term.Placeholder").appliedTo()
    case m.Term.Eta(expr) =>
      select("scala.meta.Term.Eta").appliedTo(lift(expr))
    case m.Term.Arg.Named(name, expr) =>
      select("scala.meta.Term.Arg.Named").appliedTo(lift(name), lift(expr))
    case m.Term.Arg.Repeated(expr) =>
      select("scala.meta.Term.Arg.Repeated").appliedTo(lift(expr))
    case m.Term.Param(mods, name, tpe, default) =>
      select("scala.meta.Term.Param").appliedTo(liftSeq(mods), lift(name), liftOpt(tpe), liftOpt(default))

    case m.Type.Name(name) =>
      select("scala.meta.Type.Name").appliedTo(literal(name))
    case m.Type.Select(qual, name) =>
      select("scala.meta.Type.Select").appliedTo(lift(qual), lift(name))
    case m.Type.Project(qual, name) =>
      select("scala.meta.Type.Project").appliedTo(lift(qual), lift(name))
    case m.Type.Singleton(ref) =>
      select("scala.meta.Type.Singleton").appliedTo(lift(ref))
    case m.Type.Apply(tpe, args) =>
      select("scala.meta.Type.Apply").appliedTo(lift(tpe), liftSeq(args))
    case m.Type.ApplyInfix(lhs, op, rhs) =>
      select("scala.meta.Type.ApplyInfix").appliedTo(lift(lhs), lift(op), lift(rhs))
    case m.Type.Function(params, res) =>
      select("scala.meta.Type.Function").appliedTo(liftSeq(params), lift(res))
    case m.Type.Tuple(args) =>
      select("scala.meta.Type.Tuple").appliedTo(liftSeq(args))
    case m.Type.With(lhs, rhs) =>
      select("scala.meta.Type.With").appliedTo(lift(lhs), lift(rhs))
    case m.Type.And(lhs, rhs) =>
      select("scala.meta.Type.And").appliedTo(lift(lhs), lift(rhs))
    case m.Type.Or(lhs, rhs) =>
      select("scala.meta.Type.or").appliedTo(lift(lhs), lift(rhs))
    case m.Type.Refine(tpe, stats) =>
      select("scala.meta.Type.Refine").appliedTo(liftOpt(tpe), liftSeq(stats))
    case m.Type.Existential(tpe, stats) =>
      select("scala.meta.Type.Existential").appliedTo(lift(tpe), liftSeq(stats))
    case m.Type.Annotate(tpe, annots) =>
      select("scala.meta.Type.Annotate").appliedTo(lift(tpe), liftSeq(annots))
    case m.Type.Placeholder(bounds) =>
      select("scala.meta.Type.Placeholder").appliedTo(lift(bounds))
    case m.Type.Bounds(lo, hi) =>
      select("scala.meta.Type.Bounds").appliedTo(liftOpt(lo), liftOpt(hi))
    case m.Type.Arg.ByName(tpe) =>
      select("scala.meta.Type.Arg.ByName").appliedTo(lift(tpe))
    case m.Type.Arg.Repeated(tpe) =>
      select("scala.meta.Type.Arg.Repeated").appliedTo(lift(tpe))
    case m.Type.Param(mods, name, tparams, tbounds, vbounds, cbounds) =>
      select("scala.meta.Type.Param").appliedTo(
        liftSeq(mods), lift(name), liftSeq(tparams), lift(tbounds), liftSeq(vbounds), liftSeq(cbounds)
      )

    case m.Pat.Var.Term(name) =>
      select("scala.meta.Pat.Var.Term").appliedTo(lift(name))
    case m.Pat.Var.Type(name) =>
      select("scala.meta.Pat.Var.Type").appliedTo(lift(name))
    case m.Pat.Wildcard() =>
      select("scala.meta.Pat.Wildcard").appliedTo()
    case m.Pat.Bind(lhs, rhs) =>
      select("scala.meta.Pat.Bind").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Alternative(lhs, rhs) =>
      select("scala.meta.Pat.Alternative").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Tuple(args) =>
      select("scala.meta.Pat.Tuple").appliedTo(liftSeq(args))
    case m.Pat.Extract(ref, targs, args) =>
      select("scala.meta.Pat.Extract").appliedTo(lift(ref), liftSeq(targs), liftSeq(args))
    case m.Pat.ExtractInfix(lhs, op, rhs) =>
      select("scala.meta.Pat.ExtractInfix").appliedTo(lift(lhs), lift(op), liftSeq(rhs))
    case m.Pat.Interpolate(prefix, parts, args) =>
      select("scala.meta.Pat.Interpolate").appliedTo(lift(prefix), liftSeq(parts), liftSeq(args))
    case m.Pat.Xml(parts, args) =>
      select("scala.meta.Pat.Xml").appliedTo(liftSeq(parts), liftSeq(args))
    case m.Pat.Typed(lhs, rhs) =>
      select("scala.meta.Pat.Typed").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Arg.SeqWildcard() =>
      select("scala.meta.Pat.Arg.SeqWildcard").appliedTo()
    case m.Pat.Type.Wildcard() =>
      select("scala.meta.Pat.Type.Wildcard").appliedTo()
    case m.Pat.Type.Project(qual, name) =>
      select("scala.meta.Pat.Type.Project").appliedTo(lift(qual), lift(name))
    case m.Pat.Type.Apply(tpe, args) =>
      select("scala.meta.Pat.Type.Apply").appliedTo(lift(tpe), liftSeq(args))
    case m.Pat.Type.ApplyInfix(lhs, op, rhs) =>
      select("scala.meta.Pat.Type.ApplyInfix").appliedTo(lift(lhs), lift(op), lift(rhs))
    case m.Pat.Type.Function(params, res) =>
      select("scala.meta.Pat.Type.Function").appliedTo(liftSeq(params), lift(res))
    case m.Pat.Type.Tuple(args) =>
      select("scala.meta.Pat.Type.Tuple").appliedTo(liftSeq(args))
    case m.Pat.Type.With(lhs, rhs) =>
      select("scala.meta.Pat.Type.With").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.And(lhs, rhs) =>
      select("scala.meta.Pat.Type.And").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.Or(lhs, rhs) =>
      select("scala.meta.Pat.Type.Or").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.Refine(tpe, stats) =>
      select("scala.meta.Pat.Type.Refine").appliedTo(liftOpt(tpe), liftSeq(stats))
    case m.Pat.Type.Existential(tpe, stats) =>
      select("scala.meta.Pat.Type.Existential").appliedTo(lift(tpe), liftSeq(stats))
    case m.Pat.Type.Annotate(tpe, annots) =>
      select("scala.meta.Pat.Type.Annotate").appliedTo(lift(tpe), liftSeq(annots))
    case m.Pat.Type.Placeholder(bounds) =>
      select("scala.meta.Pat.Type.Placeholder").appliedTo(lift(bounds))

    case m.Decl.Val(mods, pats, tpe) =>
      select("scala.meta.Decl.Val").appliedTo(liftSeq(mods), liftSeq(pats), lift(tpe))
    case m.Decl.Var(mods, pats, tpe) =>
      select("scala.meta.Decl.Var").appliedTo(liftSeq(mods), liftSeq(pats), lift(tpe))
    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      select("scala.meta.Decl.Def").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), lift(tpe))
    case m.Decl.Type(mods, name, tparams, bounds) =>
      select("scala.meta.Decl.Type").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), lift(bounds))

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      select("scala.meta.Defn.Val").appliedTo(liftSeq(mods), liftSeq(pats), liftOpt(tpe), lift(rhs))
    case m.Defn.Var(mods, pats, tpe, rhs) =>
      select("scala.meta.Defn.Var").appliedTo(liftSeq(mods), liftSeq(pats), liftOpt(tpe), liftOpt(rhs))
    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Def").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))
    case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Macro").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))
    case m.Defn.Type(mods, name, tparams, body) =>
      select("scala.meta.Defn.Type").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), lift(body))
    case m.Defn.Class(mods, name, tparams, ctor, templ) =>
      select("scala.meta.Defn.Class").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), lift(ctor), lift(templ))
    case m.Defn.Trait(mods, name, tparams, ctor, templ) =>
      select("scala.meta.Defn.Trait").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), lift(ctor), lift(templ))
    case m.Defn.Object(mods, name, templ) =>
      select("scala.meta.Defn.Object").appliedTo(liftSeq(mods), lift(name), lift(templ))

    case m.Pkg(ref, stats) =>
      select("scala.meta.Pkg").appliedTo(lift(ref), liftSeq(stats))
    case m.Pkg.Object(mods, name, templ) =>
      select("scala.meta.Pkg.Object").appliedTo(liftSeq(mods), lift(name), lift(templ))

    case m.Ctor.Primary(mods, name, paramss) =>
      select("scala.meta.Ctor.Primary").appliedTo(liftSeq(mods), lift(name), liftSeqSeq(paramss))
    case m.Ctor.Secondary(mods, name, paramss, body) =>
      select("scala.meta.Ctor.Secondary").appliedTo(liftSeq(mods), lift(name), liftSeqSeq(paramss), lift(body))
    case m.Ctor.Ref.Name(v) =>
      select("scala.meta.Ctor.Ref.Name").appliedTo(literal(v))
    case m.Ctor.Ref.Select(qual, name) =>
      select("scala.meta.Ctor.Ref.Select").appliedTo(lift(qual), lift(name))
    case m.Ctor.Ref.Project(qual, name) =>
      select("scala.meta.Ctor.Ref.Project").appliedTo(lift(qual), lift(name))
    case m.Ctor.Ref.Function(name) =>
      select("scala.meta.Ctor.Ref.Function").appliedTo(lift(name))

    case m.Template(early, parents, self, stats) =>
      select("scala.meta.Template").appliedTo(liftSeq(early), liftSeq(parents), lift(self), liftOptSeq(stats))

    case m.Mod.Annot(body) =>
      select("scala.meta.Mod.Annot").appliedTo(lift(body))
    case m.Mod.Private(within) =>
      select("scala.meta.Mod.Private").appliedTo(lift(within))
    case m.Mod.Protected(within) =>
      select("scala.meta.Mod.Protected").appliedTo(lift(within))
    case m.Mod.Implicit() =>
      select("scala.meta.Mod.Implicit").appliedTo()
    case m.Mod.Final() =>
      select("scala.meta.Mod.Final").appliedTo()
    case m.Mod.Sealed() =>
      select("scala.meta.Mod.Sealed").appliedTo()
    case m.Mod.Override() =>
      select("scala.meta.Mod.Override").appliedTo()
    case m.Mod.Case() =>
      select("scala.meta.Mod.Case").appliedTo()
    case m.Mod.Abstract() =>
      select("scala.meta.Mod.Abstract").appliedTo()
    case m.Mod.Covariant() =>
      select("scala.meta.Mod.Covariant").appliedTo()
    case m.Mod.Contravariant() =>
      select("scala.meta.Mod.Contravariant").appliedTo()
    case m.Mod.Lazy() =>
      select("scala.meta.Mod.Lazy").appliedTo()
    case m.Mod.ValParam() =>
      select("scala.meta.Mod.ValParam").appliedTo()
    case m.Mod.VarParam() =>
      select("scala.meta.Mod.VarParam").appliedTo()
    case m.Mod.Inline() =>
      select("scala.meta.Mod.Inline").appliedTo()

    case m.Enumerator.Generator(pat, rhs) =>
      select("scala.meta.Enumerator.Generator").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Val(pat, rhs) =>
      select("scala.meta.Enumerator.Val").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Guard(cond) =>
      select("scala.meta.Enumerator.Guard").appliedTo(lift(cond))

    case m.Import(importers) =>
      select("scala.meta.Import").appliedTo(liftSeq(importers))
    case m.Importer(ref, importees) =>
      select("scala.meta.Importer").appliedTo(lift(ref), liftSeq(importees))
    case m.Importee.Wildcard() =>
      select("scala.meta.Importee.Wildcard").appliedTo()
    case m.Importee.Name(name) =>
      select("scala.meta.Importee.Name").appliedTo(lift(name))
    case m.Importee.Rename(name, rename) =>
      select("scala.meta.Importee.Rename").appliedTo(lift(name), lift(rename))
    case m.Importee.Unimport(name) =>
      select("scala.meta.Importee.Unimport").appliedTo(lift(name))

    case m.Case(pat, cond, body) =>
      select("scala.meta.Case").appliedTo(lift(pat), liftOpt(cond), lift(body))

    case m.Source(stats) =>
      select("scala.meta.Source").appliedTo(liftSeq(stats))
  }).withPos(this.tree.pos)
}
