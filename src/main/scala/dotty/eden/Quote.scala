package dotty.eden

import dotty.tools.dotc._
import ast.untpd._
import core.Contexts._
import core.Names._
import core.Decorators._
import core.Constants._
import util.Positions._


import scala.{meta => m}

/** quote scala.meta trees as Dotty trees */
object quote {
  implicit class TreeOps(val tree: Tree) extends AnyVal {
    def select(name: Name): Select = Select(tree, name)

    def appliedTo(args: Tree*): Apply = Apply(tree, args.toList)
  }

  private def select(path: String)(implicit ctx: Context): Tree = {
    path.split('.').foldLeft[Tree](Ident("_root_".toTermName)) { (prefix, name) =>
      prefix.select(name.toTermName)
    }
  }

  private def literal(value: Any): Tree = Literal(Constant(value))

  def apply(trees: Seq[m.Tree])(implicit ctx: Context): Tree = {
    val list = select("scala.collection.immutable.List")
    val args = trees.map(quote.apply)
    list.appliedTo(args: _*)
  }

  def applyss(ts: Seq[Seq[m.Tree]])(implicit ctx: Context): Tree = {
    val list = select("scala.collection.immutable.List")
    val args = ts.map(quote.apply)
    list.appliedTo(args: _*)
  }

  def apply(treeOpt: Option[m.Tree])(implicit ctx: Context): Tree = treeOpt match {
    case Some(tree) =>
      select("scala.Some").appliedTo(quote(tree))
    case None => select("scala.None")
  }

  def applyos(treesOpt: Option[Seq[m.Tree]])(implicit ctx: Context): Tree = treesOpt match {
    case Some(trees) =>
      select("scala.Some").appliedTo(quote.apply(trees))
    case None => select("scala.None")
  }

  def apply(tree: m.Tree)(implicit ctx: Context): Tree = (tree match {
    case m.Lit(value) => select("scala.meta.Lit").appliedTo(literal(value))

    case m.Name.Anonymous() =>
      select("scala.meta.Name.Anonymous").appliedTo()
    case m.Name.Indeterminate(name) =>
      select("scala.meta.Name.Indeterminate").appliedTo(literal(name))

    case m.Term.This(qual)  =>
      select("scala.meta.Term.This").appliedTo(quote(qual))
    case m.Term.Super(thisp, superp) =>
      select("scala.meta.Term.Super").appliedTo(quote(thisp), quote(superp))
    case m.Term.Name(name) =>
      select("scala.meta.Term.Name").appliedTo(literal(name))
    case m.Term.Select(qual, name) =>
      select("scala.meta.Term.Select").appliedTo(quote(qual), quote(name))
    case m.Term.Interpolate(prefix, parts, args) =>
      select("scala.meta.Term.Interpolate").appliedTo(
        quote(prefix), quote(parts), quote(args)
      )
    case m.Term.Xml(parts, args) =>
      select("scala.meta.Term.Xml").appliedTo(quote(parts), quote(args))
    case m.Term.Apply(fun, args) =>
      select("scala.meta.Term.Apply").appliedTo(quote(fun), quote(args))
    case m.Term.ApplyType(fun, targs) =>
      select("scala.meta.Term.ApplyType").appliedTo(quote(fun), quote(targs))
    case m.Term.ApplyUnary(op, arg) =>
      select("scala.meta.Term.ApplyUnary").appliedTo(quote(op), quote(arg))
    case m.Term.Assign(lhs, rhs) =>
      select("scala.meta.Term.Assign").appliedTo(quote(lhs), quote(rhs))
    case m.Term.Update(fun, argss, rhs) =>
      select("scala.meta.Term.Update").appliedTo(quote(fun), quote.applyss(argss), quote(rhs))
    case m.Term.Return(expr) =>
      select("scala.meta.Term.Return").appliedTo(quote(expr))
    case m.Term.Throw(expr) =>
      select("scala.meta.Term.Throw").appliedTo(quote(expr))
    case m.Term.Ascribe(expr, tpe) =>
      select("scala.meta.Term.Ascribe").appliedTo(quote(expr), quote(tpe))
    case m.Term.Annotate(expr, annots) =>
      select("scala.meta.Term.Annotate").appliedTo(quote(expr), quote(annots))
    case m.Term.Tuple(args) =>
      select("scala.meta.Term.Tuple").appliedTo(quote(args))
    case m.Term.Block(stats) =>
      select("scala.meta.Term.Block").appliedTo(quote(stats))
    case m.Term.If(cond, thenp, elsep) =>
      select("scala.meta.Term.If").appliedTo(quote(cond), quote(thenp), quote(elsep))
    case m.Term.Match(expr, cases) =>
      select("scala.meta.Term.Match").appliedTo(quote(expr), quote(cases))
    case m.Term.TryWithCases(expr, catchp, finallyp) =>
      select("scala.meta.Term.TryWithCases").appliedTo(quote(expr), quote(catchp), quote(finallyp))
    case m.Term.TryWithTerm(expr, catchp, finallyp) =>
      select("scala.meta.Term.TryWithTerm").appliedTo(quote(expr), quote(catchp), quote(finallyp))
    case m.Term.Function(params, body) =>
      select("scala.meta.Term.Function").appliedTo(quote(params), quote(body))
    case m.Term.PartialFunction(cases) =>
      select("scala.meta.Term.PartialFunction").appliedTo(quote(cases))
    case m.Term.While(expr, body) =>
      select("scala.meta.Term.While").appliedTo(quote(expr), quote(body))
    case m.Term.Do(body, expr) =>
      select("scala.meta.Term.Do").appliedTo(quote(body), quote(expr))
    case m.Term.For(enums, body) =>
      select("scala.meta.Term.For").appliedTo(quote(enums), quote(body))
    case m.Term.ForYield(enums, body) =>
      select("scala.meta.Term.ForYield").appliedTo(quote(enums), quote(body))
    case m.Term.New(templ) =>
      select("scala.meta.Term.New").appliedTo(quote(templ))
    case m.Term.Placeholder() =>
      select("scala.meta.Term.Placeholder").appliedTo()
    case m.Term.Eta(expr) =>
      select("scala.meta.Term.Eta").appliedTo(quote(expr))
    case m.Term.Arg.Named(name, expr) =>
      select("scala.meta.Term.Arg.Named").appliedTo(quote(name), quote(expr))
    case m.Term.Arg.Repeated(expr) =>
      select("scala.meta.Term.Arg.Repeated").appliedTo(quote(expr))
    case m.Term.Param(mods, name, tpe, default) =>
      select("scala.meta.Term.Param").appliedTo(quote(mods), quote(name), quote(tpe), quote(default))

    case m.Type.Name(name) =>
      select("scala.meta.Type.Name").appliedTo(literal(name))
    case m.Type.Select(qual, name) =>
      select("scala.meta.Type.Select").appliedTo(quote(qual), quote(name))
    case m.Type.Project(qual, name) =>
      select("scala.meta.Type.Project").appliedTo(quote(qual), quote(name))
    case m.Type.Singleton(ref) =>
      select("scala.meta.Type.Singleton").appliedTo(quote(ref))
    case m.Type.Apply(tpe, args) =>
      select("scala.meta.Type.Apply").appliedTo(quote(tpe), quote(args))
    case m.Type.ApplyInfix(lhs, op, rhs) =>
      select("scala.meta.Type.ApplyInfix").appliedTo(quote(lhs), quote(op), quote(rhs))
    case m.Type.Function(params, res) =>
      select("scala.meta.Type.Function").appliedTo(quote(params), quote(res))
    case m.Type.Tuple(args) =>
      select("scala.meta.Type.Tuple").appliedTo(quote(args))
    case m.Type.With(lhs, rhs) =>
      select("scala.meta.Type.With").appliedTo(quote(lhs), quote(rhs))
    case m.Type.And(lhs, rhs) =>
      select("scala.meta.Type.And").appliedTo(quote(lhs), quote(rhs))
    case m.Type.Or(lhs, rhs) =>
      select("scala.meta.Type.or").appliedTo(quote(lhs), quote(rhs))
    case m.Type.Refine(tpe, stats) =>
      select("scala.meta.Type.Refine").appliedTo(quote(tpe), quote(stats))
    case m.Type.Existential(tpe, stats) =>
      select("scala.meta.Type.Existential").appliedTo(quote(tpe), quote(stats))
    case m.Type.Annotate(tpe, annots) =>
      select("scala.meta.Type.Annotate").appliedTo(quote(tpe), quote(annots))
    case m.Type.Placeholder(bounds) =>
      select("scala.meta.Type.Placeholder").appliedTo(quote(bounds))
    case m.Type.Bounds(lo, hi) =>
      select("scala.meta.Type.Bounds").appliedTo(quote(lo), quote(hi))
    case m.Type.Arg.ByName(tpe) =>
      select("scala.meta.Type.Arg.ByName").appliedTo(quote(tpe))
    case m.Type.Arg.Repeated(tpe) =>
      select("scala.meta.Type.Arg.Repeated").appliedTo(quote(tpe))
    case m.Type.Param(mods, name, tparams, tbounds, vbounds, cbounds) =>
      select("scala.meta.Type.Param").appliedTo(
        quote(mods), quote(name), quote(tparams), quote(tbounds), quote(vbounds), quote(cbounds)
      )

    case m.Pat.Var.Term(name) =>
      select("scala.meta.Pat.Var.Term").appliedTo(quote(name))
    case m.Pat.Var.Type(name) =>
      select("scala.meta.Pat.Var.Type").appliedTo(quote(name))
    case m.Pat.Wildcard() =>
      select("scala.meta.Pat.Wildcard").appliedTo()
    case m.Pat.Alternative(lhs, rhs) =>
      select("scala.meta.Pat.Alternative").appliedTo(quote(lhs), quote(rhs))
    case m.Pat.Tuple(args) =>
      select("scala.meta.Pat.Tuple").appliedTo(quote(args))
    case m.Pat.Extract(ref, targs, args) =>
      select("scala.meta.Pat.Extract").appliedTo(quote(ref), quote(targs), quote(args))
    case m.Pat.Interpolate(prefix, parts, args) =>
      select("scala.meta.Pat.Interpolate").appliedTo(quote(prefix), quote(parts), quote(args))
    case m.Pat.Xml(parts, args) =>
      select("scala.meta.Pat.Xml").appliedTo(quote(parts), quote(args))
    case m.Pat.Typed(lhs, rhs) =>
      select("scala.meta.Pat.Typed").appliedTo(quote(lhs), quote(rhs))
    case m.Pat.Arg.SeqWildcard() =>
      select("scala.meta.Pat.Arg.SeqWildcard").appliedTo()
    case m.Pat.Type.Wildcard() =>
      select("scala.meta.Pat.Type.Wildcard").appliedTo()
    case m.Pat.Type.Project(qual, name) =>
      select("scala.meta.Pat.Type.Project").appliedTo(quote(qual), quote(name))
    case m.Pat.Type.Apply(tpe, args) =>
      select("scala.meta.Pat.Type.Apply").appliedTo(quote(tpe), quote(args))
    case m.Pat.Type.ApplyInfix(lhs, op, rhs) =>
      select("scala.meta.Pat.Type.ApplyInfix").appliedTo(quote(lhs), quote(op), quote(rhs))
    case m.Pat.Type.Function(params, res) =>
      select("scala.meta.Pat.Type.Function").appliedTo(quote(params), quote(res))
    case m.Pat.Type.Tuple(args) =>
      select("scala.meta.Pat.Type.Tuple").appliedTo(quote(args))
    case m.Pat.Type.With(lhs, rhs) =>
      select("scala.meta.Pat.Type.With").appliedTo(quote(lhs), quote(rhs))
    case m.Pat.Type.And(lhs, rhs) =>
      select("scala.meta.Pat.Type.And").appliedTo(quote(lhs), quote(rhs))
    case m.Pat.Type.Or(lhs, rhs) =>
      select("scala.meta.Pat.Type.Or").appliedTo(quote(lhs), quote(rhs))
    case m.Pat.Type.Refine(tpe, stats) =>
      select("scala.meta.Pat.Type.Refine").appliedTo(quote(tpe), quote(stats))
    case m.Pat.Type.Existential(tpe, stats) =>
      select("scala.meta.Pat.Type.Existential").appliedTo(quote(tpe), quote(stats))
    case m.Pat.Type.Annotate(tpe, annots) =>
      select("scala.meta.Pat.Type.Annotate").appliedTo(quote(tpe), quote(annots))
    case m.Pat.Type.Placeholder(bounds) =>
      select("scala.meta.Pat.Type.Placeholder").appliedTo(quote(bounds))

    case m.Decl.Val(mods, pats, tpe) =>
      select("scala.meta.Decl.Val").appliedTo(quote(mods), quote(pats), quote(tpe))
    case m.Decl.Var(mods, pats, tpe) =>
      select("scala.meta.Decl.Var").appliedTo(quote(mods), quote(pats), quote(tpe))
    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      select("scala.meta.Decl.Def").appliedTo(quote(mods), quote(name), quote(tparams), quote.applyss(paramss), quote(tpe))
    case m.Decl.Type(mods, name, tparams, bounds) =>
      select("scala.meta.Decl.Type").appliedTo(quote(mods), quote(name), quote(tparams), quote(bounds))

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      select("scala.meta.Defn.Val").appliedTo(quote(mods), quote(pats), quote(tpe), quote(rhs))
    case m.Defn.Var(mods, pats, tpe, rhs) =>
      select("scala.meta.Defn.Var").appliedTo(quote(mods), quote(pats), quote(tpe), quote(rhs))
    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Def").appliedTo(quote(mods), quote(name), quote(tparams), quote.applyss(paramss), quote(tpe), quote(body))
    case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Macro").appliedTo(quote(mods), quote(name), quote(tparams), quote.applyss(paramss), quote(tpe), quote(body))
    case m.Defn.Type(mods, name, tparams, body) =>
      select("scala.meta.Defn.Type").appliedTo(quote(mods), quote(name), quote(tparams), quote(body))
    case m.Defn.Class(mods, name, tparams, ctor, templ) =>
      select("scala.meta.Defn.Class").appliedTo(quote(mods), quote(name), quote(tparams), quote(ctor), quote(templ))
    case m.Defn.Trait(mods, name, tparams, ctor, templ) =>
      select("scala.meta.Defn.Trait").appliedTo(quote(mods), quote(name), quote(tparams), quote(ctor), quote(templ))
    case m.Defn.Object(mods, name, templ) =>
      select("scala.meta.Defn.Object").appliedTo(quote(mods), quote(name), quote(templ))

    case m.Pkg(ref, stats) =>
      select("scala.meta.Pkg").appliedTo(quote(ref), quote(stats))
    case m.Pkg.Object(mods, name, templ) =>
      select("scala.meta.Pkg.Object").appliedTo(quote(mods), quote(name), quote(templ))

    case m.Ctor.Primary(mods, name, paramss) =>
      select("scala.meta.Ctor.Primary").appliedTo(quote(mods), quote(name), quote.applyss(paramss))
    case m.Ctor.Secondary(mods, name, paramss, body) =>
      select("scala.meta.Ctor.Secondary").appliedTo(quote(mods), quote(name), quote.applyss(paramss))

    case m.Template(early, parents, self, stats) =>
      select("scala.meta.Template").appliedTo(quote(early), quote(parents), quote(self), quote.applyos(stats))

    case m.Mod.Annot(body) =>
      select("scala.meta.Mod.Annot").appliedTo(quote(body))
    case m.Mod.Private(within) =>
      select("scala.meta.Mod.Private").appliedTo(quote(within))
    case m.Mod.Protected(within) =>
      select("scala.meta.Mod.Protected").appliedTo(quote(within))
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
      select("scala.meta.Enumerator.Generator").appliedTo(quote(pat), quote(rhs))
    case m.Enumerator.Val(pat, rhs) =>
      select("scala.meta.Enumerator.Val").appliedTo(quote(pat), quote(rhs))
    case m.Enumerator.Guard(cond) =>
      select("scala.meta.Enumerator.Guard").appliedTo(quote(cond))

    case m.Import(importers) =>
      select("scala.meta.Import").appliedTo(quote(importers))
    case m.Importer(ref, importees) =>
      select("scala.meta.Importer").appliedTo(quote(ref), quote(importees))
    case m.Importee.Wildcard() =>
      select("scala.meta.Importee.Wildcard").appliedTo()
    case m.Importee.Name(name) =>
      select("scala.meta.Importee.Name").appliedTo(quote(name))
    case m.Importee.Rename(name, rename) =>
      select("scala.meta.Importee.Rename").appliedTo(quote(name), quote(rename))
    case m.Importee.Unimport(name) =>
      select("scala.meta.Importee.Unimport").appliedTo(quote(name))

    case m.Case(pat, cond, body) =>
      select("scala.meta.Case").appliedTo(quote(pat), quote(cond), quote(body))

    case m.Source(stats) =>
      select("scala.meta.Source").appliedTo(quote(stats))
  }).withPos(Position(100))
}
