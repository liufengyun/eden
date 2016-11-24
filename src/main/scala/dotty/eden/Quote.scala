package dotty.eden

import dotty.tools.dotc._
import ast.untpd._
import core.Contexts._
import core.Names._
import core.Decorators._
import core.Constants._


import scala.{meta => m}

object Quote {
  implicit class TreeOps(val tree: Tree) extends AnyVal {
    def select(name: Name): Select = Select(tree, name)

    def appliedTo(args: Tree*): Apply = Apply(tree, args.toList)
  }
}

/** this scala.meta trees as Dotty trees */
class Quote(tree: Tree, args: List[Tree])(implicit ctx: Context) {
  import Quote._

  private def select(path: String): Tree = {
    path.split('.').foldLeft[Tree](Ident("_root_".toTermName)) { (prefix, name) =>
      prefix.select(name.toTermName)
    }
  }

  private def literal(value: Any): Tree = Literal(Constant(value))

  def apply(trees: Seq[m.Tree]): Tree = {
    val list = select("scala.collection.immutable.List")
    val args = trees.map(this.apply)
    list.appliedTo(args: _*)
  }

  def applyss(ts: Seq[Seq[m.Tree]]): Tree = {
    val list = select("scala.collection.immutable.List")
    val args = ts.map(this.apply)
    list.appliedTo(args: _*)
  }

  def apply(treeOpt: Option[m.Tree]): Tree = treeOpt match {
    case Some(tree) =>
      select("scala.Some").appliedTo(this(tree))
    case None => select("scala.None")
  }

  def applyos(treesOpt: Option[Seq[m.Tree]]): Tree = treesOpt match {
    case Some(trees) =>
      select("scala.Some").appliedTo(this.apply(trees))
    case None => select("scala.None")
  }

  def apply(tree: m.Tree): Tree = (tree match {
    case m.Term.Name(Quasiquote.Hole(i)) =>
      args(i)
    case m.Type.Name(Quasiquote.Hole(i)) =>
      args(i)

    case m.Lit(value) => select("scala.meta.Lit").appliedTo(literal(value))

    case m.Name.Anonymous() =>
      select("scala.meta.Name.Anonymous").appliedTo()
    case m.Name.Indeterminate(name) =>
      select("scala.meta.Name.Indeterminate").appliedTo(literal(name))

    case m.Term.This(qual)  =>
      select("scala.meta.Term.This").appliedTo(this(qual))
    case m.Term.Super(thisp, superp) =>
      select("scala.meta.Term.Super").appliedTo(this(thisp), this(superp))
    case m.Term.Name(name) =>
      select("scala.meta.Term.Name").appliedTo(literal(name))
    case m.Term.Select(qual, name) =>
      select("scala.meta.Term.Select").appliedTo(this(qual), this(name))
    case m.Term.Interpolate(prefix, parts, args) =>
      select("scala.meta.Term.Interpolate").appliedTo(
        this(prefix), this(parts), this(args)
      )
    case m.Term.Xml(parts, args) =>
      select("scala.meta.Term.Xml").appliedTo(this(parts), this(args))
    case m.Term.Apply(fun, args) =>
      select("scala.meta.Term.Apply").appliedTo(this(fun), this(args))
    case m.Term.ApplyInfix(lhs, op, targs, args) =>
      select("scala.meta.Term.ApplyInfix").appliedTo(this(lhs), this(op), this(targs), this(args))
    case m.Term.ApplyType(fun, targs) =>
      select("scala.meta.Term.ApplyType").appliedTo(this(fun), this(targs))
    case m.Term.ApplyUnary(op, arg) =>
      select("scala.meta.Term.ApplyUnary").appliedTo(this(op), this(arg))
    case m.Term.Assign(lhs, rhs) =>
      select("scala.meta.Term.Assign").appliedTo(this(lhs), this(rhs))
    case m.Term.Update(fun, argss, rhs) =>
      select("scala.meta.Term.Update").appliedTo(this(fun), this.applyss(argss), this(rhs))
    case m.Term.Return(expr) =>
      select("scala.meta.Term.Return").appliedTo(this(expr))
    case m.Term.Throw(expr) =>
      select("scala.meta.Term.Throw").appliedTo(this(expr))
    case m.Term.Ascribe(expr, tpe) =>
      select("scala.meta.Term.Ascribe").appliedTo(this(expr), this(tpe))
    case m.Term.Annotate(expr, annots) =>
      select("scala.meta.Term.Annotate").appliedTo(this(expr), this(annots))
    case m.Term.Tuple(args) =>
      select("scala.meta.Term.Tuple").appliedTo(this(args))
    case m.Term.Block(stats) =>
      select("scala.meta.Term.Block").appliedTo(this(stats))
    case m.Term.If(cond, thenp, elsep) =>
      select("scala.meta.Term.If").appliedTo(this(cond), this(thenp), this(elsep))
    case m.Term.Match(expr, cases) =>
      select("scala.meta.Term.Match").appliedTo(this(expr), this(cases))
    case m.Term.TryWithCases(expr, catchp, finallyp) =>
      select("scala.meta.Term.TryWithCases").appliedTo(this(expr), this(catchp), this(finallyp))
    case m.Term.TryWithTerm(expr, catchp, finallyp) =>
      select("scala.meta.Term.TryWithTerm").appliedTo(this(expr), this(catchp), this(finallyp))
    case m.Term.Function(params, body) =>
      select("scala.meta.Term.Function").appliedTo(this(params), this(body))
    case m.Term.PartialFunction(cases) =>
      select("scala.meta.Term.PartialFunction").appliedTo(this(cases))
    case m.Term.While(expr, body) =>
      select("scala.meta.Term.While").appliedTo(this(expr), this(body))
    case m.Term.Do(body, expr) =>
      select("scala.meta.Term.Do").appliedTo(this(body), this(expr))
    case m.Term.For(enums, body) =>
      select("scala.meta.Term.For").appliedTo(this(enums), this(body))
    case m.Term.ForYield(enums, body) =>
      select("scala.meta.Term.ForYield").appliedTo(this(enums), this(body))
    case m.Term.New(templ) =>
      select("scala.meta.Term.New").appliedTo(this(templ))
    case m.Term.Placeholder() =>
      select("scala.meta.Term.Placeholder").appliedTo()
    case m.Term.Eta(expr) =>
      select("scala.meta.Term.Eta").appliedTo(this(expr))
    case m.Term.Arg.Named(name, expr) =>
      select("scala.meta.Term.Arg.Named").appliedTo(this(name), this(expr))
    case m.Term.Arg.Repeated(expr) =>
      select("scala.meta.Term.Arg.Repeated").appliedTo(this(expr))
    case m.Term.Param(mods, name, tpe, default) =>
      select("scala.meta.Term.Param").appliedTo(this(mods), this(name), this(tpe), this(default))

    case m.Type.Name(name) =>
      select("scala.meta.Type.Name").appliedTo(literal(name))
    case m.Type.Select(qual, name) =>
      select("scala.meta.Type.Select").appliedTo(this(qual), this(name))
    case m.Type.Project(qual, name) =>
      select("scala.meta.Type.Project").appliedTo(this(qual), this(name))
    case m.Type.Singleton(ref) =>
      select("scala.meta.Type.Singleton").appliedTo(this(ref))
    case m.Type.Apply(tpe, args) =>
      select("scala.meta.Type.Apply").appliedTo(this(tpe), this(args))
    case m.Type.ApplyInfix(lhs, op, rhs) =>
      select("scala.meta.Type.ApplyInfix").appliedTo(this(lhs), this(op), this(rhs))
    case m.Type.Function(params, res) =>
      select("scala.meta.Type.Function").appliedTo(this(params), this(res))
    case m.Type.Tuple(args) =>
      select("scala.meta.Type.Tuple").appliedTo(this(args))
    case m.Type.With(lhs, rhs) =>
      select("scala.meta.Type.With").appliedTo(this(lhs), this(rhs))
    case m.Type.And(lhs, rhs) =>
      select("scala.meta.Type.And").appliedTo(this(lhs), this(rhs))
    case m.Type.Or(lhs, rhs) =>
      select("scala.meta.Type.or").appliedTo(this(lhs), this(rhs))
    case m.Type.Refine(tpe, stats) =>
      select("scala.meta.Type.Refine").appliedTo(this(tpe), this(stats))
    case m.Type.Existential(tpe, stats) =>
      select("scala.meta.Type.Existential").appliedTo(this(tpe), this(stats))
    case m.Type.Annotate(tpe, annots) =>
      select("scala.meta.Type.Annotate").appliedTo(this(tpe), this(annots))
    case m.Type.Placeholder(bounds) =>
      select("scala.meta.Type.Placeholder").appliedTo(this(bounds))
    case m.Type.Bounds(lo, hi) =>
      select("scala.meta.Type.Bounds").appliedTo(this(lo), this(hi))
    case m.Type.Arg.ByName(tpe) =>
      select("scala.meta.Type.Arg.ByName").appliedTo(this(tpe))
    case m.Type.Arg.Repeated(tpe) =>
      select("scala.meta.Type.Arg.Repeated").appliedTo(this(tpe))
    case m.Type.Param(mods, name, tparams, tbounds, vbounds, cbounds) =>
      select("scala.meta.Type.Param").appliedTo(
        this(mods), this(name), this(tparams), this(tbounds), this(vbounds), this(cbounds)
      )

    case m.Pat.Var.Term(name) =>
      select("scala.meta.Pat.Var.Term").appliedTo(this(name))
    case m.Pat.Var.Type(name) =>
      select("scala.meta.Pat.Var.Type").appliedTo(this(name))
    case m.Pat.Wildcard() =>
      select("scala.meta.Pat.Wildcard").appliedTo()
    case m.Pat.Alternative(lhs, rhs) =>
      select("scala.meta.Pat.Alternative").appliedTo(this(lhs), this(rhs))
    case m.Pat.Tuple(args) =>
      select("scala.meta.Pat.Tuple").appliedTo(this(args))
    case m.Pat.Extract(ref, targs, args) =>
      select("scala.meta.Pat.Extract").appliedTo(this(ref), this(targs), this(args))
    case m.Pat.Interpolate(prefix, parts, args) =>
      select("scala.meta.Pat.Interpolate").appliedTo(this(prefix), this(parts), this(args))
    case m.Pat.Xml(parts, args) =>
      select("scala.meta.Pat.Xml").appliedTo(this(parts), this(args))
    case m.Pat.Typed(lhs, rhs) =>
      select("scala.meta.Pat.Typed").appliedTo(this(lhs), this(rhs))
    case m.Pat.Arg.SeqWildcard() =>
      select("scala.meta.Pat.Arg.SeqWildcard").appliedTo()
    case m.Pat.Type.Wildcard() =>
      select("scala.meta.Pat.Type.Wildcard").appliedTo()
    case m.Pat.Type.Project(qual, name) =>
      select("scala.meta.Pat.Type.Project").appliedTo(this(qual), this(name))
    case m.Pat.Type.Apply(tpe, args) =>
      select("scala.meta.Pat.Type.Apply").appliedTo(this(tpe), this(args))
    case m.Pat.Type.ApplyInfix(lhs, op, rhs) =>
      select("scala.meta.Pat.Type.ApplyInfix").appliedTo(this(lhs), this(op), this(rhs))
    case m.Pat.Type.Function(params, res) =>
      select("scala.meta.Pat.Type.Function").appliedTo(this(params), this(res))
    case m.Pat.Type.Tuple(args) =>
      select("scala.meta.Pat.Type.Tuple").appliedTo(this(args))
    case m.Pat.Type.With(lhs, rhs) =>
      select("scala.meta.Pat.Type.With").appliedTo(this(lhs), this(rhs))
    case m.Pat.Type.And(lhs, rhs) =>
      select("scala.meta.Pat.Type.And").appliedTo(this(lhs), this(rhs))
    case m.Pat.Type.Or(lhs, rhs) =>
      select("scala.meta.Pat.Type.Or").appliedTo(this(lhs), this(rhs))
    case m.Pat.Type.Refine(tpe, stats) =>
      select("scala.meta.Pat.Type.Refine").appliedTo(this(tpe), this(stats))
    case m.Pat.Type.Existential(tpe, stats) =>
      select("scala.meta.Pat.Type.Existential").appliedTo(this(tpe), this(stats))
    case m.Pat.Type.Annotate(tpe, annots) =>
      select("scala.meta.Pat.Type.Annotate").appliedTo(this(tpe), this(annots))
    case m.Pat.Type.Placeholder(bounds) =>
      select("scala.meta.Pat.Type.Placeholder").appliedTo(this(bounds))

    case m.Decl.Val(mods, pats, tpe) =>
      select("scala.meta.Decl.Val").appliedTo(this(mods), this(pats), this(tpe))
    case m.Decl.Var(mods, pats, tpe) =>
      select("scala.meta.Decl.Var").appliedTo(this(mods), this(pats), this(tpe))
    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      select("scala.meta.Decl.Def").appliedTo(this(mods), this(name), this(tparams), this.applyss(paramss), this(tpe))
    case m.Decl.Type(mods, name, tparams, bounds) =>
      select("scala.meta.Decl.Type").appliedTo(this(mods), this(name), this(tparams), this(bounds))

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      select("scala.meta.Defn.Val").appliedTo(this(mods), this(pats), this(tpe), this(rhs))
    case m.Defn.Var(mods, pats, tpe, rhs) =>
      select("scala.meta.Defn.Var").appliedTo(this(mods), this(pats), this(tpe), this(rhs))
    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Def").appliedTo(this(mods), this(name), this(tparams), this.applyss(paramss), this(tpe), this(body))
    case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
      select("scala.meta.Defn.Macro").appliedTo(this(mods), this(name), this(tparams), this.applyss(paramss), this(tpe), this(body))
    case m.Defn.Type(mods, name, tparams, body) =>
      select("scala.meta.Defn.Type").appliedTo(this(mods), this(name), this(tparams), this(body))
    case m.Defn.Class(mods, name, tparams, ctor, templ) =>
      select("scala.meta.Defn.Class").appliedTo(this(mods), this(name), this(tparams), this(ctor), this(templ))
    case m.Defn.Trait(mods, name, tparams, ctor, templ) =>
      select("scala.meta.Defn.Trait").appliedTo(this(mods), this(name), this(tparams), this(ctor), this(templ))
    case m.Defn.Object(mods, name, templ) =>
      select("scala.meta.Defn.Object").appliedTo(this(mods), this(name), this(templ))

    case m.Pkg(ref, stats) =>
      select("scala.meta.Pkg").appliedTo(this(ref), this(stats))
    case m.Pkg.Object(mods, name, templ) =>
      select("scala.meta.Pkg.Object").appliedTo(this(mods), this(name), this(templ))

    case m.Ctor.Primary(mods, name, paramss) =>
      select("scala.meta.Ctor.Primary").appliedTo(this(mods), this(name), this.applyss(paramss))
    case m.Ctor.Secondary(mods, name, paramss, body) =>
      select("scala.meta.Ctor.Secondary").appliedTo(this(mods), this(name), this.applyss(paramss))

    case m.Template(early, parents, self, stats) =>
      select("scala.meta.Template").appliedTo(this(early), this(parents), this(self), this.applyos(stats))

    case m.Mod.Annot(body) =>
      select("scala.meta.Mod.Annot").appliedTo(this(body))
    case m.Mod.Private(within) =>
      select("scala.meta.Mod.Private").appliedTo(this(within))
    case m.Mod.Protected(within) =>
      select("scala.meta.Mod.Protected").appliedTo(this(within))
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
      select("scala.meta.Enumerator.Generator").appliedTo(this(pat), this(rhs))
    case m.Enumerator.Val(pat, rhs) =>
      select("scala.meta.Enumerator.Val").appliedTo(this(pat), this(rhs))
    case m.Enumerator.Guard(cond) =>
      select("scala.meta.Enumerator.Guard").appliedTo(this(cond))

    case m.Import(importers) =>
      select("scala.meta.Import").appliedTo(this(importers))
    case m.Importer(ref, importees) =>
      select("scala.meta.Importer").appliedTo(this(ref), this(importees))
    case m.Importee.Wildcard() =>
      select("scala.meta.Importee.Wildcard").appliedTo()
    case m.Importee.Name(name) =>
      select("scala.meta.Importee.Name").appliedTo(this(name))
    case m.Importee.Rename(name, rename) =>
      select("scala.meta.Importee.Rename").appliedTo(this(name), this(rename))
    case m.Importee.Unimport(name) =>
      select("scala.meta.Importee.Unimport").appliedTo(this(name))

    case m.Case(pat, cond, body) =>
      select("scala.meta.Case").appliedTo(this(pat), this(cond), this(body))

    case m.Source(stats) =>
      select("scala.meta.Source").appliedTo(this(stats))
  }).withPos(this.tree.pos)
}
