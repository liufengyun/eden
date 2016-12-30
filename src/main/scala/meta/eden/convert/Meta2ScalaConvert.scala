package scala.meta.eden
package convert

import scala.{meta => m}
import dotty.tools.dotc._
import ast.{ untpd => d }
import ast.untpd.modsDeco
import core.Contexts.Context
import core.StdNames._
import core.Flags._
import core.Decorators._
import core.Constants._
import util.Positions
import scala.collection.mutable.ListBuffer

class Meta2ScalaConvert(private var mode: Mode = TermMode, private var loc: Loc = ExprLoc) {
  def withMode[T <: d.Tree](m: Mode)(f: => T) = {
    val before = mode
    mode = m
    val res = f
    mode = before
    res
  }

  def withLoc[T <: d.Tree](l: Loc)(f: => T) = {
    val before = loc
    loc = l
    val res = f
    loc = before
    res
  }

  // carry positions with the converted tree
  private implicit class ScalaWrapper(tree: ast.Positioned) {
    def from[S <: ast.Positioned](mtree: m.Tree): S = mtree.origin match {
      case m.internal.ast.Origin.Parsed(input, dialet, pos) =>
        val posScala = Positions.Position(pos.start, pos.end)
        tree.withPos(posScala).asInstanceOf[S]
      case _ =>
        tree.asInstanceOf[S]
    }
  }

  private implicit def toScalaList[T <: d.Tree](trees: Seq[m.Tree])(implicit ctx: Context): List[T] =
    trees.map(t => toScala[T](t)).toList

  private implicit def toScalaListList[T <: d.Tree](treess: Seq[Seq[m.Tree]])(implicit ctx: Context): List[List[T]] =
    treess.map(trees => toScalaList[T](trees)).toList

  private implicit def toScalaOpt(treeOpt: Option[m.Tree])(implicit ctx: Context): d.Tree =
    if (treeOpt.isEmpty) d.EmptyTree else treeOpt.get

  private implicit def toModifiers(mods: Seq[m.Mod])(implicit ctx: Context): d.Modifiers = {
    def addMod(modifiers: d.Modifiers, mod: d.Mod): d.Modifiers =
      modifiers.withAddedMod(mod) | mod.flags

    mods.foldLeft(d.Modifiers()) { (modifiers, mod) =>
      mod match {
        case m.Mod.Annot(body) =>
          modifiers.withAddedAnnotation(mod)
        case m.Mod.Private(within) =>
          val modifiers2 = within match {
            case m.Name.Indeterminate(name) => modifiers.withPrivateWithin(name.toTypeName)
            case m.Term.This(m.Name.Anonymous()) => modifiers | Local
            case m.Name.Anonymous() => modifiers
          }
          addMod(modifiers2, d.Mod.Private().from(mod))
        case m.Mod.Protected(within) =>
          val modifiers2 = within match {
            case m.Name.Indeterminate(name) => modifiers.withPrivateWithin(name.toTypeName)
            case m.Term.This(m.Name.Anonymous()) => modifiers | Local
            case m.Name.Anonymous() => modifiers
          }
          addMod(modifiers2, d.Mod.Protected().from(mod))
        case m.Mod.Implicit() =>
          addMod(modifiers, d.Mod.Implicit().from(mod))
        case m.Mod.Final() =>
          addMod(modifiers, d.Mod.Final().from(mod))
        case m.Mod.Sealed() =>
          addMod(modifiers, d.Mod.Sealed().from(mod))
        case m.Mod.Override() =>
          addMod(modifiers, d.Mod.Override().from(mod))
        case m.Mod.Case() =>
          modifiers | Case
        case m.Mod.Abstract() =>
          addMod(modifiers, d.Mod.Abstract().from(mod))
        case m.Mod.Covariant() =>
          modifiers | Covariant
        case m.Mod.Contravariant() =>
          modifiers | Contravariant
        case m.Mod.Lazy() =>
          addMod(modifiers, d.Mod.Lazy().from(mod))
        case m.Mod.ValParam() =>
          addMod(modifiers, d.Mod.Val().from(mod))
        case m.Mod.VarParam() =>
          addMod(modifiers, d.Mod.Var().from(mod))
        case m.Mod.Inline() =>
          addMod(modifiers, d.Mod.Inline().from(mod))
      }
    }
  }

  implicit def toScala[T <: d.Tree](tree: m.Tree)(implicit ctx: Context): T = ((tree match {
    case m.Lit(value) =>
      d.Literal(Constant(value))

    // case m.Name.Anonymous() =>            // TODO: ???
    // case m.Name.Indeterminate(name) =>  d.Ident(name.toTermName)

    case m.Term.This(qual)  =>
      qual match {
        case m.Name.Anonymous() => d.This(d.Ident(tpnme.EMPTY))
        case m.Name.Indeterminate(name) => d.This(d.Ident(name.toTypeName))
      }
    case m.Term.Super(thisp, superp) =>
      val qual = thisp match {
        case m.Name.Anonymous() => d.This(d.Ident(tpnme.EMPTY))
        case m.Name.Indeterminate(name) => d.This(d.Ident(name.toTypeName))
      }

      val mix = superp match {
        case m.Name.Anonymous() => d.Ident(tpnme.EMPTY)
        case m.Name.Indeterminate(name) => d.Ident(name.toTypeName)
      }
      d.Super(qual, mix)
    case m.Term.Name(name) =>
      d.Ident(name.toTermName)
    case m.Term.Select(qual, m.Term.Name(name)) =>
      d.Select(qual, name.toTermName)
    case m.Term.Interpolate(m.Term.Name(id), parts, args) =>
      val thickets =
        for { (arg, part) <- args.zip(parts.take(args.size)) }
          yield d.Thicket(arg, part)
      val segments =
        if (parts.size > args.size)
          (parts.last: d.Tree) +: thickets
        else thickets
      d.InterpolatedString(id.toTermName, segments.toList)
    // case m.Term.Xml(parts, args) =>
    case m.Term.Apply(fun, args) =>
      d.Apply(fun, args)
    case m.Term.ApplyInfix(lhs, m.Term.Name(name), targs, args) => // TODO: targs?
      require(targs.size == 0)
      if (targs.size == 0 && args.size == 1)
        d.InfixOp(lhs, name.toTermName, args(0))
      else
        d.InfixOp(lhs, name.toTermName, d.Tuple(args))
    case m.Term.ApplyType(fun, targs) =>
      if (loc == SuperCallLoc)
        withMode(TypeMode) { d.AppliedTypeTree(fun, targs) }
      else
        d.TypeApply(fun, targs)
    case m.Term.ApplyUnary(m.Term.Name(op), arg) =>
      d.PrefixOp(op.toTermName, arg)
    case m.Term.Assign(lhs, rhs) =>
      d.Assign(lhs, rhs)
    case m.Term.Update(fun, argss, rhs) =>
      require(argss.size > 0)
      val zero = d.Apply(fun, argss(0))
      val left = argss.drop(1).foldLeft(zero) { (acc, args) =>
        d.Apply(acc, args)
      }
      d.Assign(left, rhs)
    case m.Term.Return(expr) =>
      d.Return(expr, d.EmptyTree)
    case m.Term.Throw(expr) =>
      d.Throw(expr)
    case m.Term.Ascribe(expr, tpe) =>
      d.Typed(expr, tpe)
    case m.Term.Annotate(expr, annots) =>
      require(annots.size > 0)
      val zero = d.Annotated(expr, annots.head)
      annots.tail.foldLeft(zero) { (acc, annot) => d.Annotated(acc, annot) }
    case m.Term.Tuple(args) =>
      d.Tuple(args)
    case m.Term.Block(stats) =>
      require(stats.size > 0)
      d.Block(stats.dropRight(1), stats.last)
    case m.Term.If(cond, thenp, elsep) =>
      d.If(cond, thenp, elsep)
    case m.Term.Match(expr, cases) =>
      d.Match(expr, cases)
    case m.Term.TryWithCases(expr, cases, finallyp) =>
      d.Try(expr, cases, finallyp)
    case m.Term.TryWithTerm(expr, catchp, finallyp) =>
      d.ParsedTry(expr, catchp, finallyp)
    case m.Term.Function(params, body) =>
      d.Function(params, body)
    case m.Term.PartialFunction(cases) =>
      d.Match(d.Thicket(Nil), cases)
    case m.Term.While(expr, body) =>
      d.WhileDo(expr, body)
    case m.Term.Do(body, expr) =>
      d.DoWhile(body, expr)
    case m.Term.For(enums, body) =>
      d.ForDo(enums, body)
    case m.Term.ForYield(enums, body) =>
      d.ForYield(enums, body)
    case m.Term.New(m.Template(Nil, Seq(mctor), m.Term.Param(Nil, m.Name.Anonymous(), None, None), None)) =>
      mctor match {
        case m.Term.Apply(ctor, args) => d.New(withLoc(SuperCallLoc) { (ctor: d.Tree) }, List(args))
        case _ => d.New(withLoc(SuperCallLoc) { mctor }, Nil)
      }
    case m.Term.New(templ) =>
      d.New(templ)
    case m.Term.Placeholder() =>  // TODO: ???
      d.Ident(nme.WILDCARD)                       // TODO: dotty parsed tree is _$1, _$2, etc
    case m.Term.Eta(expr) =>
      d.PostfixOp(expr, nme.WILDCARD)
    case m.Term.Arg.Named(m.Term.Name(name), expr) =>
      d.NamedArg(name.toTermName, expr)
    case m.Term.Arg.Repeated(expr) =>
      d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR))
    case m.Term.Param(mods, name, tpOpt, default) =>
      val scalaName = name match {
        case m.Name.Anonymous() => nme.WILDCARD
        case m.Term.Name(name) =>  name.toTermName
      }
      d.ValDef(scalaName, tpOpt, default).withMods(mods).withFlags(Param)

    case m.Type.Name(name) =>
      d.Ident(name.toTypeName)
    case m.Type.Select(qual, m.Type.Name(name)) =>
      d.Select(qual, name.toTypeName)
    case m.Type.Project(qual, m.Type.Name(name)) =>
      d.Select(qual, name.toTypeName)
    case m.Type.Singleton(ref) =>
      d.SingletonTypeTree(ref)
    case m.Type.Apply(tpe, args) =>
      d.AppliedTypeTree(tpe, args)
    case m.Type.ApplyInfix(lhs, m.Type.Name(op), rhs) =>
      d.InfixOp(lhs, op.toTypeName, rhs)
    case m.Type.Function(params, res) =>
      d.Function(params, res)
    case m.Type.Tuple(args) =>
      d.Tuple(args)
    case m.Type.With(lhs, rhs) =>
      d.AndTypeTree(lhs, rhs)
    case m.Type.And(lhs, rhs) =>
      d.AndTypeTree(lhs, rhs)
    case m.Type.Or(lhs, rhs) =>
      d.OrTypeTree(lhs, rhs)
    case m.Type.Refine(tpe, stats) =>
      require(tpe.nonEmpty)                                   // structural types not supported in Dotty currently
      d.RefinedTypeTree(tpe.get, stats)
    // case m.Type.Existential(tpe, stats) =>    // illegal in Dotty
    case m.Type.Annotate(tpe, annots) =>
      require(annots.size > 0)
      val zero = d.Annotated(tpe, annots.head)
      annots.tail.foldLeft(zero) { (acc, annot) => d.Annotated(acc, annot) }
    // case m.Type.Placeholder(bounds) =>   // TODO: ???
    case m.Type.Bounds(lo, hi) =>
      require(lo.nonEmpty || hi.nonEmpty)
      d.TypeBoundsTree(lo, hi)
    case m.Type.Arg.ByName(tpe) =>
      d.ByNameTypeTree(tpe)
    case m.Type.Arg.Repeated(tpe) =>
      d.PostfixOp(tpe, nme.raw.STAR)
    case m.Type.Param(mods, name, tparams, tbounds, vbounds, cbounds) =>  // no view bounds in Dotty
      require(vbounds.size == 0)
      val scalaName = name match {
        case m.Name.Anonymous() => nme.WILDCARD.toTypeName                // TODO: dotty parsed tree is _$1, _$2, etc
        case m.Type.Name(name)  => name.toTypeName
      }
      val rhs = if (cbounds.size > 0) d.ContextBounds(tbounds, cbounds) else tbounds: d.Tree
      d.TypeDef(name, d.PolyTypeTree(tparams, rhs)).withMods(mods).withFlags(Param)

    case m.Pat.Var.Term(m.Term.Name(name)) =>
      d.Ident(name.toTermName)
    case m.Pat.Var.Type(m.Type.Name(name)) =>
      d.Ident(name.toTermName)
    case m.Pat.Wildcard() =>
      d.Ident(nme.WILDCARD)
    case m.Pat.Bind(m.Pat.Var.Term(m.Term.Name(name)), rhs) =>
      d.Bind(name.toTermName, rhs)
    case m.Pat.Alternative(lhs, rhs) =>
      d.Alternative(List(lhs, rhs))
    case m.Pat.Tuple(args) =>
      d.Tuple(args)
    case m.Pat.Extract(ref, targs, args) =>
      if (targs.size > 0)
        d.Apply(d.TypeApply(ref, targs), args)
      else
        d.Apply(ref, args)
    case m.Pat.ExtractInfix(lhs, m.Term.Name(op), rhs) =>
      if (rhs.size == 1)
        d.InfixOp(lhs, op.toTermName, rhs(0))
      else
        d.InfixOp(lhs, op.toTermName, d.Tuple(rhs))
    case m.Pat.Interpolate(m.Term.Name(id), parts, args) =>
      val thickets =
        for { (arg, part) <- args.zip(parts.take(args.size)) }
          yield d.Thicket(arg, part)
      val segments =
        if (parts.size > args.size)
          (parts.last: d.Tree) +: thickets
        else thickets
      d.InterpolatedString(id.toTermName, segments.toList)
    // case m.Pat.Xml(parts, args) =>       // illegal in Dotty
    case m.Pat.Typed(lhs, rhs) =>
      d.Typed(lhs, rhs)
    // case m.Pat.Arg.SeqWildcard() =>      // illegal in Dotty
    case m.Pat.Type.Wildcard() =>
      d.Ident(tpnme.WILDCARD)
    case m.Pat.Type.Project(qual, m.Type.Name(name)) =>
      d.Select(qual, name.toTypeName)
    case m.Pat.Type.Apply(tpe, args) =>
      d.AppliedTypeTree(tpe, args)
    case m.Pat.Type.ApplyInfix(lhs, m.Type.Name(op), rhs) =>
      d.InfixOp(lhs, op.toTypeName, rhs)
    case m.Pat.Type.Function(params, res) =>
      d.Function(params, res)
    case m.Pat.Type.Tuple(args) =>
      d.Tuple(args)
    case m.Pat.Type.With(lhs, rhs) =>
      d.AndTypeTree(lhs, rhs)
    case m.Pat.Type.And(lhs, rhs) =>
      d.AndTypeTree(lhs, rhs)
    case m.Pat.Type.Or(lhs, rhs) =>
      d.OrTypeTree(lhs, rhs)
    case m.Pat.Type.Refine(tpe, stats) =>
      require(tpe.nonEmpty)                           // structural types not supported in Dotty currently
      d.RefinedTypeTree(tpe.get, stats)
    // case m.Pat.Type.Existential(tpe, stats) =>     // illegal in Dotty
    case m.Pat.Type.Annotate(tpe, annots) =>
      require(annots.size > 0)
      val zero = d.Annotated(tpe, annots.head)
      annots.tail.foldLeft(zero) { (acc, annot) => d.Annotated(acc, annot) }
    // case m.Pat.Type.Placeholder(bounds) =>         // TODO: ???

    case m.Decl.Val(mods, pats, tpe) =>
      pats match {
        case List(m.Pat.Var.Term(m.Term.Name(name))) =>
          d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods)
        case _ =>
          d.PatDef(mods, pats, tpe, d.EmptyTree)
      }
    case m.Decl.Var(mods, pats, tpe) =>
      pats match {
        case List(m.Pat.Var.Term(m.Term.Name(name))) =>
          d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods)
        case _ =>
          d.PatDef(mods, pats, tpe, d.EmptyTree)
      }
    case m.Decl.Def(mods, m.Term.Name(name), tparams, paramss, tpe) =>
      d.DefDef(name.toTermName, tparams, paramss, tpe, d.EmptyTree).withMods(mods)
    case m.Decl.Type(mods, m.Type.Name(name), tparams, bounds) =>
      if (tparams.size > 0)
        d.TypeDef(name.toTypeName, d.PolyTypeTree(tparams, bounds)).withMods(mods)
      else
        d.TypeDef(name.toTypeName, bounds).withMods(mods)

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      pats match {
        case List(m.Pat.Var.Term(m.Term.Name(name))) =>
          d.ValDef(name.toTermName, tpe, rhs: d.Tree).withMods(mods)
        case _ =>
          d.PatDef(mods, pats, tpe, rhs: d.Tree)
      }
    case m.Defn.Var(mods, pats, tpe, rhs) =>
       pats match {
        case List(m.Pat.Var.Term(m.Term.Name(name))) =>
          d.ValDef(name.toTermName, tpe, rhs: d.Tree).withMods(mods)
        case _ =>
          d.PatDef(mods, pats, tpe, rhs: d.Tree)
      }
    case m.Defn.Def(mods, m.Term.Name(name), tparams, paramss, tpe, body) =>
      d.DefDef(name.toTermName, tparams, paramss, tpe, body).withMods(mods)
    // case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
    case m.Defn.Type(mods, m.Type.Name(name), tparams, body) =>
      if (tparams.size > 0)
        d.TypeDef(name.toTypeName, d.PolyTypeTree(tparams, body)).withMods(mods)
      else
        d.TypeDef(name.toTypeName, body).withMods(mods)
    case m.Defn.Class(mods, m.Type.Name(name), tparams, ctor, templ) =>
      val scalaCtor: d.DefDef = if (tparams.size == 0) ctor else d.cpy.DefDef(ctor)(tparams = tparams)
      val scalaTempl = d.cpy.Template(templ)(constr = scalaCtor)
      d.TypeDef(name.toTypeName, scalaTempl).withMods(mods)
    case m.Defn.Trait(mods, m.Type.Name(name), tparams, ctor, templ) =>
      val scalaCtor: d.DefDef = if (tparams.size == 0) ctor else d.cpy.DefDef(ctor)(tparams = tparams)
      val scalaTempl = d.cpy.Template(templ)(constr = scalaCtor)
      d.TypeDef(name.toTypeName, scalaTempl).withMods(mods).withFlags(Trait)
    case m.Defn.Object(mods, m.Term.Name(name), templ) =>
      d.ModuleDef(name.toTermName, templ).withMods(mods).withFlags(Module)
    case m.Pkg(ref, stats) =>
      d.PackageDef(ref, stats)
    case m.Pkg.Object(mods, m.Term.Name(name), templ) =>
      d.ModuleDef(name.toTermName, templ).withMods(mods).withFlags(Package)

    case m.Ctor.Primary(mods, name, paramss) =>
      d.DefDef(nme.CONSTRUCTOR, Nil, paramss, d.TypeTree(), d.EmptyTree).withMods(mods)
    case m.Ctor.Secondary(mods, name, paramss, body) =>
      d.DefDef(nme.CONSTRUCTOR, Nil, paramss, d.TypeTree(), body).withMods(mods)
    case m.Ctor.Ref.Name(v) =>
      d.Ident(v.toTypeName)
    case m.Ctor.Ref.Select(qual, name) =>
      d.Select(qual, (name: d.Ident).name)
    case m.Ctor.Ref.Project(qual, name) =>
      d.Select(qual, name)
    // case m.Ctor.Ref.Function(name) =>                            // TODO: what is this?

    case m.Template(_, parents, self, stats) =>                     // no early stats in Dotty
      val scalaStats = stats match {
        case Some(Nil) => List(d.EmptyTree)
        case None => Nil
        case Some(stats) => toScalaList(stats)
      }
      d.Template(null, parents, self, scalaStats)                  // constructor is handled in ClassDef

    case m.Enumerator.Generator(pat, rhs) =>
      d.GenFrom(pat, rhs)
    case m.Enumerator.Val(pat, rhs) =>
      d.GenAlias(pat, rhs)
    case m.Enumerator.Guard(cond) =>
      cond

    case m.Import(importers) =>
      require(importers.size > 0)
      if (importers.size == 1)
        importers(0)
      else
        d.Thicket(importers)
    case m.Importer(ref, importees) =>
      d.Import(ref, importees)
    case m.Importee.Wildcard() =>
      d.Ident(nme.WILDCARD)
    case m.Importee.Name(m.Name.Indeterminate(name)) =>
      d.Ident(name.toTermName)
    case m.Importee.Rename(m.Name.Indeterminate(name), m.Name.Indeterminate(rename)) =>
      d.Thicket(d.Ident(name.toTermName), d.Ident(rename.toTermName))
    case m.Importee.Unimport(m.Name.Indeterminate(name)) =>
      d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD))

    case m.Case(pat, cond, body) =>
      d.CaseDef(pat, cond, body)

    // case m.Source(stats) =>
  }) : d.Tree).from(tree)
}

