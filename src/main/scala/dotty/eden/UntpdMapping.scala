package dotty.eden

import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.ast.{Trees => d}
import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._

/** Handles the mapping logic between dotty tree and meta trees
  *
  * The mapping is stateful, in order to remember the outer context of an AST
  *
  *   - TypeMode
  *   - TermMode
  *
  *   - PatLoc
  *   - ParamLoc
  *   - SelfLoc
  *   - SuperCallLoc
  *
  * Principle:
  *   1. Don't create any meta tree or do any conversion here!
  *   2. Don't create extractor if it always return Some(..)
  *
  * This module only provides helper extractors, real conversion
  * happens in Convert.
  *
  **/

class UntpdMapping(var mode: Mode, var loc: Loc) {

  // with is keyword
  def withs[T](m: Mode, l: Loc)(f: => T) = {
    this.withMode(m) { this.withLoc(l) {  f } }
  }

  def withMode[T](m: Mode)(f: => T) = {
    val before = mode
    mode = m
    val res = f
    mode = before
    res
  }

  def withLoc[T](l: Loc)(f: => T) = {
    val before = loc
    loc = l
    val res = f
    loc = before
    res
  }

  // ============ LITERALS ============
  object Literal {
    def unapply(tree: Literal): Option[Any] = tree match {
      case d.Literal(Constant(_: Type)) => None
      case d.Literal(Constant(_: Symbol)) => None
      case d.Literal(Constant(value)) => Some(value)
      case _ => None
    }
  }

  // ============ TERMS ============
  object TermApply {
    def unapply(tree: Apply): Option[(Tree, List[Tree])] = {
      if (loc != ExprLoc) return None
      Some((tree.fun, tree.args))
    }
  }

  object TermTypeApply {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = {
      if (mode == TypeMode) return None
      tree match {
        case d.TypeApply(fun, args) if loc == ExprLoc =>
          Some((fun, args))
        case d.AppliedTypeTree(tpt, args) if loc == SuperCallLoc =>  // new List[Int](4)
          Some((tpt, args))
        case _ => None
      }
    }
  }

  object TermIdent {
    def unapply(tree: Ident): Option[Name] = {
      if (loc != ExprLoc || mode != TermMode) return None
      val name = tree.name
      if (name.isTypeName || tree.name == nme.WILDCARD) None
      else Some(name)
    }
  }

  object TermSelect {
    def unapply(tree: Select): Option[(Tree, TermName)] = {
      if (tree.name.isTypeName) return None
      Some((tree.qualifier, tree.name.asTermName))
    }
  }

  object TermInfixOp {
    def unapply(tree: InfixOp): Option[(Tree, TermName, Tree)] = {
      if (tree.op.isTypeName || loc != ExprLoc || mode != TermMode) return None
      Some((tree.left, tree.op.asTermName, tree.right))
    }
  }

  object TermPostfixOp {
    def unapply(tree: PostfixOp): Option[(Tree, TermName)] = {
      if (tree.op.isTypeName || mode != TermMode || loc != ExprLoc) return None
      Some((tree.od, tree.op.asTermName))
    }
  }

  object RepeatedParam {
    def unapply(tree: Tree): Option[Ident] = tree match {
      case d.Typed(ident: Ident, d.Ident(tpnme.WILDCARD_STAR)) => Some(ident)
      case _ => None
    }
  }

  object TermNewNoTemplate {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case d.Apply(d.Select(d.New(ctor), nme.CONSTRUCTOR), args) =>
        Some((ctor, args))
      case _ =>
        None
    }
  }

  // ============ TYPES ============
  object TypeIdent {
    def unapply(tree: Ident): Option[TypeName] = {
      if (tree.name.isTermName || mode != TypeMode) return None
      Some(tree.name.asTypeName)
    }
  }

  object TypeInfixOp {
    def unapply(tree: InfixOp): Option[(Tree, TypeName, Tree)] = {
      if (mode != TypeMode) return None
      Some((tree.left, tree.op.asTypeName, tree.right))
    }
  }


  // ============ PATTERNS ============
  object PatExtract {
    def unapply(tree: Tree): Option[(Tree, List[Tree], List[Tree])] = {
      if (loc != PatLoc) return None

      val (fun, targs, args) = tree match {
        case d.Apply(d.TypeApply(fun, targs), args) => (fun, targs, args)
        case d.Apply(fun, args) => (fun, Nil, args)
        case _ => return None
      }

      Some((fun, targs, args))
    }
  }

  object PatVarIdent {
    def unapply(tree: Ident): Option[Name] = {
      if (loc != PatLoc || mode != TermMode) return None
      val name = tree.name
      if (name.isTypeName || tree.name == nme.WILDCARD) None
      else Some(name)
    }
  }

  object PatWildcard {
    def unapply(tree: Ident): Boolean = {
      if (loc != PatLoc || mode != TermMode) return false
      tree.name == nme.WILDCARD
    }
  }

  object PatBind {
    def unapply(tree: Bind): Option[(TermName, Tree)] = {
      if (loc != PatLoc || mode != TermMode) return None
      val d.Bind(name, body) = tree
      Some((name.asTermName, body))
    }
  }

  object PatTyped {
    def unapply(tree: Typed): Option[(Tree, Tree)] = {
      if (loc != PatLoc || mode != TermMode) return None
      val d.Typed(lhs, rhs) = tree
      Some((lhs, rhs))
    }
  }

  // ============ CTORS ============
  object CtorName {
    def unapply(tree: Ident): Option[TypeName] = {
      if (!tree.name.isTypeName || loc != SuperCallLoc || mode != TermMode) return None
      Some(tree.name.asTypeName)
    }
  }

  // ============ DEFNS ============
  object ValDef {
    def unapply(tree: ValDef)(implicit ctx: Context): Option[(Modifiers, Name, Option[Tree], Tree)] = {
      if (loc != ExprLoc || tree.mods.flags.is(Mutable) || tree.rhs.isEmpty) return None

      val d.ValDef(name, tpt, _) = tree
      val ltpt = if (!tpt.isEmpty) Some(tpt) else None
      Some((tree.mods, name, ltpt, tree.rhs))
    }
  }

  object VarDef {
    def unapply(tree: ValDef)(implicit ctx: Context): Option[(Modifiers, Name, Option[Tree], Tree)] = {
      if (loc != ExprLoc || !tree.mods.flags.is(Mutable) || tree.rhs.isEmpty) return None

      val d.ValDef(name, tpt, _) = tree
      val ltpt = if (!tpt.isEmpty) Some(tpt) else None
      Some((tree.mods, name, ltpt, tree.rhs))
    }
  }

  // ============ DECLS ============
  object VarDcl {
    def unapply(tree: ValDef)(implicit ctx: Context): Option[(Modifiers, Name, Tree)] = {
      if (loc != ExprLoc || !tree.mods.flags.is(Mutable) || !tree.rhs.isEmpty) return None

      val d.ValDef(name, tpt, _) = tree
      Some((tree.mods, name, tpt))
    }
  }

  object ValDcl {
    def unapply(tree: ValDef)(implicit ctx: Context): Option[(Modifiers, Name, Tree)] = {
      if (loc != ExprLoc || tree.mods.flags.is(Mutable) || !tree.rhs.isEmpty) return None

      val d.ValDef(name, tpt, _) = tree
      Some((tree.mods, name, tpt))
    }
  }

  // ============ PARAMS ============
  object ParamTerm {
    def unapply(tree: ValDef)(implicit ctx: Context): Option[(Modifiers, Name, Option[Tree], Option[Tree])] = {
      if (loc != ParamLoc) return None

      val d.ValDef(name, tpt, _) = tree
      val optTpt = if (tpt.isEmpty) None else Some(tpt)
      val optRhs = if (tree.rhs.isEmpty) None else Some(tree.rhs)
      Some((tree.mods, name, optTpt, optRhs))
    }
  }

}

