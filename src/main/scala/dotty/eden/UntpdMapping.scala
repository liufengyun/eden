package dotty.eden

import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.ast.{Trees => d}
import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.core.NameOps._

/** Handles the mapping logic between dotty tree and meta trees
  *
  * The mapping is stateful, in order to remember the outer context of an AST
  *   - PatLoc
  *   - TypeLoc
  *   - TermLoc
  *   - ParamLoc
  *   - SelfLoc
  *
  * Principle:
  *   1. Don't create any meta tree or do any conversion here!
  *   2. Don't create extractor if it always return Some(..)
  *
  * This module only provides helper extractors, real conversion
  * happens in Convert.
  *
  **/

class UntpdMapping(var mode: Loc) {

  def withMode[T](m: Loc)(f: => T) = {
    val before = mode
    mode = m
    val res = f
    mode = before
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
      if (mode != TermLoc) return None
      Some((tree.fun, tree.args))
    }
  }

  object TermIdent {
    def unapply(tree: Ident): Option[Name] = {
      if (mode != TermLoc) return None
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
      if (tree.op.isTypeName || mode != TermLoc) return None
      Some((tree.left, tree.op.asTermName, tree.right))
    }
  }

  object TermPostfixOp {
    def unapply(tree: PostfixOp): Option[(Tree, TermName)] = {
      if (tree.op.isTypeName || mode != TermLoc) return None
      Some((tree.od, tree.op.asTermName))
    }
  }

  // ============ TYPES ============
  object TypeIdent {
    def unapply(tree: Ident): Option[TypeName] = {
      if (tree.name.isTermName) return None
      Some(tree.name.asTypeName)
    }
  }

  // ============ PATTERNS ============
  object PatExtract {
    def unapply(tree: Tree): Option[(Tree, List[Tree], List[Tree])] = {
      if (mode != PatLoc) return None

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
      if (mode != PatLoc) return None
      val name = tree.name
      if (name.isTypeName || tree.name == nme.WILDCARD) None
      else Some(name)
    }
  }

  object PatWildcard {
    def unapply(tree: Ident): Boolean = {
      if (mode != PatLoc) return false
      tree.name == nme.WILDCARD
    }
  }

  object PatBind {
    def unapply(tree: Bind): Option[(TermName, Tree)] = {
      if (tree.name.isTypeName) return None
      val d.Bind(name, body) = tree
      Some((name.asTermName, body))
    }
  }

  object PatTyped {
    def unapply(tree: Typed): Option[(Tree, Tree)] = {
      if (mode != PatLoc) return None
      val d.Typed(lhs, rhs) = tree
      Some((lhs, rhs))
    }
  }
}

