package dotty.eden

import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.ast.{ Trees => d }
import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.StdNames.tpnme
import dotty.tools.dotc.core.NameOps._

/** Handles the mapping logic between dotty tree and meta trees
  *
  * Principle:
  *   1. Don't create any meta tree or do any conversion here!
  *   2. Don't create extractor if it always return Some(..)
  *
  * This module only provides helper extractors, real conversion
  * happens in Convert.
  *
  **/
object UntpdMapping {
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
      // TODO: check if it's term location
      Some((tree.fun, tree.args))
    }
  }

  object TermIdent {
    def unapply(tree: Ident): Option[Name] = {
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
      if (tree.op.isTypeName) return None
      Some((tree.left, tree.op.asTermName, tree.right))
    }
  }

  // ============ TYPES ============
  object TypeIdent {
    def unapply(tree: Ident): Option[TypeName] = {
      if (tree.name.isTermName) return None
      Some(tree.name.asTypeName)
    }
  }

}
