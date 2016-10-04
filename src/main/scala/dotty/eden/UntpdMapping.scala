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
  * Principle: Don't create any meta tree or do any conversion here !
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

  // ============ LITERALS ============
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
}
