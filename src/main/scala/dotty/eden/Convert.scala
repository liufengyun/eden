package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.Trees._
import dotty.eden.{UntpdMapping => u}

object Convert {
  // add .toMTree to untyped trees
  private implicit class UntpdTreeWrapper(tree: untpd.Tree) {
    def toMTree[T <: m.Tree]: T = Convert.toMTreeUntpd[T](tree)
  }

  // make toMTreeUntpd type check
  private implicit class TypeAdaper(tree: m.Tree) {
    def as[T <: m.Tree]: T = tree.asInstanceOf[T]
  }

  def toMTreeUntpd[T <: m.Tree](tree: untpd.Tree): T = (tree match {
    // ============ LITERALS ============
    case u.Literal(v) =>
      m.Lit(v)

    // ============ TERMS ============
    case u.TermIdent(name) =>
      m.Term.Name(name.toString)

    case u.TermApply(fun, args) =>
      val mfun = fun.toMTree[m.Term]
      val margs = args.map(toMTreeUntpd[m.Term.Arg])
      m.Term.Apply(mfun, margs)

    // ============ TYPES ============

    // ============ PATTERNS ============

    // ============ DECLS ============

    // ============ DEFNS ============

    // ============ PKGS ============

    // ============ CTORS ============

    // ============ TEMPLATES ============

    // ============ MODIFIERS ============
    case _ => ???
  }).as[T]

  def toMTreeTpd(tree: tpd.Tree): m.Tree = ???
}

