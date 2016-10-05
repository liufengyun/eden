package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts.Context
import dotty.eden.{UntpdMapping => u}
import dotty.tools.dotc.core.StdNames._

object Convert {
  // add .toMTree to untyped trees
  private implicit class UntpdTreeWrapper(tree: untpd.Tree) {
    def toMTree[T <: m.Tree](implicit ctx: Context): T = Convert.toMTreeUntpd[T](tree)
  }

  // make toMTreeUntpd type check
  private implicit class TypeAdaper(tree: m.Tree) {
    def as[T <: m.Tree]: T = tree.asInstanceOf[T]
  }

  def toMTreeUntpd[T <: m.Tree](tree: untpd.Tree)(implicit ctx: Context): T = (tree match {
    // ============ LITERALS ============
    case u.Literal(v) =>
      m.Lit(v)

    // ============ TERMS ============
    case t: untpd.This =>
      if (t.qual == tpnme.EMPTY)
        m.Term.This(m.Name.Anonymous())
      else
        m.Term.This(m.Name.Indeterminate(t.qual.show))

    case u.TermIdent(name) =>
      m.Term.Name(name.show)

    case u.TermSelect(pre, name) =>
      val mpre = pre.toMTree[m.Term]
      val mname = m.Term.Name(name.show)
      m.Term.Select(mpre, mname)

    case u.TermApply(fun, args) =>
      val mfun = fun.toMTree[m.Term]
      val margs = args.map(toMTreeUntpd[m.Term.Arg])
      m.Term.Apply(mfun, margs)

    case t: untpd.TypeApply =>
      val mfun = t.fun.toMTree[m.Term]
      val mtargs = t.args.map(toMTreeUntpd[m.Type])
      m.Term.ApplyType(mfun, mtargs)


    case u.TermInfixOp(left, op, right) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Term]
      val mright = right.toMTree[m.Term]
      m.Term.ApplyInfix(mleft, mop, Nil, List(mright))

    // ============ TYPES ============
    case u.TypeIdent(name) =>
      m.Type.Name(name.show)

    // ============ PATTERNS ============

    // ============ DECLS ============

    // ============ DEFNS ============

    // ============ PKGS ============

    // ============ CTORS ============

    // ============ TEMPLATES ============

    // ============ MODIFIERS ============
    case _ => println(tree); ???
  }).as[T]

  def toMTreeTpd(tree: tpd.Tree): m.Tree = ???
}

