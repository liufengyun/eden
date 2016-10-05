package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._

class UntpdConvert(initialMode: Loc) {
  val u = new UntpdMapping(initialMode)

  // add .toMTree to untyped trees
  private implicit class TreeWrapper(tree: untpd.Tree) {
    def toMTree[T <: m.Tree](implicit ctx: Context): T = UntpdConvert.this.toMTree[T](tree)
  }

  def toMTree[T <: m.Tree](tree: untpd.Tree)(implicit ctx: Context): T = (tree match {
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
      val margs = args.map(toMTree[m.Term.Arg])
      m.Term.Apply(mfun, margs)

    case t: untpd.TypeApply =>
      val mfun = t.fun.toMTree[m.Term]
      val mtargs = t.args.map(toMTree[m.Type])
      m.Term.ApplyType(mfun, mtargs)

    case u.TermInfixOp(left, op, right) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Term]
      val mright = right.toMTree[m.Term]
      m.Term.ApplyInfix(mleft, mop, Nil, List(mright))

    case t: untpd.Assign =>
      val mlhs = t.lhs.toMTree[m.Term.Ref]
      val mrhs = t.rhs.toMTree[m.Term]
      m.Term.Assign(mlhs, mrhs)

    case t: untpd.Block =>
      val mstats = (t.stats :+ t.expr).filterNot(_.isEmpty).map(toMTree[m.Stat])
      mstats match {
        case Seq(stat) => stat
        case _ => m.Term.Block (mstats)
      }

    case t: untpd.If =>
      val mcond = t.cond.toMTree[m.Term]
      val mthen = t.thenp.toMTree[m.Term]
      val melse = t.elsep.toMTree[m.Term]
      m.Term.If(mcond, mthen, melse)

    case t: untpd.Parens =>
      t.forwardTo.toMTree

    // ============ TYPES ============
    case u.TypeIdent(name) =>
      m.Type.Name(name.show)

    // ============ PATTERNS ============
    case t: untpd.Match =>
      val mscrut = t.selector.toMTree[m.Term]
      val mcases = t.cases.map(toMTree[m.Case])
      m.Term.Match(mscrut, mcases)

    case t: untpd.CaseDef =>
      val mpat = u.withMode(PatLoc) { t.pat.toMTree[m.Pat] }
      val mguard = if (t.guard.isEmpty) None else Some(t.guard.toMTree[m.Term])
      val mbody = t.body.toMTree[m.Term]
      m.Case(mpat, mguard, mbody)

    case u.PatVarIdent(name) =>
      if (name.show.charAt(0).isUpper)
        m.Term.Name(name.show)
      else
        m.Pat.Var.Term(m.Term.Name(name.show))

    case u.PatExtract(ref, targs, args) =>
      val mref = u.withMode(TermLoc) { ref.toMTree[m.Term.Ref] }
      val mtargs = targs.map(toMTree[m.Pat.Type])
      val margs = args.map(toMTree[m.Pat.Arg])
      m.Pat.Extract(mref, mtargs, margs)

    case u.PatWildcard() =>
      m.Pat.Wildcard()

    case u.PatBind(name, body) =>
      val mlhs= m.Pat.Var.Term(m.Term.Name(name.show))
      val mrhs = body.toMTree[m.Pat.Arg]
      m.Pat.Bind(mlhs, mrhs)

    case u.PatTyped(llhs, lrhs) =>
      val mlrhs = llhs.toMTree[m.Pat]
      val mrhs = lrhs.toMTree[m.Pat.Type] // dveim replaced
      m.Pat.Typed(mlrhs, mrhs)

    // ============ DECLS ============

    // ============ DEFNS ============

    // ============ PKGS ============

    // ============ CTORS ============

    // ============ TEMPLATES ============

    // ============ MODIFIERS ============
    case _ => println(tree); ???
  }).asInstanceOf[T]

}
