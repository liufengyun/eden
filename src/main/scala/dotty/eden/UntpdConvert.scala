package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{ untpd => d }
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Flags._

class UntpdConvert(initialMode: Mode, initialLoc: Loc) {
  val u = new UntpdMapping(initialMode, initialLoc)

  // add .toMTree to untyped trees
  private implicit class TreeWrapper(tree: d.Tree) {
    def toMTree[T <: m.Tree](implicit ctx: Context): T = UntpdConvert.this.toMTree[T](tree)
  }

  def toMTree[T <: m.Tree](tree: d.Tree)(implicit ctx: Context): T = (tree match {
    // ============ LITERALS ============
    case u.Literal(v) =>
      m.Lit(v)

    // ============ TERMS ============
    case t: d.This =>
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

    case u.TermNew(ctor, args) => // important to before TermApply
      val mctor = u.withLoc(SuperCallLoc) { ctor.toMTree[m.Term] }
      val margs = u.withLoc(ExprLoc) { args.map(toMTree[m.Term.Arg]) }
      m.Term.New(m.Template(Nil, List(m.Term.Apply(mctor, margs)), m.Term.Param(Nil, m.Name.Anonymous(), None, None), None))

    case t: d.New =>
      m.Term.New(t.tpt.toMTree[m.Template])

    case u.TermApply(fun, args) =>
      val mfun = fun.toMTree[m.Term]
      val margs = args.map(toMTree[m.Term.Arg])
      m.Term.Apply(mfun, margs)

    case t: d.NamedArg =>
      val mname = m.Term.Name(t.name.show)
      val narg  = t.arg.toMTree[m.Term]
      m.Term.Arg.Named(mname, narg)

    case u.RepeatedParam(name) =>
      val mterm = m.Term.Name(name.show)
      m.Term.Arg.Repeated(mterm)

    case u.TermTypeApply(fun, args) =>
      val mfun = u.withMode(TermMode) { fun.toMTree[m.Term] }
      val mtargs = u.withMode(TypeMode) { args.map(toMTree[m.Type]) }
      m.Term.ApplyType(mfun, mtargs)

    case u.TermInfixOp(left, op, right) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Term]
      val mright = right.toMTree[m.Term]
      m.Term.ApplyInfix(mleft, mop, Nil, List(mright))

    case u.TermPostfixOp(left, op) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Term]
      m.Term.Select(mleft, mop)

    case t: d.Assign =>
      val mlhs = t.lhs.toMTree[m.Term.Ref]
      val mrhs = t.rhs.toMTree[m.Term]
      m.Term.Assign(mlhs, mrhs)

    case t: d.Block =>
      val mstats = (t.stats :+ t.expr).filterNot(_.isEmpty).map(toMTree[m.Stat])
      mstats match {
        case Seq(stat) => stat
        case _ => m.Term.Block (mstats)
      }

    case t: d.If =>
      val mcond = t.cond.toMTree[m.Term]
      val mthen = t.thenp.toMTree[m.Term]
      val melse = t.elsep.toMTree[m.Term]
      m.Term.If(mcond, mthen, melse)

    case t: d.Parens =>
      t.forwardTo.toMTree

    case t: d.WhileDo =>
      val mcond = t.cond.toMTree[m.Term]
      val mbody = t.body.toMTree[m.Term]
      m.Term.While(mcond, mbody)

    case t: d.DoWhile =>
      val mcond = t.cond.toMTree[m.Term]
      val mbody = t.body.toMTree[m.Term]
      m.Term.Do(mbody, mcond)

    // ============ TYPES ============
    case u.TypeIdent(name) =>
      m.Type.Name(name.show)

    case t: d.AppliedTypeTree =>
      val mtpt = u.withMode(TypeMode) { t.tpt.toMTree[m.Type] }
      val margs = u.withMode(TypeMode) { t.args.map(toMTree[m.Type]) }
      m.Type.Apply(mtpt, margs)

    case u.TypeInfixOp(lhs, op, rhs) =>
      val mop = m.Type.Name(op.show)
      val mlhs = u.withs(TypeMode, ExprLoc) { lhs.toMTree[m.Type] }
      val mrhs = u.withs(TypeMode, ExprLoc) { rhs.toMTree[m.Type] }
      m.Type.ApplyInfix(mlhs, mop, mrhs)

    // ============ PATTERNS ============
    case t: d.Match =>
      val mscrut = t.selector.toMTree[m.Term]
      val mcases = t.cases.map(toMTree[m.Case])
      m.Term.Match(mscrut, mcases)

    case t: d.CaseDef =>
      val mpat = u.withLoc(PatLoc) { t.pat.toMTree[m.Pat] }
      val mguard = if (t.guard.isEmpty) None else Some(t.guard.toMTree[m.Term])
      val mbody = t.body.toMTree[m.Term]
      m.Case(mpat, mguard, mbody)

    case u.PatVarIdent(name) =>
      if (name.show.charAt(0).isUpper)
        m.Term.Name(name.show)
      else
        m.Pat.Var.Term(m.Term.Name(name.show))

    case u.PatExtract(ref, targs, args) =>
      val mref = u.withMode(TermMode) { ref.toMTree[m.Term.Ref] }
      val mtargs = targs.map(toMTree[m.Pat.Type])
      val margs = args.map(toMTree[m.Pat.Arg])
      m.Pat.Extract(mref, mtargs, margs)

    case u.PatWildcard() =>
      m.Pat.Wildcard()

    case u.PatBind(name, body) =>
      val mlhs = m.Pat.Var.Term(m.Term.Name(name.show))
      val mrhs = body.toMTree[m.Pat.Arg]
      m.Pat.Bind(mlhs, mrhs)

    case u.PatTyped(llhs, lrhs) =>
      val mlrhs = llhs.toMTree[m.Pat]
      val mrhs = u.withMode(TypeMode) { lrhs.toMTree[m.Pat.Type] } // dveim replaced
      m.Pat.Typed(mlrhs, mrhs)

    case t: d.Alternative =>
      // must have one alternative
      val pats:+mpat = t.trees.map(toMTree[m.Pat])
      pats.foldRight(mpat) { (pat, alt) =>
        m.Pat.Alternative(pat, alt)
      }

    // ============ DECLS ============
    case u.VarDcl(modifiers, name, tpt) =>
      val mtpt = u.withMode(TypeMode) { tpt.toMTree[m.Type] }
      m.Decl.Var(Nil, List(m.Pat.Var.Term(m.Term.Name(name.show))), mtpt)

    case u.ValDcl(modifiers, name, tpt) =>
      val mtpt = u.withMode(TypeMode) { tpt.toMTree[m.Type] }
      m.Decl.Val(Nil, List(m.Pat.Var.Term(m.Term.Name(name.show))), mtpt)

    case t: d.DefDef if t.rhs.isEmpty =>
      val mname = m.Term.Name(t.name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { t.tparams.map(toMTree[m.Type.Param]) }
      val mvparams = u.withs(TermMode, ParamLoc) { t.vparamss.map(_.map(toMTree[m.Term.Param]))}
      val mtpt = u.withMode(TypeMode) { t.tpt.toMTree[m.Type] }
      m.Decl.Def(Nil, mname, mtparams, mvparams, mtpt)

    case t: d.PatDef if t.rhs.isEmpty =>
      val mpats = u.withLoc(PatLoc) { t.pats.map(toMTree[m.Pat.Var.Term]) }
      val mtpt = u.withMode(TypeMode) { t.tpt.toMTree[m.Type] }

      if (t.mods.flags.is(Mutable))
        m.Decl.Var(Nil, mpats, mtpt)
      else
        m.Decl.Val(Nil, mpats, mtpt)

    case u.TypeDcl(modifiers, name, tparams, bounds) =>
      val mname = m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val mbounds = u.withs(TypeMode, ExprLoc) { bounds.toMTree[m.Type.Bounds] }
      m.Decl.Type(Nil, mname, mtparams, mbounds)


    // ============ DEFNS ============
    case u.ValDef(modifiers, name, tpt, rhs) =>
      val mrhs = u.withMode(TermMode) { rhs.toMTree[m.Term] }
      val mtpt = u.withMode(TypeMode) { tpt.map(toMTree[m.Type]) }
      m.Defn.Val(Nil, List(m.Pat.Var.Term(m.Term.Name(name.show))), mtpt, mrhs)

    case u.VarDef(modifiers, name, tpt, rhs) =>
      val mrhs = if (rhs.isEmpty) None else u.withMode(TermMode) { Some(rhs.toMTree[m.Term]) }
      val mtpt = u.withMode(TypeMode) { tpt.map(toMTree[m.Type]) }
      m.Defn.Var(Nil, List(m.Pat.Var.Term(m.Term.Name(name.show))), mtpt, mrhs)

    case t: d.PatDef if !t.rhs.isEmpty =>
      val mpats = u.withLoc(PatLoc) { t.pats.map(toMTree[m.Pat]) }
      val mtpt = if (t.tpt.isEmpty) None else u.withMode(TypeMode) { Some(t.tpt.toMTree[m.Type]) }
      val mrhs = u.withMode(TermMode) { t.rhs.toMTree[m.Term] }

      if (t.mods.flags.is(Mutable))
        m.Defn.Var(Nil, mpats, mtpt, Some(mrhs))
      else
        m.Defn.Val(Nil, mpats, mtpt, mrhs)

    case t: d.DefDef if !t.rhs.isEmpty =>
      val mname = m.Term.Name(t.name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { t.tparams.map(toMTree[m.Type.Param]) }
      val mvparams = u.withs(TermMode, ParamLoc) { t.vparamss.map(_.map(toMTree[m.Term.Param]))}
      val mtpt = if (t.tpt.isEmpty) None else Some(u.withMode(TypeMode) { t.tpt.toMTree[m.Type] })
      val mrhs = u.withs(TermMode, ExprLoc) { t.rhs.toMTree[m.Term] }
      m.Defn.Def(Nil, mname, mtparams, mvparams, mtpt, mrhs)

    case u.ParamTerm(modifiers, name, tpt, default) =>
      val mname = if (name == nme.WILDCARD) m.Name.Anonymous() else m.Term.Name(name.show)
      val mrhs = u.withs(TermMode, ExprLoc) { default.map(toMTree[m.Term]) }
      val mtpt = u.withMode(TypeMode) { tpt.map(toMTree[m.Type]) }
      m.Term.Param(Nil, mname, mtpt, mrhs)

    case t: d.TypeBoundsTree =>
      val mlo = if (t.lo.isEmpty) None else Some(u.withMode(TypeMode) { t.lo.toMTree[m.Type] })
      val mhi = if (t.hi.isEmpty) None else Some(u.withMode(TypeMode) { t.hi.toMTree[m.Type] })
      m.Type.Bounds(mlo, mhi)

    case u.ParamType(modifiers, name, tparams, bounds, ctxbounds) =>
      val mname = if (name.startsWith(nme.USCORE_PARAM_PREFIX)) m.Name.Anonymous() else m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val mbounds = u.withMode(TypeMode) { bounds.toMTree[m.Type.Bounds] }
      val mctxbounds = u.withs(TypeMode, ExprLoc) { ctxbounds.map(toMTree[m.Type]) }
      val mmods =
        if (modifiers.flags.is(Covariant)) List(m.Mod.Covariant())
        else if (modifiers.flags.is(Contravariant)) List(m.Mod.Contravariant())
        else Nil

      m.Type.Param(mmods, mname, mtparams, mbounds, Nil/*view bounds*/, mctxbounds)

    case u.TypeDef(modifiers, name, tparams, rhs) =>        // important to be after ParamType
      val mname = m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val mrhs = u.withs(TypeMode, ExprLoc) { rhs.toMTree[m.Type] }
      m.Defn.Type(Nil, mname, mtparams, mrhs)

    case t: d.Template =>
      val mparents = u.withs(TermMode, SuperCallLoc) { t.parents.map(toMTree[m.Ctor.Call]) }
      val mself = u.withs(TermMode, ParamLoc) { t.self.toMTree[m.Term.Param] }
      val mstats = t.body match {
        case Nil => None
        case List(d.EmptyTree) => Some(Nil)
        case l => Some(l.map(toMTree[m.Stat]))
      }
      m.Template(Nil, mparents, mself, mstats)

    case u.ClassDef(modifiers, name, tparams, templ) =>
      val mname = m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val ctorparams = u.withs(TermMode, ParamLoc) { templ.constr.vparamss.map(_.map(toMTree[m.Term.Param])) }
      val mctor = m.Ctor.Primary(Nil, m.Ctor.Name("this"), ctorparams)
      val mtempl = templ.toMTree[m.Template]
      if (modifiers.is(Trait))
        m.Defn.Trait(Nil, mname, mtparams, mctor, mtempl)
      else
        m.Defn.Class(Nil, mname, mtparams, mctor, mtempl)

    case u.SuperCall(ctor, args) =>
      val mctor = u.withLoc(SuperCallLoc) { ctor.toMTree[m.Term] }
      val margs = u.withLoc(ExprLoc) { args.map(toMTree[m.Term.Arg]) }
      m.Term.Apply(mctor, margs)

    // ============ PKGS ============

    // ============ CTORS ============
    case u.CtorName(name) =>
      m.Ctor.Name(name.show)

    // ============ TEMPLATES ============

    // ============ MODIFIERS ============
    case _ => println(tree); ???
  }).asInstanceOf[T]

}
