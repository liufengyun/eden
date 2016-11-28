package dotty.eden

import scala.{meta => m}
import dotty.tools.dotc.ast.{ untpd => d }
import dotty.tools.dotc.ast.untpd.modsDeco
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Flags._
import scala.collection.mutable.ListBuffer

class UntpdConvert(initialMode: Mode, initialLoc: Loc)(implicit ctx: Context) {
  val u = new UntpdMapping(initialMode, initialLoc)

  // add .toMTree to untyped trees
  private implicit class TreeWrapper(tree: d.Tree) {
    def toMTree[T <: m.Tree]: T = UntpdConvert.this.toMTree[T](tree)
  }

  private def toEnums(enums: List[d.Tree]): List[m.Enumerator] = enums.map {
    case t: d.GenFrom =>
      val mpat = u.withs(TermMode, PatLoc) { t.pat.toMTree[m.Pat] }
      val mexpr = u.withs(TermMode, ExprLoc) { t.expr.toMTree[m.Term] }
      m.Enumerator.Generator(mpat, mexpr)
    case t: d.GenAlias =>
      val mpat = u.withs(TermMode, PatLoc) { t.pat.toMTree[m.Pat] }
      val mexpr = u.withs(TermMode, ExprLoc) { t.expr.toMTree[m.Term] }
      m.Enumerator.Val(mpat, mexpr)
    case t =>
      val expr = u.withs(TermMode, ExprLoc) { t.toMTree[m.Term] }
      m.Enumerator.Guard(expr)
  }

  private implicit def toMods(modifiers: d.Modifiers): List[m.Mod] = {
    val lb = new ListBuffer[m.Mod]

    if (modifiers.hasAnnotations) {
      lb ++= modifiers.annotations.map { annot =>
        m.Mod.Annot(u.withs(TermMode, SuperCallLoc) { annot.toMTree[m.Ctor.Call] })
      }
    }

    import d.{Mod => mod}
    lb ++= modifiers.mods.map {
      case _: mod.Override =>
        m.Mod.Override()
      case _: mod.Abstract =>
        m.Mod.Abstract()
      case _: mod.Final    =>
        m.Mod.Final()
      case _: mod.Implicit =>
        m.Mod.Implicit()
      case _: mod.Inline   =>
        m.Mod.Inline()
      case _: mod.Lazy     =>
        m.Mod.Lazy()
      case _: mod.Private  =>
        if (modifiers.hasPrivateWithin)
          m.Mod.Private(m.Name.Indeterminate(modifiers.privateWithin.show))
        else if (modifiers is Local)
          m.Mod.Private(m.Term.This(m.Name.Anonymous()))
        else
          m.Mod.Private(m.Name.Anonymous())
      case _: mod.Protected=>
        if (modifiers.hasPrivateWithin)
          m.Mod.Protected(m.Name.Indeterminate(modifiers.privateWithin.show))
        else if (modifiers is Local)
          m.Mod.Protected(m.Term.This(m.Name.Anonymous()))
        else
          m.Mod.Protected(m.Name.Anonymous())
      case _: mod.Sealed   =>
        m.Mod.Sealed()
      case _: mod.Type     =>
        ???
      case _: mod.Val if u.loc == ParamLoc   =>
        m.Mod.ValParam()
      case _: mod.Var if u.loc == ParamLoc   =>
        m.Mod.VarParam()
      case _ => null
    }.filter(_ != null)

    if (modifiers.is(Covariant)) lb += m.Mod.Covariant()
    if (modifiers.is(Contravariant)) lb += m.Mod.Contravariant()

    lb.toList
  }

  def toMTree[T <: m.Tree](tree: d.Tree): T = (tree match {
    // ============ LITERALS ============
    case u.Literal(v) =>
      m.Lit(v)

    // ============ TERMS ============
    case t: d.This =>
      if (t.qual.name == tpnme.EMPTY)
        m.Term.This(m.Name.Anonymous())
      else
        m.Term.This(m.Name.Indeterminate(t.qual.name.show))

    case t: d.Super =>
      val mmix = if (t.mix.isEmpty)
        m.Name.Anonymous()
      else
        m.Name.Indeterminate(t.mix.show)

      val qual = t.qual.asInstanceOf[d.This].qual
      val mqual = if (qual.isEmpty)
        m.Name.Anonymous()
      else
        m.Name.Indeterminate(qual.name.show)

      m.Term.Super(mqual, mmix)

    case u.TermIdent(name) =>
      if (name.startsWith(nme.USCORE_PARAM_PREFIX)) m.Term.Placeholder()
      else if (name == nme.CONSTRUCTOR) m.Ctor.Name("this")
      else m.Term.Name(name.show)

    case u.TermSelect(pre, name) =>
      val mpre = u.withLoc(ExprLoc) { pre.toMTree[m.Term] }
      val mname = m.Term.Name(name.show)
      m.Term.Select(mpre, mname)

    case u.TermNew(ctor, args) => // important to before TermApply
      val mctor = u.withLoc(SuperCallLoc) { ctor.toMTree[m.Ctor.Call] }
      val margs = u.withLoc(ExprLoc) { args.map(toMTree[m.Term.Arg]) }
      if (margs.isEmpty)
        m.Term.New(
          m.Template(Nil, List(mctor),
            m.Term.Param(Nil, m.Name.Anonymous(), None, None), None))
      else
        m.Term.New(
          m.Template(Nil, List(m.Term.Apply(mctor, margs)),
            m.Term.Param(Nil, m.Name.Anonymous(), None, None), None))

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

    case u.TermRepeated(tree) =>
      val mtree = tree.toMTree[m.Term]
      m.Term.Arg.Repeated(mtree)

    case u.TermTypeApply(fun, args) =>
      val mfun = u.withMode(TermMode) { fun.toMTree[m.Term] }
      val mtargs = u.withMode(TypeMode) { args.map(toMTree[m.Type]) }
      m.Term.ApplyType(mfun, mtargs)

    case u.TermInfixOp(left, op, args) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Term]
      val mright = args.map(toMTree[m.Term])
      m.Term.ApplyInfix(mleft, mop, Nil, mright)

    case u.TermPostfixOp(left, op) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Term]
      m.Term.Select(mleft, mop)

    case u.TermPrefixOp(op, right) =>
      val mop = m.Term.Name(op.show)
      val mright = right.toMTree[m.Term]
      m.Term.ApplyUnary(mop, mright)

    case u.TermEta(tree) =>
      val mtree = tree.toMTree[m.Term]
      m.Term.Eta(mtree)

    case u.TermTyped(expr, tpt) =>
      val mexpr = expr.toMTree[m.Term]
      val mtpt = u.withMode(TypeMode) { tpt.toMTree[m.Type] }
      m.Term.Ascribe(mexpr, mtpt)

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
      val melse = if (t.elsep.isEmpty) m.Lit() else t.elsep.toMTree[m.Term]
      m.Term.If(mcond, mthen, melse)

    case t: d.Parens =>
      t.forwardTo.toMTree[m.Tree]

    case t: d.WhileDo =>
      val mcond = t.cond.toMTree[m.Term]
      val mbody = t.body.toMTree[m.Term]
      m.Term.While(mcond, mbody)

    case t: d.DoWhile =>
      val mcond = t.cond.toMTree[m.Term]
      val mbody = t.body.toMTree[m.Term]
      m.Term.Do(mbody, mcond)

    case t: d.Return =>
      val mexpr = u.withs(TermMode, ExprLoc) { t.expr.toMTree[m.Term] }
      m.Term.Return(mexpr)

    case t: d.Throw =>
      val mexpr = u.withs(TermMode, ExprLoc) { t.expr.toMTree[m.Term] }
      m.Term.Throw(mexpr)

    case t: d.ParsedTry =>
      // keep blocks -- don't optimize for one-statement blocks
      def transform(t: d.Tree) = t match {
        case t: d.Block =>
          val stats = (t.stats :+ t.expr).filterNot(_.isEmpty).map(toMTree[m.Stat])
          m.Term.Block(stats)
        case _ => t.toMTree[m.Term]
      }

      val mexpr = u.withs(TermMode, ExprLoc) { transform(t.expr) }
      val mfinalizer = if (t.finalizer.isEmpty) None else Some(transform(t.finalizer))

      if (t.handler.isEmpty)
        m.Term.TryWithCases(mexpr, Nil, mfinalizer)
      else if (t.handler.isInstanceOf[d.Match]) {
        val cases =  t.handler.asInstanceOf[d.Match].cases
        val mcases = cases.map(toMTree[m.Case])
        m.Term.TryWithCases(mexpr, mcases, mfinalizer)
      }
      else {
        val term = t.handler.toMTree[m.Term]
        m.Term.TryWithTerm(mexpr, term, mfinalizer)
      }

    case t: d.ForDo =>
      val menums = toEnums(t.enums)
      val mbody = t.body.toMTree[m.Term]
      m.Term.For(menums, mbody)

    case t: d.ForYield =>
      val menums = toEnums(t.enums)
      val mbody = t.expr.toMTree[m.Term]
      m.Term.ForYield(menums, mbody)

    case t: d.Tuple if u.mode == TermMode && u.loc == ExprLoc =>
      val mtrees = t.trees.map(toMTree[m.Term])
      if (mtrees.isEmpty)
        m.Lit(())
      else
        m.Term.Tuple(mtrees)

    case u.WildcardFunction(body) =>
      body.toMTree[m.Term]

    case u.Function(fun) =>
      val margs = u.withs(TermMode, ParamLoc) { fun.args.map(toMTree[m.Term.Param]) }
      val mbody = u.withs(TermMode, ExprLoc) { fun.body.toMTree[m.Term] }
      m.Term.Function(margs, mbody)

    case u.TermInterpolate(id, lits, args) =>
      val margs = u.withs(TermMode, ExprLoc) { args.map(toMTree[m.Term]) }
      m.Term.Interpolate(m.Term.Name(id.show), lits.map(m.Lit(_)), margs)

    // ============ TYPES ============
    case u.TypeIdent(name) =>
      m.Type.Name(name.show)

    case u.TypeApply(tpt, args) =>
      val mtpt = u.withMode(TypeMode) { tpt.toMTree[m.Type] }
      val margs = u.withMode(TypeMode) { args.map(toMTree[m.Type]) }
      m.Type.Apply(mtpt, margs)

    case u.TypeInfixOp(lhs, op, rhs) =>
      val mop = m.Type.Name(op.show)
      val mlhs = u.withs(TypeMode, ExprLoc) { lhs.toMTree[m.Type] }
      val mrhs = u.withs(TypeMode, ExprLoc) { rhs.toMTree[m.Type] }
      m.Type.ApplyInfix(mlhs, mop, mrhs)

    case t: d.ByNameTypeTree =>
      val mtpt = u.withMode(TypeMode) { t.result.toMTree[m.Type] }
      m.Type.Arg.ByName(mtpt)

    case t: d.RefinedTypeTree =>
      val mtpt = Some(t.tpt.toMTree[m.Type])
      val mrefinements = t.refinements.map(toMTree[m.Stat])
      m.Type.Refine(mtpt, mrefinements)

    case u.TypeSelect(pre, name) =>
      val mpre = u.withs(TermMode, ExprLoc) { pre.toMTree[m.Term.Ref] }
      val mname = m.Type.Name(name.show)
      m.Type.Select(mpre, mname)

    case u.TypeProject(pre, name) =>
      val mpre = u.withs(TypeMode, ExprLoc) { pre.toMTree[m.Type] }
      val mname = m.Type.Name(name.show)
      m.Type.Project(mpre, mname)

    case u.TypeFunction(tparams, tret) =>
      val mtparams = tparams.map(toMTree[m.Type])
      val mtret = tret.toMTree[m.Type]
      m.Type.Function(mtparams, mtret)

    case t: d.SingletonTypeTree =>
      val mref = u.withs(TermMode, ExprLoc) { t.ref.toMTree[m.Term.Ref] }
      m.Type.Singleton(mref)

    case u.TypeOr(lhs, rhs) =>
      val mlhs = u.withMode(TypeMode) { lhs.toMTree[m.Type] }
      val mrhs = u.withMode(TypeMode) { rhs.toMTree[m.Type] }
      m.Type.Or(mlhs, mrhs)

    case u.TypeAnd(lhs, rhs) =>
      val mlhs = u.withMode(TypeMode) { lhs.toMTree[m.Type] }
      val mrhs = u.withMode(TypeMode) { rhs.toMTree[m.Type] }
      m.Type.And(mlhs, mrhs)

    case u.TypeTuple(tpts) =>
      val mtypes = tpts.map(toMTree[m.Type])
      m.Type.Tuple(mtypes)

    case u.TypeBounds(lo, hi) =>
      val mlo = u.withMode(TypeMode) { lo.map(toMTree[m.Type]) }
      val mhi = u.withMode(TypeMode) { hi.map(toMTree[m.Type]) }
      m.Type.Bounds(mlo, mhi)

    case u.TypeRepeated(tp) =>
      val mtype = tp.toMTree[m.Type]
      m.Type.Arg.Repeated(mtype)

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
      val mtargs = u.withMode(TypeMode) { targs.map(toMTree[m.Pat.Type]) }
      val margs = args.map(toMTree[m.Pat.Arg])
      m.Pat.Extract(mref, mtargs, margs)

    case u.PatInfixExtract(left, op, args) =>
      val mop = m.Term.Name(op.show)
      val mleft = left.toMTree[m.Pat]
      val margs = args.map(toMTree[m.Pat.Arg])
      m.Pat.ExtractInfix(mleft, mop, margs)

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

    case t: d.Tuple if u.mode == TermMode && u.loc == PatLoc =>
      val mtrees = t.trees.map(toMTree[m.Pat])
      if (mtrees.isEmpty)
        m.Lit(())
      else
        m.Pat.Tuple(mtrees)

    case u.PatInterpolate(id, lits, args) =>
      val margs = u.withs(TermMode, PatLoc) { args.map(toMTree[m.Pat]) }
      m.Pat.Interpolate(m.Term.Name(id.show), lits.map(m.Lit(_)), margs)

    // pat types

    case u.PatTypeApply(tpt, args) =>
      val mtpt = tpt.toMTree[m.Pat.Type]
      val margs = args.map(toMTree[m.Pat.Type])
      m.Pat.Type.Apply(mtpt, margs)

    case u.PatTypeWildcard() =>
      m.Pat.Type.Wildcard()

    case u.PatTypeInfixOp(lhs, op, rhs) =>
      val mop = m.Type.Name(op.show)
      val mlhs = lhs.toMTree[m.Pat.Type]
      val mrhs = rhs.toMTree[m.Pat.Type]
      m.Pat.Type.ApplyInfix(mlhs, mop, mrhs)

    case u.PatTypeProject(pre, name) =>
      val mpre = pre.toMTree[m.Pat.Type]
      val mname = m.Type.Name(name.show)
      m.Pat.Type.Project(mpre, mname)

    case u.PatTypeFunction(tparams, tret) =>
      val mtparams = tparams.map(toMTree[m.Pat.Type])
      val mtret = tret.toMTree[m.Pat.Type]
      m.Pat.Type.Function(mtparams, mtret)

    case u.PatTypeOr(lhs, rhs) =>
      val mlhs = lhs.toMTree[m.Pat.Type]
      val mrhs = rhs.toMTree[m.Pat.Type]
      m.Pat.Type.Or(mlhs, mrhs)

    case u.PatTypeAnd(lhs, rhs) =>
      val mlhs = lhs.toMTree[m.Pat.Type]
      val mrhs = rhs.toMTree[m.Pat.Type]
      m.Pat.Type.And(mlhs, mrhs)

    case u.PatTypeTuple(tpts) =>
      val mtypes = tpts.map(toMTree[m.Pat.Type])
      m.Pat.Type.Tuple(mtypes)

    // ============ DECLS ============
    case u.VarDcl(modifiers, name, tpt) =>
      val mtpt = u.withMode(TypeMode) { tpt.toMTree[m.Type] }
      val mname = u.withLoc(PatLoc) { d.Ident(name).toMTree[m.Pat.Var.Term] }
      m.Decl.Var(modifiers, List(mname), mtpt)

    case u.ValDcl(modifiers, name, tpt) =>
      val mtpt = u.withMode(TypeMode) { tpt.toMTree[m.Type] }
      val mname = u.withLoc(PatLoc) { d.Ident(name).toMTree[m.Pat.Var.Term] }
      m.Decl.Val(modifiers, List(mname), mtpt)

    case t: d.DefDef if t.rhs.isEmpty =>
      val mname = m.Term.Name(t.name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { t.tparams.map(toMTree[m.Type.Param]) }
      val mvparams = u.withs(TermMode, ParamLoc) { t.vparamss.map(_.map(toMTree[m.Term.Param]))}
      val mtpt = u.withMode(TypeMode) { t.tpt.toMTree[m.Type] }
      m.Decl.Def(t.mods, mname, mtparams, mvparams, mtpt)

    case t: d.PatDef if t.rhs.isEmpty =>
      val mpats = u.withLoc(PatLoc) { t.pats.map(toMTree[m.Pat.Var.Term]) }
      val mtpt = u.withMode(TypeMode) { t.tpt.toMTree[m.Type] }

      if (t.mods.is(Mutable))
        m.Decl.Var(t.mods, mpats, mtpt)
      else
        m.Decl.Val(t.mods, mpats, mtpt)

    case u.TypeDcl(modifiers, name, tparams, bounds) =>
      val mname = m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val mbounds = u.withs(TypeMode, ExprLoc) { bounds.toMTree[m.Type.Bounds] }
      m.Decl.Type(Nil, mname, mtparams, mbounds)


    // ============ DEFNS ============
    case u.ValDef(modifiers, name, tpt, rhs) =>
      val mrhs = u.withMode(TermMode) { rhs.toMTree[m.Term] }
      val mtpt = u.withMode(TypeMode) { tpt.map(toMTree[m.Type]) }
      val mname = u.withLoc(PatLoc) { d.Ident(name).toMTree[m.Pat] }
      m.Defn.Val(modifiers, List(mname), mtpt, mrhs)

    case u.VarDef(modifiers, name, tpt, rhs) =>
      val mname = u.withLoc(PatLoc) { d.Ident(name).toMTree[m.Pat] }
      val mrhs = if (rhs.isInstanceOf[d.Ident] && rhs.asInstanceOf[d.Ident].name == nme.WILDCARD)
        None
      else
        u.withMode(TermMode) { Some(rhs.toMTree[m.Term]) }

      val mtpt = u.withMode(TypeMode) { tpt.map(toMTree[m.Type]) }
      m.Defn.Var(modifiers, List(mname), mtpt, mrhs)

    case t: d.PatDef if !t.rhs.isEmpty =>
      val mpats = u.withLoc(PatLoc) { t.pats.map(toMTree[m.Pat]) }
      val mtpt = if (t.tpt.isEmpty) None else u.withMode(TypeMode) { Some(t.tpt.toMTree[m.Type]) }
      val mrhs = u.withMode(TermMode) { t.rhs.toMTree[m.Term] }

      if (t.mods.is(Mutable))
        m.Defn.Var(t.mods, mpats, mtpt, Some(mrhs))
      else
        m.Defn.Val(t.mods, mpats, mtpt, mrhs)

    case t: d.DefDef if !t.rhs.isEmpty && t.name != nme.CONSTRUCTOR =>
      val mname = m.Term.Name(t.name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { t.tparams.map(toMTree[m.Type.Param]) }
      val mvparams = u.withs(TermMode, ParamLoc) { t.vparamss.map(_.map(toMTree[m.Term.Param]))}
      val mtpt = if (t.tpt.isEmpty) None else Some(u.withMode(TypeMode) { t.tpt.toMTree[m.Type] })
      val mrhs = u.withs(TermMode, ExprLoc) { t.rhs.toMTree[m.Term] }
      m.Defn.Def(t.mods, mname, mtparams, mvparams, mtpt, mrhs)

    case t: d.DefDef if t.name == nme.CONSTRUCTOR =>
      val rhs = t.rhs match {
        case t: d.Block if t.expr.show == "()" => t.stats.head
        case _ => t.rhs
      }

      val mvparams = u.withs(TermMode, ParamLoc) { t.vparamss.map(_.map(toMTree[m.Term.Param]))}
      val mrhs = u.withs(TermMode, ExprLoc) { rhs.toMTree[m.Term] }
      m.Ctor.Secondary(t.mods, m.Ctor.Name("this"), mvparams, mrhs)

    case u.ParamTerm(modifiers, name, tpt, default) =>
      val mname = if (name == nme.WILDCARD) m.Name.Anonymous() else m.Term.Name(name.show)
      val mrhs = u.withs(TermMode, ExprLoc) { default.map(toMTree[m.Term]) }
      val mtpt = u.withMode(TypeMode) { tpt.map(toMTree[m.Type.Arg]) }
      m.Term.Param(modifiers, mname, mtpt, mrhs)

    case u.ParamType(modifiers, name, tparams, bounds, ctxbounds) =>
      val mname = if (name.startsWith(nme.USCORE_PARAM_PREFIX))
        m.Name.Anonymous()
      else
        m.Type.Name(name.show)

      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val mbounds = u.withMode(TypeMode) { bounds.toMTree[m.Type.Bounds] }
      val mctxbounds = u.withs(TypeMode, ExprLoc) { ctxbounds.map(toMTree[m.Type]) }

      m.Type.Param(modifiers, mname, mtparams, mbounds, Nil/*view bounds*/, mctxbounds)

    case u.TypeDef(modifiers, name, tparams, rhs) =>        // important to be after ParamType
      val mname = m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }
      val mrhs = u.withs(TypeMode, ExprLoc) { rhs.toMTree[m.Type] }
      m.Defn.Type(modifiers, mname, mtparams, mrhs)

    case t: d.Template =>
      val mparents = u.withs(TermMode, SuperCallLoc) { t.parents.map(toMTree[m.Ctor.Call]) }
      val mself =  {
        val self = t.self

        val mname = if (self.name == nme.WILDCARD)
          m.Name.Anonymous()
        else
          m.Term.Name(self.name.show)

        val mtpt = if (self.isEmpty)
          None
        else
          Some(u.withMode(TypeMode) { self.tpt.toMTree[m.Type.Arg] })

        m.Term.Param(Nil, mname, mtpt, None)
      }

      val mstats = t.body match {
        case Nil => None
        case List(d.EmptyTree) => Some(Nil)
        case l => Some(l.map(toMTree[m.Stat]))
      }
      m.Template(Nil, mparents, mself, mstats)

    case u.ClassDef(modifiers, name, tparams, templ) =>
      val mname = m.Type.Name(name.show)
      val mtparams = u.withs(TypeMode, ParamLoc) { tparams.map(toMTree[m.Type.Param]) }

      val ctorparams = u.withs(TermMode, ParamLoc) {
        templ.constr.vparamss.map(_.map(toMTree[m.Term.Param]))
      }
      val mctor = m.Ctor.Primary(Nil, m.Ctor.Name("this"), ctorparams)
      val mtempl = templ.toMTree[m.Template]

      if (modifiers.is(Trait))
        m.Defn.Trait(modifiers, mname, mtparams, mctor, mtempl)
      else
        m.Defn.Class(modifiers, mname, mtparams, mctor, mtempl)

    case u.SuperCall(ctor, args) =>
      val mctor = u.withLoc(SuperCallLoc) { ctor.toMTree[m.Term] }
      val margs = u.withLoc(ExprLoc) { args.map(toMTree[m.Term.Arg]) }
      m.Term.Apply(mctor, margs)

    case t: d.ModuleDef =>
      val mname = m.Term.Name(t.name.show)
      val mtempl = t.impl.toMTree[m.Template]
      m.Defn.Object(t.mods, mname, mtempl)

    // ============ IMPORTS ============
    case u.Import(imports) =>
      val mimports = imports.map { case (expr, selectors) =>
        val mexpr = u.withs(TermMode, ExprLoc) { expr.toMTree[m.Term.Ref] }
        val mselectors = u.withLoc(ImportLoc) { selectors.map(toMTree[m.Importee]) }
        m.Importer(mexpr, mselectors)
      }
      m.Import(mimports)

    case u.ImportName(name) =>
      if (name == nme.WILDCARD) m.Importee.Wildcard()
      else m.Importee.Name(m.Name.Indeterminate(name.show))

    case u.ImportRename(a, b) =>
      m.Importee.Rename(m.Name.Indeterminate(a.show), m.Name.Indeterminate(b.show))

    case u.UnImport(name) =>
      m.Importee.Unimport(m.Name.Indeterminate(name.show))

    // ============ CTORS ============
    case u.CtorName(name) =>
      m.Ctor.Name(name.show)

    case u.CtorSelect(qual, name) =>
      val mqual = u.withs(TermMode, ExprLoc) { qual.toMTree[m.Term.Ref] }
      m.Ctor.Ref.Select(mqual, m.Ctor.Name(name.show))

    case u.CtorProject(qual, name) =>
      val mqual = u.withs(TypeMode, ExprLoc) { qual.toMTree[m.Type] }
      m.Ctor.Ref.Project(mqual, m.Ctor.Name(name.show))

    // ============ Annotations ============
    case u.TermAnnot(arg, annots) =>
      val marg = arg.toMTree[m.Term]
      val mannots = annots.map { annot =>
        val mannot = u.withs(TermMode, SuperCallLoc) { annot.toMTree[m.Term] }
        m.Mod.Annot(mannot)
      }
      m.Term.Annotate(marg, mannots)

    case u.TypeAnnot(arg, annots) =>
      val marg = arg.toMTree[m.Type]
      val mannots = annots.map { annot =>
        val mannot = u.withs(TermMode, SuperCallLoc) { annot.toMTree[m.Term] }
        m.Mod.Annot(mannot)
      }
      m.Type.Annotate(marg, mannots)

    case u.PatAnnot(arg, annots) =>
      val marg = arg.toMTree[m.Pat.Type]
      val mannots = annots.map { annot =>
        val mannot = u.withs(TermMode, SuperCallLoc) { annot.toMTree[m.Term] }
        m.Mod.Annot(mannot)
      }
      m.Pat.Type.Annotate(marg, mannots)

    case _ =>
      println("mode: " + u.mode)
      println("location: " + u.loc)
      println(tree);
      ???
  }).asInstanceOf[T]

}
