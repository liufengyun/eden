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
      if (mode != TermMode) return None

      if (loc != ExprLoc  &&
        !(loc == PatLoc && tree.isInstanceOf[BackquotedIdent])
      ) return None

      val name = tree.name
      if (name.isTypeName || tree.name == nme.WILDCARD) None
      else Some(name)
    }
  }

  object TermSelect {
    def unapply(tree: Select): Option[(Tree, TermName)] = {
      if (tree.name.isTypeName || mode != TermMode) return None
      Some((tree.qualifier, tree.name.asTermName))
    }
  }

  object TermInfixOp {
    def unapply(tree: InfixOp): Option[(Tree, TermName, List[Tree])] = {
      if (loc != ExprLoc || mode != TermMode) return None

      val args = tree.right match {
        case Tuple(args) => args
        case _ => List(tree.right)
      }

      Some((tree.left, tree.op.asTermName, args))
    }
  }

  object TermPostfixOp {
    def unapply(tree: PostfixOp): Option[(Tree, TermName)] = {
      if (mode != TermMode || loc != ExprLoc || tree.op == nme.WILDCARD) return None
      Some((tree.od, tree.op.asTermName))
    }
  }

  object TermPrefixOp {
    def unapply(tree: PrefixOp): Option[(TermName, Tree)] = {
      if (mode != TermMode || loc != ExprLoc) return None
      Some((tree.op.asTermName, tree.od))
    }
  }

  object TermEta {
    def unapply(tree: PostfixOp): Option[Tree] = {
      if (mode != TermMode || loc != ExprLoc || tree.op != nme.WILDCARD) return None
      Some(tree.od)
    }
  }

  object TermRepeated {
    def unapply(tree: Typed): Option[Tree] = {
      if (mode != TermMode || loc != ExprLoc) return None

      tree match {
        case d.Typed(tree: Tree, d.Ident(tpnme.WILDCARD_STAR)) => Some(tree)
        case _ => None
      }
    }
  }

  object TermNew {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case d.Apply(d.Select(d.New(ctor), nme.CONSTRUCTOR), args) if loc != SuperCallLoc =>
        Some((ctor, args))
      case _ =>
        None
    }
  }

  object TermTyped {
    def unapply(tree: Typed): Option[(Tree, Tree)] = {
      if (loc != ExprLoc || mode != TermMode) return None
      val d.Typed(lhs, rhs) = tree
      Some((lhs, rhs))
    }
  }


  object WildcardFunction {
    def unapply(fun: Function): Option[Tree] = {
      if (mode != TermMode) return None

      val wildcard = fun.args.forall {
        case d.ValDef(n, _, _) => n.startsWith(nme.USCORE_PARAM_PREFIX)
        case _ => false
      }

      if (!wildcard) return None

      Some(fun.body)
    }
  }

  object Function {
    def unapply(fun: Function): Option[Function] = {
      if (mode != TermMode) return None

      fun.args.foreach {
        case d.ValDef(n, _, _) if n.startsWith(nme.USCORE_PARAM_PREFIX) => return None
        case _ =>
      }

      Some(fun)
    }
  }

  object Interpolate {
    def unapply(s: InterpolatedString): Option[(TermName, List[String], List[Tree])] = {
      val segs = s.segments.flatMap { seg =>
        if (seg.isInstanceOf[Thicket])
          seg.asInstanceOf[Thicket].trees
        else
          List(seg)
      }
      val (lits, args) = segs.partition(seg => seg.isInstanceOf[Literal])
      val strs: List[String] = lits.map { t =>
        t.asInstanceOf[Literal].const.value.asInstanceOf[String]
      }
      Some((s.id, strs, args))
    }
  }

  // ============ TYPES ============
  object TypeIdent {
    def unapply(tree: Ident): Option[TypeName] = {
      if (tree.name.isTermName || mode != TypeMode) return None
      Some(tree.name.asTypeName)
    }
  }

  object TypeApply {
    def unapply(tree: AppliedTypeTree): Option[(Tree, List[Tree])] = {
      if (mode != TypeMode || loc == PatLoc) return None
      Some((tree.tpt, tree.args))
    }
  }

  object TypeInfixOp {
    def unapply(tree: InfixOp): Option[(Tree, TypeName, Tree)] = {
      if (mode != TypeMode || loc == PatLoc ||
        tree.op == tpnme.raw.BAR || tree.op == tpnme.raw.AMP) return None
      Some((tree.left, tree.op.asTypeName, tree.right))
    }
  }

  object TypeOr {
    def unapply(tree: InfixOp): Option[(Tree, Tree)] = {
      if (mode != TypeMode || loc == PatLoc || tree.op != tpnme.raw.BAR) return None
      Some((tree.left, tree.right))
    }
  }

  object TypeAnd {
    def unapply(tree: InfixOp): Option[(Tree, Tree)] = {
      if (mode != TypeMode || loc == PatLoc || tree.op != tpnme.raw.AMP) return None
      Some((tree.left, tree.right))
    }
  }

  object TypeSelect {
    def isProject(qualifier: Tree): Boolean = qualifier match {
      case d.Select(qual, name) => name.isTypeName
      case d.Ident(name) => name.isTypeName
      case _ => false
    }

    def unapply(tree: Select): Option[(Tree, TypeName)] = {
      if (!tree.name.isTypeName || mode != TypeMode || isProject(tree.qualifier)) return None
      Some((tree.qualifier, tree.name.asTypeName))
    }
  }

  object TypeProject {
    def unapply(tree: Select): Option[(Tree, TypeName)] = {
      if (!tree.name.isTypeName ||
        mode != TypeMode || loc == PatLoc ||
        !TypeSelect.isProject(tree.qualifier)) return None

      Some((tree.qualifier, tree.name.asTypeName))
    }
  }

  object TypeFunction {
    def unapply(fun: Function): Option[(List[Tree], Tree)] = {
      if (mode != TypeMode || loc == PatLoc) return None
      Some((fun.args, fun.body))
    }
  }

  object TypeTuple {
    def unapply(t: Tuple): Option[(List[Tree])] = {
      if (mode != TypeMode || loc == PatLoc) return None
      Some(t.trees)
    }
  }

  object TypeBounds {
    def unapply(bound: TypeBoundsTree): Option[(Option[Tree], Option[Tree])] = {
      if (mode != TypeMode || loc == PatLoc) return None

      val mlo = if (bound.lo.isEmpty) None else Some(bound.lo)
      val mhi = if (bound.hi.isEmpty) None else Some(bound.hi)
      Some((mlo, mhi))
    }
  }

  object TypeRepeated {
    def unapply(tree: PostfixOp): Option[Tree] = {
      if (mode != TypeMode || loc != ParamLoc) return None

      tree match {
        case PostfixOp(tree, nme.raw.STAR) => Some(tree)
        case _ => None
      }
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

  object PatInfixExtract {
    def unapply(tree: InfixOp): Option[(Tree, TermName, List[Tree])] = {
      if (loc != PatLoc) return None

      val args = tree.right match {
        case Tuple(args) => args
        case _ => List(tree.right)
      }

      Some((tree.left, tree.op.asTermName, args))
    }
  }

  object PatVarIdent {
    def unapply(tree: Ident): Option[Name] = {
      if (loc != PatLoc || mode != TermMode || tree.isInstanceOf[BackquotedIdent])
        return None

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


  object PatTypeApply {
    def unapply(tree: AppliedTypeTree): Option[(Tree, List[Tree])] = {
      if (mode != TypeMode || loc != PatLoc) return None
      Some((tree.tpt, tree.args))
    }
  }

  object PatTypeInfixOp {
    def unapply(tree: InfixOp): Option[(Tree, TypeName, Tree)] = {
      if (mode != TypeMode || loc != PatLoc ||
        tree.op == tpnme.raw.BAR || tree.op == tpnme.raw.AMP) return None
      Some((tree.left, tree.op.asTypeName, tree.right))
    }
  }

  object PatTypeOr {
    def unapply(tree: InfixOp): Option[(Tree, Tree)] = {
      if (mode != TypeMode || loc != PatLoc || tree.op != tpnme.raw.BAR) return None
      Some((tree.left, tree.right))
    }
  }

  object PatTypeAnd {
    def unapply(tree: InfixOp): Option[(Tree, Tree)] = {
      if (mode != TypeMode || loc != PatLoc || tree.op != tpnme.raw.AMP) return None
      Some((tree.left, tree.right))
    }
  }

  object PatTypeProject {
    def unapply(tree: Select): Option[(Tree, TypeName)] = {
      if (!tree.name.isTypeName ||
        mode != TypeMode || loc != PatLoc ||
        !TypeSelect.isProject(tree.qualifier)) return None

      Some((tree.qualifier, tree.name.asTypeName))
    }
  }

  object PatTypeFunction {
    def unapply(fun: Function): Option[(List[Tree], Tree)] = {
      if (mode != TypeMode || loc != PatLoc) return None
      Some((fun.args, fun.body))
    }
  }

  object PatTypeTuple {
    def unapply(t: Tuple): Option[(List[Tree])] = {
      if (mode != TypeMode || loc != PatLoc) return None
      Some(t.trees)
    }
  }

  object PatTypeWildcard {
    def unapply(bound: TypeBoundsTree): Boolean = {
      if (mode != TypeMode || loc != PatLoc) return false

      // concatenate if predicate with return predicate harms readability
      bound.lo.isEmpty && bound.hi.isEmpty
    }
  }

  // ============ CTORS ============
  object CtorName {
    def unapply(tree: Ident): Option[TypeName] = {
      if (!tree.name.isTypeName || loc != SuperCallLoc || mode != TermMode) return None
      Some(tree.name.asTypeName)
    }
  }

  object CtorSelect {
    def unapply(tree: Select): Option[(Tree, TypeName)] = {
      if (!tree.name.isTypeName ||
        loc != SuperCallLoc ||
        mode != TermMode ||
        TypeSelect.isProject(tree.qualifier)) return None

      Some((tree.qualifier, tree.name.asTypeName))
    }
  }

  object CtorProject {
    def unapply(tree: Select): Option[(Tree, TypeName)] = {
      if (!tree.name.isTypeName ||
        loc != SuperCallLoc ||
        mode != TermMode ||
        !TypeSelect.isProject(tree.qualifier)) return None

      Some((tree.qualifier, tree.name.asTypeName))
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

  // no template
  object TypeDef {
    def unapply(tree: TypeDef)(implicit ctx: Context): Option[(Modifiers, TypeName, List[TypeDef], Tree)] = {
      if (loc == ParamLoc) return None

      val tp = tree.rhs match {
        case _: Template | _: TypeBoundsTree | EmptyTree => return None // only parse bounds
        case tp => tp
      }

      Some((tree.mods, tree.name, tree.tparams, tp))
    }
  }

  object ClassDef {
    def unapply(tree: TypeDef)(implicit ctx: Context): Option[(Modifiers, TypeName, List[TypeDef], Template)] = {
      if (loc != ExprLoc) return None

      val templ = tree.rhs match {
        case t: Template => t
        case  _ => return None // only parse bounds
      }

      // Note: tree.tparams can only get type params for type def, not class/trait definition
      Some((tree.mods, tree.name, templ.constr.tparams, templ))
    }
  }

  object SuperCall {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = {
      if (loc != SuperCallLoc) return None

      tree match {
        case d.Apply(d.Select(d.New(ctor), nme.CONSTRUCTOR), args) =>
          Some((ctor, args))
        case _ =>
          None
      }
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

  object TypeDcl {
    def unapply(tree: TypeDef)(implicit ctx: Context): Option[(Modifiers, TypeName, List[TypeDef], Tree)] = {
      if (loc == ParamLoc) return None

      val tp = tree.rhs match {
        case tp: TypeBoundsTree => tp
        case _ => return None
      }

      Some((tree.mods, tree.name, tree.tparams, tp))
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

  object ParamType {
    def unapply(tree: TypeDef)(implicit ctx: Context): Option[(Modifiers, TypeName, List[TypeDef], Tree, List[Tree])] = {
      if (loc != ParamLoc) return None

      val (bounds, ctxBounds) = tree.rhs match {
        case ContextBounds(bounds, cxBounds) =>
          val cbs = cxBounds.map {
            case d.AppliedTypeTree(ctx, _) => ctx
            case t => throw new Exception("unexpected tree: " + t)
          }
          (bounds, cbs)
        case bounds: TypeBoundsTree => (bounds, Nil)
      }

      Some((tree.mods, tree.name, tree.tparams, bounds, ctxBounds))
    }
  }

  // ============ IMPORTS ============
  object Import {
    def unapply(tree: Tree): Option[List[(Tree, List[Tree])]] = tree match {
      case d.Import(expr, selectors) => Some(List(expr -> selectors))
      case t: Thicket if t.trees.forall(_.isInstanceOf[Import]) => // import a.b, a.c
        Some(t.trees.map { imp =>
          val impt = imp.asInstanceOf[Import]
          (impt.expr -> impt.selectors)
        })
      case _ => None
    }
  }

  object ImportName {
    def unapply(tree: Ident): Option[Name] = {
      if (loc != ImportLoc) return None
      else Some(tree.name)
    }
  }

  // Thicket(Ident(c), Ident(_))
  object ImportRename {
    def unapply(tree: Thicket): Option[(Name, Name)] = {
      if (loc != ImportLoc) return None
      tree.trees match {
        case List(d.Ident(a), d.Ident(b)) if b != nme.WILDCARD =>
          Some((a, b))
        case _ => None
      }
    }
  }

  object UnImport {
    def unapply(tree: Thicket): Option[Name] = {
      if (loc != ImportLoc) return None
      tree.trees match {
        case List(d.Ident(name), d.Ident(nme.WILDCARD)) =>
          Some(name)
        case _ => None
      }
    }
  }

  // ============ Annotations ============
  object TermAnnot {
    def collect(tree: Tree): (Tree, List[Tree]) = tree match {
      case d.Annotated(arg, annot) =>
        val (tree, annots) = collect(arg)
        (tree, annots :+ annot)
      case _ => (tree, Nil)
    }

    def unapply(tree: Annotated): Option[(Tree, List[Tree])] = {
      if (mode != TermMode) return None
      return Some(collect(tree))
    }
  }

  object TypeAnnot {
    def unapply(tree: Annotated): Option[(Tree, List[Tree])] = {
      if (mode != TypeMode) return None
      return Some(TermAnnot.collect(tree))
    }
  }
}
