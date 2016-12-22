package dotty.eden

import dotty.tools.dotc._
import core._
import Names._
import StdNames._
import transform.TreeTransforms._
import DenotTransformers._
import ast.Trees._
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import Decorators._
import macros._

/** Transform macros definitions
 *
 *  1. Annotation Macros is transformed from:
 *
 *    class main extends scala.annotation.StaticAnnotation {
 *      def apply(defn: Any): Any = meta {
 *        body
 *      }
 *    }
 *
 *   to:
 *
 *    class main extends scala.annotation.StaticAnnotation {
 *      def apply(defn: Any): Any = ???
 *    }
 *
 *    object main$inline {
 *      def apply(prefix: Any)(defn: Any): Any = body
 *    }
 */
class MacrosTransform extends MiniPhaseTransform {
  thisTransformer =>

  import ast.tpd._

  private var _metaPackageObjSymbol: Symbol = null

  def metaSymbol(implicit ctx: Context) = {
    if (_metaPackageObjSymbol != null) _metaPackageObjSymbol
    else {
      _metaPackageObjSymbol = ctx.requiredModule("scala.meta.package")
      _metaPackageObjSymbol
    }
  }

  override def phaseName = "macrosTransform"

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!tree.isClassDef) return super.transformTypeDef(tree)

    val template = tree.rhs.asInstanceOf[Template]

    val macros = template.body.filter {
      case mdef : DefDef =>
        val rhsValid = mdef.rhs match {
          case Apply(Select(meta, nme.apply), _) => meta.symbol == metaSymbol
          case _ => false
        }

        mdef.symbol.is(Inline) && rhsValid
      case _ => false
    }.asInstanceOf[List[DefDef]]

    if (macros.isEmpty) return super.transformTypeDef(tree)

    val implObj = createImplObject(tree.symbol, macros)

    // modify macros body in class def
    val macrosNew = macros.map { m =>
      val mdef = cpy.DefDef(m)(rhs = Literal(Constant(())))
      mdef.withFlags(Flags.EmptyFlags)
    }

    val bodyNew = macrosNew :: template.body.diff(macros)
    val treeNew = cpy.TypeDef(tree)(rhs = cpy.Template(template)(body = bodyNew))

    Thicket(treeNew :: implObj.trees)
  }

  /** create macro implementation for the A$inline object */
  def createImplMethod(defn: DefDef, owner: Symbol)(implicit ctx: Context): Tree = {
    val Apply(_, rhs :: _) = defn.rhs

    val methodTp = MethodType(List("prefix".toTermName), List(ctx.definitions.AnyRefType), defn.tpe)

    val methodSym = ctx.newSymbol(owner, defn.name,
      Synthetic | Method | Stable, methodTp, coord = defn.pos).entered

    val impl = DefDef(methodSym, rhs)

    val prefixSym = impl.vparamss(0)(0).symbol
    val fromSyms = defn.vparamss.drop(1).flatten.map(_.symbol)
    val toSyms   = impl.vparamss.drop(1).flatten.map(_.symbol)

    val rhs2 = rhs.changeOwner(defn.symbol, methodSym).subst(fromSyms, toSyms)

    // replace `this` with `prefix`
    val metaSym = ctx.requiredClass("scala.meta.Term")
    val mapper = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: This if tree.tpe.isRef(metaSym) =>
          ref(prefixSym)
        case _ =>
          super.transform(tree)
      }
    }

    cpy.DefDef(impl)(rhs = mapper.transform(rhs2))
  }

  /** create A$inline to hold all macros implementations */
  def createImplObject(current: Symbol, macros: List[DefDef])(implicit ctx: Context): Thicket = {
    val moduleName = (current.name + "$inline").toTermName
    val moduleSym = ctx.newCompleteModuleSymbol(
      current.owner, moduleName,
      Synthetic | ModuleVal, Synthetic | ModuleClass,
      defn.ObjectType :: Nil, Scopes.newScope,
      assocFile = current.asClass.assocFile).entered

    val methods = macros.map(m => createImplMethod(m, moduleSym.moduleClass))

    ModuleDef(moduleSym, methods)
  }
}

