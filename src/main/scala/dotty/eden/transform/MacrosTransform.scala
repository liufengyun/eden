package dotty.eden
package transform

import dotty.tools.dotc._
import core._
import Names._
import StdNames._
import transform.TreeTransforms._
import ast.Trees._
import Flags._
import Types._
import Contexts.Context
import Symbols._
import Decorators._
import Annotations._
import Constants._

/** Transform macros definitions
 *
 *  1. Macro definition is transformed from:
 *
 *    class main {
 *      inline def apply[T](defn: Any): Any = meta {
 *        body
 *      }
 *    }
 *
 *   to:
 *
 *    class main {
 *      <macro> def apply(defn: Any): Any = null
 *    }
 *
 *    object main$inline {
 *      def apply(prefix: Any)(T: Any)(defn: Any): Any = body
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

    val (implObj, implMethods, moduleSym) = createImplObject(tree.symbol, macros)

    // modify macros body and flags
    val macrosNew = macros.map { m =>
      val mdef = cpy.DefDef(m)(rhs = Literal(Constant(null)))
      mdef.symbol.removeAnnotation(ctx.definitions.BodyAnnot)
      mdef.symbol.setFlag(Flags.Macro)
      mdef.symbol.resetFlag(Flags.Inline)
      mdef
    }

    val bodyNew = macrosNew ++ template.body.diff(macros)
    val treeNew = cpy.TypeDef(tree)(rhs = cpy.Template(template)(body = bodyNew))

    Thicket(treeNew :: implObj.trees)
  }

  /** create macro implementation for the A$inline object */
  def createImplMethod(defn: DefDef, owner: Symbol)(implicit ctx: Context): DefDef = {
    val Apply(_, rhs :: _) = defn.rhs

    def resultType(res: Type): MethodType = {
      def updateSignature(tp: Type): Type = tp match {
        case tp: MethodType =>
          tp.derivedMethodType(
            tp.paramNames,
            tp.paramNames.map(x => ctx.definitions.AnyType),
            updateSignature(tp.resultType)
          )
        case _ => ctx.definitions.AnyType
      }

      MethodType(
        List("prefix".toTermName),
        List(ctx.definitions.AnyType),
        updateSignature(res)
      )
    }

    val wtp = defn.tpe.widen

    val methodTp =
      if (wtp.isInstanceOf[MethodType]) resultType(wtp)
      else { // PolyType
        val names = wtp.typeParams.map(_.paramName.toTermName)
        val types = names.map(x => ctx.requiredClassRef("scala.meta.Type"))
        resultType(MethodType(names, types, wtp.resultType))
      }

    val methodSym = ctx.newSymbol(owner, defn.name,
      Synthetic | Method | Stable, methodTp, coord = defn.pos).entered

    val impl = DefDef(methodSym, rhs)

    val prefixSym = impl.vparamss(0)(0).symbol
    val fromSyms = defn.tparams.map(_.symbol) ::: defn.vparamss.flatten.map(_.symbol)
    val toSyms   = impl.vparamss.drop(1).flatten.map(_.symbol)

    // replace `this` with `prefix` and param refs with original refs
    val metaSym = ctx.requiredClass("scala.meta.Term")
    val mapper = new TreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: This if tree.tpe.isRef(metaSym) =>
          ref(prefixSym)
        case tree: Ident =>
          tree.removeAttachment(macros.OriginSymOfTree) match {
            case Some(origSym) =>
              val index = fromSyms.indexOf(origSym)
              ref(toSyms(index))
            case _ => tree
          }
        case _ =>
          super.transform(tree)
      }
    }

    val rhs2 = mapper.transform(rhs).changeOwner(defn.symbol, methodSym)
    cpy.DefDef(impl)(rhs = rhs2)
  }

  /** create A$inline to hold all macros implementations */
  def createImplObject(current: Symbol, macros: List[DefDef])(implicit ctx: Context): (Thicket, List[DefDef], Symbol) = {
    val moduleName = (current.name + "$inline").toTermName
    val moduleSym = ctx.newCompleteModuleSymbol(
      current.owner, moduleName,
      Synthetic, Synthetic,
      defn.ObjectType :: Nil, Scopes.newScope,
      assocFile = current.asClass.assocFile).entered

    val methods = macros.map(m => createImplMethod(m, moduleSym.moduleClass))

    (ModuleDef(moduleSym, methods), methods, moduleSym)
  }
}

