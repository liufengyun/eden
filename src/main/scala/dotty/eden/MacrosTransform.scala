package dotty.eden

import dotty.tools.dotc._
import core._
import Names._
import transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform, TreeTransformer}
import ast.Trees._
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import Decorators._

/** Transform macros definitions
 *
 *  1. Annotation Macros is transformed from:
 *
 *    class main extends scala.annotation.MacrosAnnotation {
 *      def apply(defn: Any): Any = meta {
 *        body
 *      }
 *    }
 *
 *   to:
 *
 *    class main extends scala.annotation.MacrosAnnotation {
 *      def apply(defn: Any): Any = ???
 *    }
 *
 *    object main$inline {
 *      def apply(prefix: Any)(defn: Any): Any = body
 *    }
 */
class MacrosTransform extends MiniPhaseTransform { thisTransformer =>
  import ast.tpd._

  var _baseMacrosType: Type = null
  def baseMacrosType(implicit ctx: Context) = {
    if (_baseMacrosType != null) _baseMacrosType
    else {
      _baseMacrosType = ctx.requiredClassRef("scala.annotation.MacrosAnnotation")
      _baseMacrosType
    }
  }

  override def phaseName = "macrosTransform"

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!tree.isClassDef || ! (tree.tpe.classSymbol.typeRef <:< baseMacrosType))
      return super.transformTypeDef(tree)

    val template = tree.rhs.asInstanceOf[Template]

    val mapplyOpt = template.body.find {
      case mdef @ DefDef(name, Nil, List(List(ValDef(_, tp1, _))), tp2, _) =>
        val rhsValid = mdef.rhs match {
          case Apply(meta, _) => meta.symbol eq ctx.definitions.metaMethod
          case _ => false
        }

        name.show == "apply" && rhsValid &&
          (tp1.symbol eq ctx.definitions.AnyType.symbol) &&
          (tp2.symbol eq ctx.definitions.AnyType.symbol)
      case _ => false
    }

    if (mapplyOpt.isEmpty) {
      ctx.error("A method `def apply(defn: Any): Any = meta { ... }` is missing for annotation macros", tree.pos)
      return super.transformTypeDef(tree)
    }

    val mapply: DefDef = mapplyOpt.get.asInstanceOf[DefDef]

    // create new object in the same scope
    val moduleName = (tree.symbol.name + "$inline").toTermName
    val moduleSym = ctx.newCompleteModuleSymbol(tree.symbol.owner, moduleName, Synthetic | ModuleVal, Synthetic | ModuleClass,
      defn.ObjectType :: Nil, Scopes.newScope, assocFile = tree.symbol.asClass.assocFile).entered
    val methodTp = MethodType(List("prefix".toTermName), List(ctx.definitions.AnyRefType),
      MethodType(List("defn".toTermName), List(ctx.definitions.AnyRefType), ctx.definitions.AnyRefType))
    val methodSym = ctx.newSymbol(moduleSym.moduleClass, "apply".toTermName, Synthetic | Method | Stable, methodTp, coord = tree.pos).entered
    val Apply(_, rhsBody::_) = mapply.rhs

    val updatedRhs = rhsBody.changeOwner(mapply.symbol, methodSym)

    val methodTree = DefDef(methodSym, updatedRhs)
    val moduleTree = ModuleDef(moduleSym, List(methodTree))

    // modify `apply` in class def
    val applyNew = cpy.DefDef(mapply)(rhs = Literal(Constant(())))
    val bodyNew = applyNew :: template.body.filter(_ != mapply)
    val annotTree = cpy.TypeDef(tree)(rhs = cpy.Template(template)(body = bodyNew))

    Thicket(annotTree :: moduleTree.trees)
  }
}

