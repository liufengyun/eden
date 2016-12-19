package dotty.eden

import dotty.tools.dotc._
import core._
import Names._
import StdNames._
import transform.TreeTransforms.{TransformerInfo, MiniPhaseTransform, TreeTransformer}
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
class MacrosTransform extends MiniPhaseTransform { thisTransformer =>
  import ast.tpd._

  private var _baseMacrosType: Type = null
  def baseMacrosType(implicit ctx: Context) = {
    if (_baseMacrosType != null) _baseMacrosType
    else {
      _baseMacrosType = ctx.requiredClassRef("scala.annotation.StaticAnnotation")
      _baseMacrosType
    }
  }

  private var _metaPackageObjSymbol: Symbol = null
  def metaSymbol(implicit ctx: Context) = {
    if (_metaPackageObjSymbol!= null) _metaPackageObjSymbol
    else {
      _metaPackageObjSymbol = ctx.requiredModule("scala.meta.package")
      _metaPackageObjSymbol
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
          case Apply(Select(meta, nme.apply), _) => meta.symbol == metaSymbol
          case _ => false
        }

        name.show == "apply" && rhsValid &&
          (tp1.symbol eq ctx.definitions.AnyType.symbol) &&
          (tp2.symbol eq ctx.definitions.AnyType.symbol)
      case _ => false
    }

    // If user mistake the signature of `apply`, no warning message - pity!
    if (mapplyOpt.isEmpty) return super.transformTypeDef(tree)

    val mapply: DefDef = mapplyOpt.get.asInstanceOf[DefDef]
    val defnSymOld = mapply.vparamss(0)(0).symbol

    // create new object in the same scope
    val moduleName = (tree.symbol.name + "$inline").toTermName
    val moduleSym = ctx.newCompleteModuleSymbol(
      tree.symbol.owner, moduleName,
      Synthetic | ModuleVal, Synthetic | ModuleClass,
      defn.ObjectType :: Nil, Scopes.newScope,
      assocFile = tree.symbol.asClass.assocFile).entered

    def buildObject(implicit ctx: Context) = {
      val methodTp = MethodType(List("prefix".toTermName), List(defn.AnyRefType),
        MethodType(List("defn".toTermName), List(defn.AnyRefType), defn.AnyRefType))

      val methodSym = ctx.newSymbol(moduleSym.moduleClass, "apply".toTermName,
        Synthetic | Method | Stable, methodTp, coord = tree.pos).entered

      def buildMethod(implicit ctx: Context) = {
        val Apply(_, rhs :: _) = mapply.rhs
        val tree = DefDef(methodSym, rhs)

        val prefixSym = tree.vparamss(0)(0).symbol
        val defnSym = tree.vparamss(1)(0).symbol

        val rhs2 = rhs.changeOwner(mapply.symbol, methodSym).subst(List(defnSymOld), List(defnSym))

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

        cpy.DefDef(tree)(rhs = mapper.transform(rhs2))
      }

      val methodTree = buildMethod(ctx.withOwner(methodSym))
      val moduleTree = ModuleDef(moduleSym, List(methodTree))

      // modify `apply` in class def
      val applyNew = cpy.DefDef(mapply)(rhs = Literal(Constant(())))
      val bodyNew = applyNew :: template.body.filter(_ != mapply)
      val annotTree = cpy.TypeDef(tree)(rhs = cpy.Template(template)(body = bodyNew))

      Thicket(annotTree :: moduleTree.trees)
    }

    buildObject(ctx.withOwner(moduleSym))
  }
}

