package dotty.tools
package dotc

import ast.tpd._
import ast.{tpd, untpd}
import core._
import core.Annotations._
import core.Contexts._
import core.Decorators._
import core.DenotTransformers._
import core.Flags._
import core.Phases._
import core.Symbols._
import core.Types._
import transform.TreeTransforms._
import typer.Typer

// Philosophical approach:
//  - Add @readonly to every type that I don't want to be mutable.
//
// Perhaps symbol info transformation can handle the adaptation of the symbol wrt. outer references.
//
// Viewpoint adapation of field reads can be done through the type comparer.
//
// Applications are a sticky issue right now.
// Can viewpoint adaptation on applications be dealt with through typedApply or similar interface?
// (Note: remember that getters/setters can also have JavaBeans naming, e.g.: def getX() for field x.)
//
// A RefChecks phase can check constructibility of parent classes.
// And overriding, etc.

object DotMod {

  def isRecognizedTypeAnnotation(annot: Annotation)(implicit ctx: Context) = {
    val sym = annot.symbol
    (sym.derivesFrom(defn.MutableAnnotType)
      || sym.derivesFrom(defn.ReadonlyAnnotType)
      || sym.derivesFrom(defn.PolyReadAnnotType))
  }

  def canAnnotateMethod(annot: Annotation)(implicit ctx: Context) = {
    (annot.symbol == defn.MutableAnnotType
      || annot.symbol == defn.ReadonlyAnnotType
      || annot.symbol == defn.PolyReadAnnotType)
  }

  def canAnnotateClass(annot: Annotation)(implicit ctx: Context) = {
    (annot.symbol == defn.MutableAnnotType
      || annot.symbol == defn.ReadonlyAnnotType)
  }


  class PermissionType {
    // A permission type is a (possibly polymorphic) type that holds mutability information.
  }


  /*sealed abstract class BoundSpec
  case object Hi extends BoundSpec
  case object Lo extends BoundSpec

  def lowerMutabilityBound(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Symbol = {
    if (sym1.derivesFrom(defn.MutableAnnotType) || sym2.derivesFrom(defn.MutableAnnotType))
      defn.MutableAnnotType
    else // TODO polyread
  }

  def upperMutabilityBound(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Symbol = {
    if (sym1.derivesFrom(defn.ReadonlyAnnotType) || sym2.derivesFrom(defn.ReadonlyAnnotType))
      defn.ReadonlyAnnotType
    else // TODO polyread
  }

  /// Returns the mutability bounds of the type where all polymorphic mutability components have been removed.
  def getAnnotatedMutabilityBound(tp: Type, bound: BoundSpec)(implicit ctx: Context): Symbol = {
    if (!tp.exists || tp.isError) defn.MutableAnnotType
    else tp match {

      case AnnotatedType(tpe, annot) =>
        if (canAnnotateType(annot)) annot.symbol      // a type annotation serves as both upper and lower bound
        else getAnnotatedMutabilityBound(tpe, bound)  // look at underlying type

      case AndType(tp1, tp2) =>
        bound match {
          case Lo => lowerMutabilityBound(getAnnotatedMutabilityBound(tp1, Lo), getAnnotatedMutabilityBound(tp2, Lo))
          case Hi => upperMutabilityBound(getAnnotatedMutabilityBound(tp1, Hi), getAnnotatedMutabilityBound(tp2, Hi))
        }

      case OrType(tp1, tp2) =>
        bound match {
          case Lo => upperMutabilityBound(getAnnotatedMutabilityBound(tp1, Lo), getAnnotatedMutabilityBound(tp2, Lo))
          case Hi => lowerMutabilityBound(getAnnotatedMutabilityBound(tp1, Hi), getAnnotatedMutabilityBound(tp2, Hi))
        }

      case tp: TermRef =>   // perform viewpoint adaptation
        bound match {
          case Lo => upperMutabilityBound(getAnnotatedMutabilityBound(tp.prefix, Lo), getAnnotatedMutabilityBound(tp.underlying, Lo))
          case Hi => upperMutabilityBound(getAnnotatedMutabilityBound(tp.prefix, Hi), getAnnotatedMutabilityBound(tp.underlying, Hi))
        }

      // TODO: what about polyread? is polyread a "polymorphic mutability component?"
    }
  }

  /// Returns a list of the polymorphic mutability components of this type (excludes top-level annotations).
  def getPolymorphicMutabilityVars(tp: Type)(implicit ctx: Context): List[Type] = {
    if (!tp.exists || tp.isError) Nil
    else tp match {

      case AnnotatedType(tpe, annot) =>
        if (canAnnotateType(annot)) Nil          // presence of a recognized type annotation means no polymorphic mutability vars found
        else getPolymorphicMutabilityVars(tpe)   // look at underlying type

      case AndType(tp1, tp2) =>  // get any mutability components present in both halves of the intersection
        getPolymorphicMutabilityVars(tp1) ::: getPolymorphicMutabilityVars(tp2)

      case tp: TermRef =>   // perform viewpoint adaptation
        //
        val lhsVars = getPolymorphicMutabilityVars(tp.prefix)
        val rhsVars = getPolymorphicMutabilityVars(tp.underlying)

    }
  }*/

  def isGetter(sym: Symbol)(implicit ctx: Context): Boolean = {
    // A symbol refers to a getter method if:
    //  - the symbol has no parameters (ExprType)
    //  - or the symbol has one empty parameter list and the name starts with "get" + S where S does not start with a lower-case letter
    //  - or the symbol is annotated with @polyread
    if (!(sym is Method)) false
    else if (sym.annotations.exists(_.matches(defn.PolyReadAnnotType))) true
    else {
      def isGetterMethodType(tpe: Type): Boolean = {
        tpe match {
          case _: ExprType => true
          case tpe: MethodType =>
            tpe.paramNames.isEmpty && sym.name.startsWith("get") && (sym.name.length == 3 || !sym.name.toString.charAt(3).isLower)
          case tpe: PolyType =>
            // If we've got a polymorphic type, then one of two cases applies:
            //  - the result is a method/expression that may be a getter
            //  - or the result is something else, in which case this PolyType is an expression, which is treated as a getter
            tpe.resType match {
              case _: ExprType | _: MethodType | _: PolyType => isGetterMethodType(tpe.resType)
              case _ => true
            }
        }
      }
      isGetterMethodType(sym.info)
    }
  }

  /// Is the annotation symbol sym1 a subtype of sym2?
  def isSubMutability(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean = {
    if (sym1.derivesFrom(defn.MutableAnnotType)) true
    else if (sym2.derivesFrom(defn.MutableAnnotType)) false
    else if (sym1.derivesFrom(defn.PolyReadAnnotType)) true
    else if (sym2.derivesFrom(defn.PolyReadAnnotType)) false
    else true  // both symbols are @readonly
  }

  /// Is tp1 a subtype of the given annotation symbol?
  def isSubMutability(tp1: Type, sym2: Symbol)(implicit ctx: Context): Boolean = {
    // TODO I'm not certain I actually need AndType, OrType, or MethodicType (since they do not directly involve
    // TODO  viewpoint adaptation, and isSubType should already recurse through the necessary types).
    // TODO On the contrary, AndType and OrType might be buried underneath (e.g.) a TermRef prefix, which
    // TODO  is not considered by the super type comparer. Methodic types are still not important here,
    // TODO  since they cannot exist as prefix types.

    tp1 match {

      case tp1: AnnotatedType =>
        if (isRecognizedTypeAnnotation(tp1.annot))
          isSubMutability(tp1.annot.symbol, sym2)  // found an annotation
        else
          isSubMutability(tp1.underlying, sym2)    // ignore the annotation, and check underlying

      case tp1: TermRef =>
        if (tp1.symbol is Method) false   // a methodic type cannot be a subtype of a value type
        else {
          // viewpoint adaptation of field reads.
          // see if the union of tp2's prefix and underlying type is a subtype of sym2.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          val prefixIsSubtype: Boolean =
            if (tp1.prefix ne NoPrefix) isSubMutability(tp1.prefix, sym2)
            else {
              // TODO viewpoint adaptation for a reach into the enclosing environment
              true
            }
          val underlyingIsSubtype = isSubMutability(tp1.underlying, sym2)
          prefixIsSubtype && underlyingIsSubtype
        }

      case tp1: ThisType =>
        // TODO viewpoint adaptation for a reach into the enclosing environment
        isSubMutability(tp1.underlying, sym2)

      case tp1: SuperType =>
        // do viewpoint adaptation on reach into enclosing environment via thistpe
        isSubMutability(tp1.thistpe, sym2) && isSubMutability(tp1.supertpe, sym2)

      case AndType(tp11, tp12) =>
        isSubMutability(tp11, sym2) || isSubMutability(tp12, sym2)

      case OrType(tp11, tp12) =>
        isSubMutability(tp11, sym2) && isSubMutability(tp12, sym2)

      case tp1: MethodicType =>
        isSubMutability(tp1.finalResultType, sym2)

      case tp1: TypeProxy => isSubMutability(tp1.underlying, sym2)

    }
  }

  /**
   * Mutability subtyping.
   * Basically, we find the mutability of tp2, and see if the mutability of tp1 is compatible with it.
   *
   * @param tp1
   * @param tp2
   * @return
   */
  def isSubMutability(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = {

    tp2 match {

      case tp2: AnnotatedType =>
        if (isRecognizedTypeAnnotation(tp2.annot))
          isSubMutability(tp1, tp2.annot.symbol)  // found an annotation
        else
          isSubMutability(tp1, tp2.underlying)    // ignore the annotation, and check underlying TODO already checked by isSubType

      case tp2: TermRef =>
        if (tp2.symbol is Method) {
          // TODO viewpoint adaptation of method results (change final result type of method)
          // TODO if tp2.prefix is NoPrefix, add viewpoint adaptation for a reach into the enclosing environment
          // TODO I'm not convinced I actually need to consider methodic types here, due to isSubType
          // TODO   doing a lot of recursion work already, and the result types are already adapted on Select trees.
          // TODO However, if I call TypeComparer.isSubType wherever a prefix is considered (instead of merely
          // TODO   isSubMutability), then I really only need to check TermRefs, ThisTypes/SuperTypes, and AnnotatedTypes.
          //if (isGetter(tp2.symbol))

          isSubMutability(tp1, tp2.underlying)  // no adaptation of non-getter methods
        } else {
          // viewpoint adaptation of field reads.
          // see if tp1 is a subtype of the union of tp2's prefix and underlying type.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          // TODO check that tp1 <:< tp2.prefix (which wouldn't normally be checked by the type comparer)
          val isSubtypeOfPrefix: Boolean =
            if (tp2.prefix ne NoPrefix) isSubMutability(tp1, tp2.prefix)
            else {
              // TODO viewpoint adaptation for a reach into the enclosing environment
              true
            }
          val isSubtypeOfUnderlying = isSubMutability(tp1, tp2.underlying)
          isSubtypeOfPrefix || isSubtypeOfUnderlying
        }

      case tp2: ThisType =>
        // TODO viewpoint adaptation for a reach into the enclosing environment
        isSubMutability(tp1, tp2.underlying)

      case tp2: SuperType =>
        // do viewpoint adaptation on reach into enclosing environment via thistpe
        isSubMutability(tp1, tp2.thistpe) || isSubMutability(tp1, tp2.supertpe)

      case AndType(tp21, tp22) =>
        isSubMutability(tp1, tp21) && isSubMutability(tp1, tp22)

      case OrType(tp21, tp22) =>
        isSubMutability(tp1, tp21) || isSubMutability(tp1, tp22)

      case tp2: MethodicType =>
        // check the (possibly viewpoint adapted) final result type.
        // TODO: anything else needed here?
        isSubMutability(tp1, tp2.finalResultType)

    }
  }

  class DotModTyper extends Typer {

    //def intersectWithMutability(tp: Type, mtp: Type): Type = {

    //}

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      if (tree0.isInstanceOf[Select]) println("Select " + tree0.symbol + ": " + tree0.tpe)
      //if (tree0.isInstanceOf[TypeDef]) println("TypeDef " + tree0.symbol + ": " + tree0.tpe)
      //if (tree0.isInstanceOf[TypTree]) println("TypTree " + tree0.symbol + ": " + tree0.tpe)

      def viewpointAdapt(tree: Tree): Tree = {
        if (!tree.hasType || tree.tpe.isError || !tree.isInstanceOf[Select] || !isGetter(tree.symbol))
          tree
        else {
          //println(tree + " => " + fun(tree).tpe + " => " + tree.symbol.info.resultType)

          def fun(tree: Tree): Tree = tree match {
            case _: Ident | _: Select => tree
            case tree: Apply => fun(tree.fun)
            case tree: TypeApply => fun(tree.fun)
            case tree: Typed => fun(tree.expr)
          }

          val functionType = fun(tree).tpe
          if (functionType.isError) tree
          else {
            val receiverType = functionType.stripAnnots.asInstanceOf[TermRef].prefix
            if (receiverType.isError) tree
            else {
              // Do viewpoint adaptation of the result type.
              val finalResultType = tree.symbol.info.finalResultType
              //getAnnotatedMutabilityBound(receiverType, Lo)
              //println("       (apply getter)  " + receiverType + " . " + tree.symbol)
              tree
            }
          }
        }
      }

      val tree = viewpointAdapt(super.adapt(tree0, pt, original))

      val dontCheck = (
        pt == WildcardType
          || !tree.tpe.exists
          || pt.isInstanceOf[ProtoType]
          || tree.tpe <:< defn.AnyValType
        )

      if(dontCheck) tree else {
        // TODO check that tree.tpe <:< pt wrt. mutability
        tree
      }
    }
  }

  class DotModInfoPhase extends MiniPhaseTransform with InfoTransformer {
    override def phaseName = "dotmodinfo"

    override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
      // Do we really need anything in here?
      tp
    }
  }
}
