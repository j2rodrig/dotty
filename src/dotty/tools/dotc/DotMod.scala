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

import scala.annotation.meta.getter

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
//
// TODO An alternative to @polyread:
// TODO  @getter as a method annotation (already exists in the standard library as annotation.meta.getter)!
// TODO  @rothis as the viewpoint-adapted mutability of a this-prefixed field selection in a getter method.

object DotMod {

  def isRecognizedTypeAnnotation(annot: Annotation)(implicit ctx: Context) = {
    val sym = annot.symbol
    sym.derivesFrom(defn.MutableAnnot) || sym.derivesFrom(defn.RoThisAnnot) || sym.derivesFrom(defn.ReadonlyAnnot)
  }

  def canAnnotateMethod(annot: Annotation)(implicit ctx: Context) = {
    val sym = annot.symbol
    sym.derivesFrom(defn.MutableAnnot) || sym.derivesFrom(defn.ReadonlyAnnot) || sym.derivesFrom(defn.GetterMetaAnnot)
  }

  def canAnnotateClass(annot: Annotation)(implicit ctx: Context) = {
    val sym = annot.symbol
    sym.derivesFrom(defn.MutableAnnot) || sym.derivesFrom(defn.ReadonlyAnnot)
  }


  def createAnnotation(fromSym: Symbol)(implicit ctx: Context): Annotation = {
    new ConcreteAnnotation(tpd.New(fromSym.typeRef, Nil))
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
    // A method symbol refers to a getter method if:
    //  - the symbol has no parameters (ExprType)
    //  - or the symbol has one empty parameter list and the name starts with "get" + S where S does not start with a lower-case letter
    //  - or the symbol is annotated with @getter
    if (!(sym is Method)) false
    else if (sym.annotations.exists(_.matches(defn.GetterMetaAnnot))) true
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

  def defaultMutabilityType(implicit ctx: Context): Type = {
    AnnotatedType(defn.AnyType, createAnnotation(defn.MutableAnnot))
  }

  /**
   * Finds the mutability of the outer-accessor path traversed to reach a symbol sym from
   * the current location in the source code.
   */
  def getEnvironmentPathMutabilityTo(sym: Symbol)(implicit ctx: Context): Type = {
    // TODO find common owner of current (context) location and sym
    // TODO find all annotated owner symbols between current location and common owner, computing adapted mutability

    defaultMutabilityType
  }

  /**
   * Makes a best-effort attempt to extract the mutability components of a type.
   *
   * If no mutability information is extractable, returns defaultMutabilityType.
   */
  def getMutabilityOf(tp: Type)(implicit ctx: Context): Type = {

    /**
     * If the annotated type atp has an underlying type Any, then
     * atp is already an extractable mutability type, so return it.
     * Otherwise, create an extractable mutability type with the given annotation.
     */
    def reuseOrCreate(atp: AnnotatedType) = {
      if (atp.underlying.matches(defn.AnyType))
        atp
      else
        AnnotatedType(defn.AnyType, atp.annot)
    }

    defaultMutabilityType   // TODO implement

    /*tp match {
      case tp: AnnotatedType =>
        if (isRecognizedTypeAnnotation(tp.annot)) reuseOrCreate(tp)
        else getMutabilityOf(tp.underlying)

      case tp: TermRef =>
        val prefixMutability =
          if (tp.prefix eq NoPrefix) getEnvironmentPathMutabilityTo(tp.symbol)
          else getMutabilityOf(tp.prefix)
        val underlyingMutability =
          getMutabilityOf(tp.underlying)
    }*/
  }


  /*/// Is the annotation symbol sym1 a subtype of sym2?
  def isSubMutabilityDep(sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Boolean = {
    if (sym1.derivesFrom(defn.MutableAnnotType)) true
    else if (sym2.derivesFrom(defn.MutableAnnotType)) false
    else if (sym1.derivesFrom(defn.PolyReadAnnotType)) true
    else if (sym2.derivesFrom(defn.PolyReadAnnotType)) false
    else true  // both symbols are @readonly
  }

  /// Is tp1 a subtype of the given annotation symbol?
  def isSubMutabilityDep(tp1: Type, sym2: Symbol)(implicit ctx: Context): Boolean = {
    // TODO I'm not certain I actually need AndType, OrType, or MethodicType (since they do not directly involve
    // TODO  viewpoint adaptation, and isSubType should already recurse through the necessary types).
    // TODO On the contrary, AndType and OrType might be buried underneath (e.g.) a TermRef prefix, which
    // TODO  is not considered by the super type comparer. Methodic types are still not important here,
    // TODO  since they cannot exist as prefix types.

    tp1 match {

      case tp1: AnnotatedType =>
        if (isRecognizedTypeAnnotation(tp1.annot))
          isSubMutabilityDep(tp1.annot.symbol, sym2)  // found an annotation
        else
          isSubMutabilityDep(tp1.underlying, sym2)    // ignore the annotation, and check underlying

      case tp1: TermRef =>
        if (tp1.symbol is Method) false   // a methodic type cannot be a subtype of a value type
        else {
          // viewpoint adaptation of field reads.
          // see if the union of tp2's prefix and underlying type is a subtype of sym2.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          val prefixIsSubtype: Boolean =
            if (tp1.prefix ne NoPrefix) isSubMutabilityDep(tp1.prefix, sym2)
            else {
              // TODO viewpoint adaptation for a reach into the enclosing environment
              true
            }
          val underlyingIsSubtype = isSubMutabilityDep(tp1.underlying, sym2)
          prefixIsSubtype && underlyingIsSubtype
        }

      case tp1: ThisType =>
        // TODO viewpoint adaptation for a reach into the enclosing environment
        isSubMutabilityDep(tp1.underlying, sym2)

      case tp1: SuperType =>
        // do viewpoint adaptation on reach into enclosing environment via thistpe
        isSubMutabilityDep(tp1.thistpe, sym2) && isSubMutabilityDep(tp1.supertpe, sym2)

      case AndType(tp11, tp12) =>
        isSubMutabilityDep(tp11, sym2) || isSubMutabilityDep(tp12, sym2)

      case OrType(tp11, tp12) =>
        isSubMutabilityDep(tp11, sym2) && isSubMutabilityDep(tp12, sym2)

      case tp1: MethodicType =>
        isSubMutabilityDep(tp1.finalResultType, sym2)

      case tp1: TypeProxy => isSubMutabilityDep(tp1.underlying, sym2)

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
  def isSubMutabilityDep(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = {

    tp2 match {

      case tp2: AnnotatedType =>
        if (isRecognizedTypeAnnotation(tp2.annot))
          isSubMutabilityDep(tp1, tp2.annot.symbol)  // found an annotation
        else
          isSubMutabilityDep(tp1, tp2.underlying)    // ignore the annotation, and check underlying TODO already checked by isSubType

      case tp2: TermRef =>
        if (tp2.symbol is Method) {
          // TODO viewpoint adaptation of method results (change final result type of method)
          // TODO if tp2.prefix is NoPrefix, add viewpoint adaptation for a reach into the enclosing environment
          // TODO I'm not convinced I actually need to consider methodic types here, due to isSubType
          // TODO   doing a lot of recursion work already, and the result types are already adapted on Select trees.
          // TODO However, if I call TypeComparer.isSubType wherever a prefix is considered (instead of merely
          // TODO   isSubMutability), then I really only need to check TermRefs, ThisTypes/SuperTypes, and AnnotatedTypes.
          //if (isGetter(tp2.symbol))

          isSubMutabilityDep(tp1, tp2.underlying)  // no adaptation of non-getter methods
        } else {
          // viewpoint adaptation of field reads.
          // see if tp1 is a subtype of the union of tp2's prefix and underlying type.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          // TODO check that tp1 <:< tp2.prefix (which wouldn't normally be checked by the type comparer)
          val isSubtypeOfPrefix: Boolean =
            if (tp2.prefix ne NoPrefix) isSubMutabilityDep(tp1, tp2.prefix)
            else {
              // TODO viewpoint adaptation for a reach into the enclosing environment
              true
            }
          val isSubtypeOfUnderlying = isSubMutabilityDep(tp1, tp2.underlying)
          isSubtypeOfPrefix || isSubtypeOfUnderlying
        }

      case tp2: ThisType =>
        // TODO viewpoint adaptation for a reach into the enclosing environment
        isSubMutabilityDep(tp1, tp2.underlying)

      case tp2: SuperType =>
        // do viewpoint adaptation on reach into enclosing environment via thistpe
        isSubMutabilityDep(tp1, tp2.thistpe) || isSubMutabilityDep(tp1, tp2.supertpe)

      case AndType(tp21, tp22) =>
        isSubMutabilityDep(tp1, tp21) && isSubMutabilityDep(tp1, tp22)

      case OrType(tp21, tp22) =>
        isSubMutabilityDep(tp1, tp21) || isSubMutabilityDep(tp1, tp22)

      case tp2: MethodicType =>
        // check the (possibly viewpoint adapted) final result type.
        // TODO: anything else needed here?
        isSubMutabilityDep(tp1, tp2.finalResultType)

    }
  }*/

  // I need a Type Comparer that also checks mutability.
  // I can instantiate a new type comparer with a context, which should make it possible to use in the typer phase.

  /**
   * A TypeComparer that also compares mutability.
   * @param initCtx the context the comparer operates within
   */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    /**
     * We keep around an ordinary type comparer, which is used when a non-mutability-related
     * type comparison is required.
     */
    val ordinaryTypeComparer = new TypeComparer(initCtx)

    /**
     * Is mutability annotation symbol sym1 considered a subtype of mutability annotation symbol sym2?
     */
    def isSubMutability(sym1: Symbol, sym2: Symbol): Boolean = {
      if (sym1.derivesFrom(defn.MutableAnnot)) true
      else if (sym2.derivesFrom(defn.MutableAnnot)) false
      else if (sym1.derivesFrom(defn.RoThisAnnot)) true
      else if (sym2.derivesFrom(defn.RoThisAnnot)) false
      else true  // both symbols are @readonly
    }

    /**
     * Is the type tp1 considered a subtype of the mutability annotation symbol mutSym?
     */
    protected def isSubMutability(tp1: Type, mutSym: Symbol): Boolean = {
      tp1 match {

        case tp1: AnnotatedType =>
          if (isRecognizedTypeAnnotation(tp1.annot))
            isSubMutability(tp1.annot.symbol, mutSym)  // found an annotation--check that it is compatible with mutSym
          else
            isSubMutability(tp1.underlying, mutSym)    // ignore the annotation, and check underlying

        case tp1: TermRef if !(tp1.symbol is Method) =>  // method selections are not viewpoint adapated here
          // viewpoint adaptation of field reads.
          // see if mutSym represents a supertype of the union of tp1's prefix and underlying type.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          val prefixMutability =
            if (tp1.prefix eq NoPrefix) getEnvironmentPathMutabilityTo(tp1.symbol)
            else getMutabilityOf(tp1.prefix)
          isSubMutability(prefixMutability, mutSym) && isSubMutability(tp1.underlying, mutSym)

        //TODO case tp1: ThisType =>

        case _ => true   // assume a default of @mutable, which is compatible with any passed symbol
      }
    }

    /**
     * Is the type tp1 a subtype of tp2, with respect to ordinary typing rules and to mutability rules?
     */
    protected def isSubTypeWithMutability(tp1: Type, tp2: Type): Boolean = {
      // First check for key types where ordinary subtyping behaviour should be is overridden.
      // Then call either ordinaryTypeComparer.isSubType or super.isSubType, depending on
      //   whether subsequent comparisons should take mutability into account.
      tp2 match {

        case tp2: AnnotatedType =>
          if (isRecognizedTypeAnnotation(tp2.annot))
            isSubMutability(tp1, tp2.annot.symbol) &&     // found an annotation--check that tp1 is compatible with it
              ordinaryTypeComparer.topLevelSubType(tp1, tp2.underlying)  // do underlying types check without annotations?
          else
            isSubTypeWithMutability(tp1, tp2.underlying)  // ignore the annotation, and check underlying

        case tp2: TermRef if !(tp2.symbol is Method) =>   // method selections are not viewpoint adapted here
          // viewpoint adaptation of field reads.
          // see if tp1 is a subtype of the union of tp2's prefix and underlying type.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          val prefixMutability =
            if (tp2.prefix eq NoPrefix) getEnvironmentPathMutabilityTo(tp2.symbol)
            else getMutabilityOf(tp2.prefix)

          // is tp1 compatible with either the prefix mutability OR tp2 with annotations?
          // we do it this way so we don't have to separate out the (possibly complicated) mutability from tp2.
          val isMutabilityOk = isSubTypeWithMutability(tp1, prefixMutability) || super.isSubType(tp1, tp2)
          
          // is tp1's mutability compatible with tp2, AND is unannotated tp1 compatible with unannotated tp2?
          isMutabilityOk && ordinaryTypeComparer.topLevelSubType(tp1, tp2)

        //TODO case tp2: ThisType =>

        case _ => super.isSubType(tp1, tp2)  // default to ordinary subtype handling for other types
      }
    }
    
    override def isSubType(tp1: Type, tp2: Type): Boolean = isSubTypeWithMutability(tp1, tp2)

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }

  class DotModTyper extends Typer {

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      //if (tree0.isInstanceOf[Select]) println("Select " + tree0.symbol + ": " + tree0.tpe)
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
