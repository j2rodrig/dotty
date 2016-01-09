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
import core.Symbols._
import core.Types._
import dotty.tools.dotc.core.Names.Name
import transform.TreeTransforms._
import typer.Namer
import typer.RefChecks
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
//
//
// @getter def m...
// @[mutable|rothis|readonly|uncheckedMutability] def m... / class C...
// T @[mutable|rothis|readonly|mutabilityOf(...)|uncheckedMutability]
//

/**
 * In general, 
 *
 *
 * There are two ways used here to access method parameter lists and annotations on symbols.
 *
 * For performing viewpoint adaptation of term selections, we access the types of completed symbols.
 * Completed symbols may have been loaded from class files, so are not always associated with syntax trees.
 *
 * For determining the viewpoint-adapted mutability of accesses into the enclosing environment,
 * we reach into enclosing contexts and examine possibly-untyped trees.
 * Attempting to force completion of these trees' symbols can cause cyclic reference errors.
 */


object DotMod {

  /**
   * Mutabilities can be concrete or abstract.
   *
   * Concrete mutabilities are either mutable or readonly.
   * Types can be declared to have concrete mutabilites by annotating them: T@Mutable or T@Readonly.
   * Mutability permissions can be added to types by intersection, e.g.: T with Mutable.
   *
   * Abstract mutabilities are themselves abstract types. For example:
   *   def m[T >: Mutable <: Readonly] ... // type parameter T is a polymorphic mutability type.
   * The @rothis annotation refers to the hidden polymorphic mutability of the receiver.
   */

  trait Mutability {

    /**
     * Computes the mutability of rhs, as adapted to the left-hand prefix this.
     */
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability

    /**
     * Returns true if this is a sub-mutability of rhs.
     */
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean

    /**
     * Returns true if this is a mutability identical to rhs.
     */
    def =:=(rhs: Mutability)(implicit ctx: Context): Boolean = this <:< rhs && rhs <:< this

    /**
     * Finds an upper bound of this and another mutability.
     */
    def lub(rhs: Mutability)(implicit ctx: Context): Mutability = {
      if (this <:< rhs) rhs
      else if (rhs <:< this) this
      else ConcreteMutability(defn.ReadonlyAnnot)
    }

    /**
     * Finds a lower bound of this and another mutability.
     */
    def glb(rhs: Mutability)(implicit ctx: Context): Mutability = {
      if (this <:< rhs) this
      else if (rhs <:< this) rhs
      else ConcreteMutability(defn.MutableAnnot)
    }
  }

  /** A marker trait for abstract mutability types */
  trait AbstractMutability extends Mutability

  /**
   * An abstract type with a minimum lower bound of Mutable.
   * The given type must be either a TypeRef to TypeBounds, or otherwise a PolyParam.
   */
  case class AbstractMutabilityType(tp: Type) extends AbstractMutability {
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      case rhs: ConcreteMutability =>
        if (rhs.symbol eq defn.ReadonlyAnnot) rhs else this
      case rhs: ReceiverMutability => this
      case rhs: AbstractMutabilityType =>
        if (isIdenticalTo(rhs)) rhs
        else if (upperConcreteBound eq defn.ReadonlyAnnot) ConcreteMutability(defn.ReadonlyAnnot)
        else rhs
    }
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean = rhs match {
      case rhs: ConcreteMutability => (rhs.symbol eq defn.ReadonlyAnnot) || (upperConcreteBound eq defn.MutableAnnot)
      case rhs: ReceiverMutability => upperConcreteBound eq defn.MutableAnnot
      case rhs: AbstractMutabilityType =>
        // todo it may be possible that the upper or lower bound is annotated @RoThis, but it is probably a rare enough case that we don't need to check for it
        isIdenticalTo(rhs) || (upperConcreteBound eq defn.MutableAnnot) || (rhs.lowerConcreteBound eq defn.ReadonlyAnnot)
    }
    def isIdenticalTo(rhs: AbstractMutabilityType)(implicit ctx: Context): Boolean = {
      // todo may improve precision with a more extensive equality check (but probably unnecessary)
      tp == rhs.tp || (tp.typeSymbol eq rhs.tp.typeSymbol)
    }
    def getBounds(implicit ctx: Context): TypeBounds = tp match {
      case tp: TypeRef => tp.info.asInstanceOf[TypeBounds]
      case tp: PolyParam => ctx.typerState.constraint.fullBounds(tp)
    }
    def upperConcreteBound(implicit ctx: Context): Symbol = {
      defn.ReadonlyAnnot  // todo may return MutableAnnot if upper bound is mutable
    }
    def lowerConcreteBound(implicit ctx: Context): Symbol = {
      defn.MutableAnnot  // todo may return ReadonlyAnnot if lower bound is readonly
    }
  }

  /**
   * The receiver-polymorphic mutability RoThis.
   * RoThis in one method is not compatible with RoThis in any other method,
   * and all uses of RoThis within a method refer to the same mutability type.
   * The origin symbol is the method or class RoThis refers to.
   */
  case class ReceiverMutability(origin: Symbol) extends AbstractMutability {
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      case rhs: ConcreteMutability => if (rhs.symbol eq defn.ReadonlyAnnot) rhs else this
      case rhs: ReceiverMutability => this
      case rhs: AbstractMutabilityType =>
        if (rhs.upperConcreteBound eq defn.MutableAnnot) this else ConcreteMutability(defn.ReadonlyAnnot)
    }
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean = rhs match {
      case rhs: ConcreteMutability => rhs.symbol eq defn.ReadonlyAnnot
      case rhs: ReceiverMutability => origin eq rhs.origin
      case rhs: AbstractMutabilityType => rhs.lowerConcreteBound eq defn.ReadonlyAnnot
    }
  }

  /**
   * A concrete mutability. The symbol must be either MutableAnnot or ReadonlyAnnot.
   */
  case class ConcreteMutability(symbol: Symbol) extends Mutability {
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      case rhs: ConcreteMutability =>
        if ((symbol eq defn.ReadonlyAnnot) && (rhs.symbol eq defn.MutableAnnot)) this else rhs
      case rhs: ReceiverMutability => this
      case _ => if (symbol eq defn.ReadonlyAnnot) this else rhs
    }
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean = rhs match {
      case rhs: ConcreteMutability => (symbol eq defn.MutableAnnot) || (rhs.symbol eq defn.ReadonlyAnnot)
      case rhs: ReceiverMutability => symbol eq defn.MutableAnnot
      case rhs: AbstractMutabilityType => (symbol eq defn.MutableAnnot) || (rhs.lowerConcreteBound eq defn.ReadonlyAnnot)
    }
  }


  def isGetterLikeName(name: Name): Boolean = name.startsWith("get") && (name.length == 3 || !name(4).isLower)

  def isSetterLikeName(name: Name): Boolean = name.startsWith("set") && (name.length == 3 || !name(4).isLower)

  def getAnnotationSymbols(annotations: List[Tree])(implicit ctx: Context): List[Symbol] = {
    //val annotations = getAnnotations
    annotations map { annotation =>
      if (annotation.symbol.isConstructor) annotation.symbol.owner else annotation.tpe.typeSymbol
    }
  }

  def getFirstMatchingAnnotationOf(annotations: List[Tree], givenSymbols: List[Symbol])(implicit ctx: Context): Symbol = {
    val annotationSymbols = getAnnotationSymbols(annotations)
    annotationSymbols.foreach { annotSym =>
      if (givenSymbols contains annotSym) return annotSym
    }
    NoSymbol
  }

  def getFormalReceiverMutability(annotations: List[Tree], isGetterLike: => Boolean)(implicit ctx: Context): Symbol = {
    val receiverAnnotation = getFirstMatchingAnnotationOf(
      annotations,
      List(defn.MutableAnnot, defn.ReadonlyAnnot, defn.GetterMetaAnnot, defn.RoThisAnnot)
    )
    if (receiverAnnotation eq defn.GetterMetaAnnot)
      defn.RoThisAnnot
    else if (receiverAnnotation eq NoSymbol) {
      if (isGetterLike) defn.RoThisAnnot else defn.MutableAnnot
    } else receiverAnnotation
  }

  def getFormalReceiverMutability(tree: untpd.Tree)(implicit ctx: Context): Symbol =
    getFormalReceiverMutability(tree.getAnnotations, tree.isGetterLike)

  def getFormalReceiverMutability(completedSymbol: Symbol)(implicit ctx: Context): Symbol = {
    assert(completedSymbol.isCompleted)
    getFormalReceiverMutability(completedSymbol.getAnnotations, completedSymbol.isGetterLike)
  }

  /**
   * Get information about the given Def tree.
   *
   * In practice, a Context always has a tree and an owner context.
   * But trees are not always associated with a type or a completed symbol.
   * So this is our way of getting needed information about enclosing definitions.
   */
  implicit class UntypedTreeDecorator(val tree: untpd.Tree) extends AnyVal {
    def name: Name = tree match {
      case tree: untpd.NameTree => tree.name
    }
    /** retrieve symbol attached to this tree, but without actually completing it or removing the attachment. */
    def possiblyUncompletedSymbol(implicit ctx: Context): Symbol = tree match {
      case tree: tpd.Tree => tree.symbol
      case _ => tree.attachmentOrElse(ctx.typer.SymOfTree, NoSymbol)
    }
    def getFormals: List[List[untpd.ValDef]] = tree match {
      case tree: untpd.DefDef => tree.vparamss
      case tree: untpd.TypeDef => tree.rhs.getFormals       // if class or trait
      case tree: untpd.Template => tree.constr.getFormals   // for class/trait constructor
      case _ => Nil
    }
    def hasNoMandatoryFormalLists(implicit ctx: Context): Boolean = {
      val formals = getFormals
      if (formals.isEmpty) true
      else formals.forall { formalList =>
        // formal lists containing only implicits are not considered mandatory
        formalList.nonEmpty && formalList.head.possiblyUncompletedSymbol.is(Implicit)
      }
    }
    def isGetterLike(implicit ctx: Context): Boolean =
      tree.isInstanceOf[untpd.ValDef] || (tree.isInstanceOf[DefDef] &&
        (isGetterLikeName(name) || (hasNoMandatoryFormalLists && !isSetterLikeName(name))))

    def getAnnotations(implicit ctx: Context): List[tpd.Tree] = tree match {
      case tree: untpd.MemberDef =>
        if (tree.symbol.isCompleted)
          tree.symbol.annotations.map(_.tree)
        else {
          val untypedAnnotTrees = untpd.modsDeco(tree).mods.annotations
          untypedAnnotTrees.mapconserve(ctx.typer.typedAnnotation)
        }
      case _ => Nil
    }
  }

  implicit class CompletedSymbolDecorator(val symbol: Symbol) extends AnyVal {
    def possiblyUncompletedSymbol(implicit ctx: Context): Symbol = symbol

    def hasNoMandatoryFormalLists(implicit ctx: Context): Boolean = {
      // This symbol's type either has no formal parameter lists, or all parameter lists are implicit.
      def findMethodOrExpr(tp: Type): Boolean = tp match {
        case tp: ExprType => true
        case tp: PolyType => findMethodOrExpr(tp.resultType)
        case tp: MethodType => tp.isImplicit && findMethodOrExpr(tp.resultType)
        case _ => true
      }
      findMethodOrExpr(symbol.info)
    }
    def isGetterLike(implicit ctx: Context): Boolean =
      if (symbol is Method)  // methods are "getter-like" if they either have a getter-like name or no mandatory formal parameters
        isGetterLikeName(symbol.name) || (hasNoMandatoryFormalLists && !isSetterLikeName(symbol.name))
      else if (symbol.isTerm) true  // variables are considered "getter-like" (for uniformity with getter-like methods)
      else false

    def getAnnotations(implicit ctx: Context): List[tpd.Tree] = symbol.annotations.map(_.tree)
  }

  implicit class TypeDecorator(val tp: Type) extends AnyVal {

    def getMutability(implicit ctx: Context): Mutability = tp.stripTypeVar match {
      case tp: AnnotatedType =>
        if ((tp.annot.symbol eq defn.MutableAnnot) || (tp.annot.symbol eq defn.ReadonlyAnnot))
          ConcreteMutability(tp.annot.symbol)
        else if (tp.annot.symbol eq defn.RoThisAnnot)
          ReceiverMutability(ctx.owner.skipWeakOwner)
        else
          tp.tpe.getMutability

      case tp: TypeRef => tp.info match {
        case info: TypeAlias => info.underlying.getMutability
        case info: TypeBounds => AbstractMutabilityType(tp)
        case info => info.getMutability
      }

      case tp: PolyParam => AbstractMutabilityType(tp)

      case AndType(tp1, tp2) => tp1.getMutability glb tp2.getMutability

      case OrType(tp1, tp2) => tp1.getMutability lub tp2.getMutability

      case tp: ClassInfo =>
        if (tp.cls.derivesFrom(defn.MutableClass)) ConcreteMutability(defn.MutableAnnot)
        else if (tp.cls.derivesFrom(defn.RoThisClass)) ReceiverMutability(ctx.owner.skipWeakOwner)
        else if (tp.cls eq defn.AnyClass) ConcreteMutability(defn.ReadonlyAnnot)   // assume Any is Readonly
        else ConcreteMutability(defn.MutableAnnot)  // default other classes to Mutable

      case tp: TypeProxy => tp.underlying.getMutability

      case _ => ConcreteMutability(defn.MutableAnnot)
    }

    def stripMutabilityAnnotations(implicit ctx: Context): Type = tp match {
      // Returns the type without immediate mutability annotations, if any are present.
      case tp: AnnotatedType =>
        if (isRecognizedTypeAnnotation(tp.annot)) tp.tpe.stripMutabilityAnnotations
        else tp.derivedAnnotatedType(tp.tpe.stripMutabilityAnnotations, tp.annot)
      case _ => tp
    }

    def withMutabilityAnnotation(symbol: Symbol)(implicit ctx: Context): Type = {
      AnnotatedType(tp.stripMutabilityAnnotations, createAnnotation(symbol))
    }

    def withMutability(mut: Mutability)(implicit ctx: Context): Type = mut match {
      case mut: ConcreteMutability => withMutabilityAnnotation(mut.symbol)
      case mut: ReceiverMutability => withMutabilityAnnotation(defn.RoThisAnnot)
      case mut: AbstractMutabilityType =>
    }

    /** If tp has a top-level polymorphic mutability component (minimum lower bound Any@mutable),
      * return that mutability component. Otherwise NoType.
      */
    /*def getMutabilityPoly(implicit ctx: Context): Type = NoType

    def getLowerMutabilityBound(implicit ctx: Context): Symbol = tp match {
      // Gets the lower mutability bound of tp. Result is one of: MutableAnnot, ReadonlyAnnot, or NoSymbol.
      case tp: AnnotatedType =>
        if ((tp.annot.symbol eq defn.MutableAnnot) || (tp.annot.symbol eq defn.ReadonlyAnnot))
          tp.annot.symbol
        else if (tp.annot.symbol eq defn.RoThisAnnot)   // since @rothis is polymorphic, return MutableAnnot
          defn.MutableAnnot
        else
          tp.tpe.getLowerMutabilityBound  // not a mutability annotation. Look at underlying type
      // todo other cases: check bounds on polymorphic types
      case _ => NoSymbol
    }

    def getUpperMutabilityBound(implicit ctx: Context): Symbol = tp match {
      // Gets the lower mutability bound of tp. Result is one of: MutableAnnot, ReadonlyAnnot, or NoSymbol.
      case tp: AnnotatedType =>
        if ((tp.annot.symbol eq defn.MutableAnnot) || (tp.annot.symbol eq defn.ReadonlyAnnot))
          tp.annot.symbol
        else if (tp.annot.symbol eq defn.RoThisAnnot)   // since @rothis is polymorphic, return MutableAnnot
          defn.ReadonlyAnnot
        else
          tp.tpe.getUpperMutabilityBound  // not a mutability annotation. Look at underlying type
      // todo other cases: check bounds on polymorphic types
      case _ => NoSymbol
    }*/

    /*def withAnnotation(annotation: Symbol)(implicit ctx: Context): Type = tp match {
      // Returns the type annotated with the given mutability symbol.
      case tp: AnnotatedType =>
        if (tp.annot.symbol eq annotation) tp
        else AnnotatedType(tp.stripMutabilityAnnotations, createAnnotation(annotation))
      case _ =>
        AnnotatedType(tp, createAnnotation(annotation))
    }

    def replaceRothisWithAnnotation(annotation: Symbol)(implicit ctx: Context): Type = {
      // Replaces an immediate @rothis on tp with the given annotation symbol.
      // If there is not an immediate @rothis, returns tp unmodified.
      if (tp.getMutabilityAnnotation eq defn.RoThisAnnot)
        tp.withAnnotation(annotation)
      else
        tp
    }*/

    //def substMutabilityForRoThis()  // the idea here is to substitute all occurrences of @rothis in method signatures with a given mutability // todo
  }

  def isRecognizedTypeAnnotation(annot: Annotation)(implicit ctx: Context) = {
    val sym = annot.symbol
    sym.eq(defn.MutableAnnot) || sym.eq(defn.RoThisAnnot) || sym.eq(defn.ReadonlyAnnot)
  }

  def createAnnotation(fromSym: Symbol)(implicit ctx: Context): Annotation = {
    new ConcreteAnnotation(tpd.New(fromSym.typeRef, Nil))
  }


  def viewpointAdapt(lhs: Type, rhs: Type)(implicit ctx: Context): Type = {
    val lhm = lhs.getMutability
  }


  /**
   * Finds the mutability of the viewpoint-adapted path from the current context to
   * the given symbol (or owner of the given symbol).
   * @param toSymbol the given symbol
   * @param ctx the current context
   * @return
   */
  def getOuterAccessPathMutability(toSymbol: Symbol)(implicit ctx: Context): Symbol = {
    // todo finish
    defn.MutableAnnot
  }

  def viewpointAdapt(prefix: Type, resultType: Type): Type = {
    val 
    if (prefix.upperBound eq defn.MutableAnnot) resultType
    else {
      val result = getMutability(resultType)
      if (result.lowerBound eq defn.MutableAnnot)
    }
  }



  // todo viewpoint adapt methodic types

  /**
   * Finds the mutability of the formally-declared receiver for the given symbol.
   *
   * If the symbol is a class/trait, finds the mutability of the outer (enclosing-environment) reference.
   * @param sym
   * @return
   */
  /*def getFormalReceiverMutability(sym: Symbol): Symbol = {
    // If sym is annotated with [mutable|rothis|readonly|uncheckedMutability], the receiver mutability is that annotation.
    if (sym.isAnnotatedWith(defn.UncheckedMutabilityAnnot)) defn.UncheckedMutabilityAnnot
    else {
      val first = sym.getFirstAnnotationOf(List(
        defn.MutableAnnot, defn.RoThisAnnot, defn.ReadonlyAnnot
      ))
      if (first ne NoSymbol) first
      // Otherwise, if sym is annotated with @getter, the receiver mutability is @rothis.
      else if (sym.isAnnotatedWith(defn.GetterMetaAnnot)) defn.RoThisAnnot
      // Otherwise, if sym is getter-like, the receiver mutability is @rothis.
      // todo is this going to blow up if we start asking questions about the symbol's type?
      // todo we want to know if the symbol has a parameter list or not.
      else {
        // Otherwise, the receiver mutability is @mutable.
        sym
        //if (isGetter(sym)) defn.ReadonlyAnnot
      }
      else defn.MutableAnnot
    }
  }*/

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

  /*// todo this getter method assumes sym has been completed, which is not necessarily the case.
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
  }*/

  /**
   * Finds the mutability of the outer-accessor path traversed to reach a symbol sym from
   * the current location in the source code.
   */
  /*def getEnvironmentPathMutabilityToDep(sym: Symbol)(implicit ctx: Context): Type = {
    // TODO find common owner of current (context) location and sym
    // TODO find all annotated owner symbols between current location and common owner, computing adapted mutability

    defaultMutabilityType
  }*/

  /**
   * Finds the mutability of the outer-accessor path traversed to reach a symbol sym from
   * the current location in the source code.
   */
  /*def getEnvironmentPathMutabilityTo(sym: Symbol)(implicit ctx: Context): Symbol = {
    // TODO find common owner of current (context) location and sym
    // TODO find all annotated owner symbols between current location and common owner, computing adapted mutability

    defn.MutableAnnot
  }*/

  /**
   * Makes a best-effort attempt to extract the mutability components of a type.
   *
   * If no mutability information is extractable, returns defaultMutabilityType.
   */
  /*def getMutabilityOf(tp: Type)(implicit ctx: Context): Type = {

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
  }*/


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


    /*
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
            if (tp1.prefix eq NoPrefix) getEnvironmentPathMutabilityToDep(tp1.symbol)
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
      if (tp2 eq NoType) false
      else if (tp2 eq tp1) true
      // First check for key types where ordinary subtyping behaviour should be is overridden.
      // Then call either ordinaryTypeComparer.isSubType or super.isSubType, depending on
      //   whether subsequent comparisons should take mutability into account.
      else tp2 match {

        case tp2: AnnotatedType if isRecognizedTypeAnnotation(tp2.annot) =>

          tp1 match {
            case tp1: AnnotatedType if isRecognizedTypeAnnotation(tp1.annot) =>
              isSubMutability(tp1.annot.symbol, tp2.annot.symbol)

            case tp1: TermRef if !(tp1.symbol is Method) =>
              // viewpoint adaptation of field reads.
              // see if tp1 is a subtype of the union of tp2's prefix and underlying type.
              // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
              val prefixMutability =
                if (tp1.prefix eq NoPrefix) getEnvironmentPathMutabilityToDep(tp1.symbol)
                else getMutabilityOf(tp1.prefix)
              val underlyingMutability = getMutabilityOf(tp1.underlying)

              isSubTypeWithMutability(prefixMutability, tp2) &&
                isSubTypeWithMutability(underlyingMutability, tp2) &&
                ordinaryTypeComparer.topLevelSubType(tp1, tp2)

            case _: ClassInfo | _: MethodType | NoType | ErrorType =>
              // If we reach a class definition (or other singular ground type), we assume @mutable.
              // No additional mutability checks are needed.
              ordinaryTypeComparer.topLevelSubType(tp1, tp2)

            case tp1: TypeProxy => //super.isSubType(tp1, tp2)  // the problem with this is that the tp2 annotation gets ignored

          }


        case tp2: TermRef if !(tp2.symbol is Method) =>   // method selections are not viewpoint adapted here
          // viewpoint adaptation of field reads.
          // see if tp1 is a subtype of the union of tp2's prefix and underlying type.
          // reason: the viewpoint-adapted result has only those permissions common to the prefix and underlying type.
          val prefixMutability =
            if (tp2.prefix eq NoPrefix) getEnvironmentPathMutabilityToDep(tp2.symbol)
            else getMutabilityOf(tp2.prefix)

          // is tp1 compatible with either the prefix mutability OR tp2 with annotations?
          // we do it this way so we don't have to separate out the (possibly complicated) mutability from tp2.
          val isMutabilityOk = isSubTypeWithMutability(tp1, prefixMutability) || super.isSubType(tp1, tp2)
          
          // is tp1's mutability compatible with tp2, AND is unannotated tp1 compatible with unannotated tp2?
          isMutabilityOk && ordinaryTypeComparer.topLevelSubType(tp1, tp2)

        //TODO case tp2: ThisType =>

        case _ => tp1 match {
          case tp1: AnnotatedType =>
            if (isRecognizedTypeAnnotation(tp1.annot))
        }

          super.isSubType(tp1, tp2)  // default to ordinary subtype handling for other types
      }
    }

    protected def isSubTypeWithMutabilityDep(tp1: Type, tp2: Type): Boolean = {
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
            if (tp2.prefix eq NoPrefix) getEnvironmentPathMutabilityToDep(tp2.symbol)
            else getMutabilityOf(tp2.prefix)

          // is tp1 compatible with either the prefix mutability OR tp2 with annotations?
          // we do it this way so we don't have to separate out the (possibly complicated) mutability from tp2.
          val isMutabilityOk = isSubTypeWithMutability(tp1, prefixMutability) || super.isSubType(tp1, tp2)

          // is tp1's mutability compatible with tp2, AND is unannotated tp1 compatible with unannotated tp2?
          isMutabilityOk && ordinaryTypeComparer.topLevelSubType(tp1, tp2)

        //TODO case tp2: ThisType =>

        case _ => tp1 match {
          case tp1: AnnotatedType =>
            if (isRecognizedTypeAnnotation(tp1.annot))
        }

          super.isSubType(tp1, tp2)  // default to ordinary subtype handling for other types
      }
    }

    def isSubMutability(tp1: Type, tp2: Type): Boolean = {
      tp2 match {
        case tp2: AnnotatedType =>

          tp1 match {

            case tp1: AnnotatedType =>
              isSubMutability(tp1.annot.symbol, tp2.annot.symbol)

            case tp1: TermRef =>  // TermRefs entail viewpoint adaptation

            case tp1: TypeProxy =>

          }

      }
    }*/
    
    
    def foundLowerMutabilityBound(bound: Symbol, tp2: Type): Boolean = {
      tp2.stripTypeVar match {

        case tp2: AnnotatedType if isRecognizedTypeAnnotation(tp2.annot) =>
          isSubMutability(bound, tp2.annot.symbol)  // found upper bound

        case tp2: TermRef if !(tp2.symbol is Method) =>   // methods are not viewpoint adapted here
          // TermRefs are viewpoint adapted, which means bound must be below the union
          // of tp2.info and tp2.prefix. (This is correct because viewpoint adaptation
          // can only remove mutation permissions.)
          //
          // If there is no prefix, then the mutability is viewpoint adapted path from the current
          // location to the outer environment that contains the TermRef symbol.
          //
          val acceptablePrefix =
            if (tp2.prefix eq NoPrefix) isSubMutability(bound, getEnvironmentPathMutabilityTo(tp2.symbol))
            else foundLowerMutabilityBound(bound, tp2.prefix)
          acceptablePrefix || foundLowerMutabilityBound(bound, tp2.info)

        case tp2: ThisType =>
          // ThisTypes are viewpoint adapted, since "this" reaches into the enclosing environment.
          // (Similar to TermRefs with NoPrefix.)
          isSubMutability(bound, getEnvironmentPathMutabilityTo(tp2.cls))

        case tp2: SuperType => foundLowerMutabilityBound(bound, tp2.thistpe)

        case tp2: TypeBounds => foundLowerMutabilityBound(bound, tp2.lo)

        case AndType(tp21, tp22) =>
          foundLowerMutabilityBound(bound, tp21) && foundLowerMutabilityBound(bound, tp22)

        case OrType(tp21, tp22) =>
          foundLowerMutabilityBound(bound, tp21) || foundLowerMutabilityBound(bound, tp22)

        case tp2: MethodicType =>
          // Methodic types have no relationship with annotated types.
          false

        case tp2: ClassInfo =>
          // Assume unannotated class types are @mutable, except for Any (which is @readonly)
          if (tp2.cls eq AnyClass) true
          else isSubMutability(bound, defn.MutableAnnot)

        case tp2: TypeProxy => foundLowerMutabilityBound(bound, tp2.underlying)

        case tp2: WildcardType =>
          tp2.optBounds match {
            case TypeBounds(_, hi) => foundLowerMutabilityBound(bound, hi)
            case _ => true
          }

        case _ =>
          // Assume unannotated class types (and other ground types) are @mutable.
          isSubMutability(bound, defn.MutableAnnot)
      }
    }

    def hasCompatibleMutability(tp1: Type, tp2: Type, tp1FromNamedType: Boolean = false): Boolean = {
      if (refersToSameTypeSymbol(tp1, tp2)) true
      else tp1.stripTypeVar match {

        case tp1: AnnotatedType if isRecognizedTypeAnnotation(tp1.annot) =>
          foundLowerMutabilityBound(tp1.annot.symbol, tp2)

        case tp1: TermRef if !(tp1.symbol is Method) =>   // methods are not viewpoint adapted here
          // TermRefs are viewpoint adapted, which means the union of tp1.prefix and tp1.info
          // must be compatible with tp2. (This is correct because viewpoint adaptation can only
          // remove mutation permissions.)
          //
          // If there is no prefix, then the mutability is viewpoint adapted path from the current
          // location to the outer environment that contains the TermRef symbol.
          //
          val acceptablePrefix =
            if (tp1.prefix eq NoPrefix) foundLowerMutabilityBound(getEnvironmentPathMutabilityTo(tp1.symbol), tp2)
            else hasCompatibleMutability(tp1.prefix, tp2)
          acceptablePrefix && hasCompatibleMutability(tp1.info, tp2, tp1FromNamedType = true)

        case tp1: NamedType => hasCompatibleMutability(tp1.info, tp2, tp1FromNamedType = true)

        case tp1: ThisType =>
          // ThisTypes are viewpoint adapted, since "this" reaches into the enclosing environment.
          // (Similar to TermRefs with NoPrefix.)
          foundLowerMutabilityBound(getEnvironmentPathMutabilityTo(tp1.cls), tp2)

        case tp1: SuperType => hasCompatibleMutability(tp1.thistpe, tp2)

        case tp1: TypeBounds if tp1FromNamedType => hasCompatibleMutability(tp1.hi, tp2)

        case tp1: TypeBounds if !tp1FromNamedType =>
          tp2 match {
            case tp2: TypeBounds =>
              // See if tp1 is inside tp2 (with variance)
              (tp2.variance > 0 && tp1.variance >= 0 || (tp2.lo eq NothingType) ||
                hasCompatibleMutability(tp2.lo, tp1.lo)) &&
                (tp2.variance < 0 && tp1.variance <= 0 || (tp2.hi eq AnyType) ||
                  hasCompatibleMutability(tp1.hi, tp2.hi))
            case tp2: ClassInfo =>
              ((tp1.lo eq NothingType) || hasCompatibleMutability(tp1.lo, tp2)) &&
                ((tp1.hi eq AnyType) || hasCompatibleMutability(tp2, tp1.hi))
            case _ =>
              // can't reject subtype judgement--got something we don't recognize here
              true
          }

        case AndType(tp11, tp12) =>
          hasCompatibleMutability(tp11, tp2) || hasCompatibleMutability(tp12, tp2)

        case OrType(tp11, tp12) =>
          hasCompatibleMutability(tp11, tp2) && hasCompatibleMutability(tp12, tp2)

        case tp1: MethodicType => true  // TODO what should be done about methods here?

        case tp1: TypeProxy => hasCompatibleMutability(tp1.underlying, tp2)

        case tp1: ClassInfo =>
          // Assume unannotated class types are @mutable, except for Any (which is @readonly)
          if (tp1.cls eq AnyClass) foundLowerMutabilityBound(defn.ReadonlyAnnot, tp2)
          else foundLowerMutabilityBound(defn.MutableAnnot, tp2)

        case tp1: WildcardType =>
          tp1.optBounds match {
            case TypeBounds(lo, _) => hasCompatibleMutability(lo, tp2)
            case _ => true
          }

        case _ =>
          // Assume other ground types are @mutable.
          foundLowerMutabilityBound(defn.MutableAnnot, tp2)
      }
    }

    /**
     * Returns true if tp1 and tp2 both refer to the same type symbol.
     * tp1 and tp2 may be TypeRefs or refined TypeRefs, which may possibly refer to TypeAliases.
     */
    private def refersToSameTypeSymbol(tp1: Type, tp2: Type): Boolean = {
      // Note: it is tempting to make this method more complicated because in general,
      //  TypeDefs cannot be considered equal unless their prefixes are also equal.
      //  However, it is safe to return true here even if the prefixes are not equal,
      //  because then the subsequent ordinary type comparison would return false.
      //
      //  The use case for this method is to allow equal types to be considered equal,
      //  regardless of mutabilities. Where types are not equal, mutabilities should be
      //  compared by their upper/lower bounds. Therefore, it is safe to return true
      //  if either tp1 refers to the same type as tp2, or the ordinary type comparer would
      //  return false. However, it must return false if tp1 and tp2 are NOT the same type
      //  and the ordinary type comparer would return true.
      //
      if (tp1 == tp2) true  // not sure if this check is beneficial, but it can't hurt much.
      else {
        val sym1 = findRefSymbol(tp1)
        val sym2 = findRefSymbol(tp2)
        (sym1 eq sym2) && (sym1 ne NoSymbol)
      }
    }

    def isSubTypeAnnotated(tp1: Type, tp2: Type): Boolean = {
      // The idea here is: Check for compatibility of annotations, and replicate only as much of the standard
      //   type-comparer logic as needed. In general, we want to return false if there is a mutability
      //   violation, and true if tp1 <:< tp2 without annotations. But it is safe to return true if tp1 is
      //   not really a subtype of tp2 (since this is checked by the ordinary type comparer), provided there
      //   is no mutability violation.
      // We check for three things:
      //  * Containment--if we're checking whether tp1 fits within the bounds of tp2
      //  * Identity--do tp1 and tp2 refer to the same class/type member/polymorphic parameter/etc?
      //  * Mutability--is the mutability of tp1 compatible with tp2?
      //  - Expressions commented with "// fuzzy" may return true where ordinary type comparison of the same types
      //    may return false
      //  - NOTE: It may be necessary to call isSubType on the same context before calling this method.
      //    Otherwise, the bounds computed on the GADT table may not have been narrowed correctly.
      if (tp1 eq tp2) true
      else {
        tp2 match {
          case tp2: NamedType =>
            def compareNamed(tp1: Type, tp2: NamedType): Boolean = {
              implicit val ctx: Context = this.ctx
              tp2.info match {
                case info2: TypeAlias => isSubTypeAnnotated(tp1, info2.alias)
                case _ =>
                  tp1 match {
                    case tp1: NamedType =>
                      tp1.info match {
                        case info1: TypeAlias =>
                          if (isSubTypeAnnotated(info1.alias, tp2)) return true
                          if (tp1.prefix.isStable) return false
                        case _ =>  // fallthrough
                      }
                      val sym1 = tp1.symbol
                      (if ((sym1 ne NoSymbol) && (sym1 eq tp2.symbol))
                        true  // fuzzy
                      else
                        (tp1.name eq tp2.name) && isSubTypeAnnotated(tp1.prefix, tp2.prefix)  // fuzzy
                      ) || isSubTypeNamedAnnotated(tp1, tp2)
                    case _ =>
                      isSubTypeAnnotated2(tp1, tp2)
                  }
              }
            }
            compareNamed(tp1, tp2)
          case tp2: AnnotatedType => false // todo see if tp2 is also annotated
          case tp2: ProtoType => isMatchedByProto(tp2, tp1)
          case tp2: BoundType => tp1 == tp2
          case tp2: WildcardType =>
            tp2.optBounds match {
              case TypeBounds(_, hi) => isSubTypeAnnotated(tp1, hi)
              case NoType => true
            }
          case tp2: ThisType =>
            val cls2 = tp2.cls
            tp1 match {
              case tp1: ThisType =>
                val cls1 = tp1.cls
                cls1.classInfo.selfType.derivesFrom(cls2) &&
                  cls2.classInfo.selfType.derivesFrom(cls1)
              case tp1: TermRef if (cls2 is Module) && (cls2 eq tp1.widen.typeSymbol) =>
                true  // fuzzy
              case _ =>
                isSubTypeAnnotated2(tp1, tp2)
            }
          case tp2: SuperType => true  // todo what to do here?--check that thistypes are subtypes, and supertypes are equal
          case AndType(tp21, tp22) => isSubTypeAnnotated(tp1, tp21) && isSubTypeAnnotated(tp1, tp22)
          case OrType(tp21, tp22) =>
            if (tp21.stripTypeVar eq tp22.stripTypeVar) isSubTypeAnnotated(tp1, tp21)
            else isSubTypeAnnotated2(tp1, tp2)
          case TypeErasure.ErasedValueType(cls2, underlying2) =>
            tp1 match {
              case TypeErasure.ErasedValueType(cls1, underlying1) =>
                (cls1 eq cls2) && isSameTypeAnnotated(underlying1, underlying2)
              case _ =>
                isSubTypeAnnotated2(tp1, tp2)
            }
          case tp2: TypeProxy => isSubTypeAnnotated(tp1, tp2.underlying)  // fuzzy
          case ErrorType => true
          case _ => isSubTypeAnnotated2(tp1, tp2)  // fuzzy
        }
      }
    }

    def isSameTypeAnnotated(tp1: Type, tp2: Type): Boolean = isSubTypeAnnotated(tp1, tp2) && isSubTypeAnnotated(tp2, tp1)

    private def isSubTypeAnnotated2(tp1: Type, tp2: Type): Boolean = {
      tp1 match {
        case tp1: NamedType =>
          tp1.info match {
            case info1: TypeAlias =>
              if (isSubTypeAnnotated(info1.alias, tp2)) return true
              if (tp1.prefix.isStable) return false
              true  // fuzzy
            case TypeBounds(_, hi1) =>
              val gbounds1 = ctx.gadt.bounds(tp1.symbol)
              (gbounds1 != null && isSubTypeWhenFrozen(gbounds1.hi, tp2)) || isSubTypeAnnotated(hi1, tp2)  // fuzzy
            case _ => true  // fuzzy
          }
        case tp1: PolyParam => true  // todo see if isSubTypeWhenFrozen(bounds(tp1).hi, tp2)
        case tp1: AnnotatedType => false // todo see if tp2 is also annotated
        case tp1: SkolemType =>
          tp2 match {
            case tp2: SkolemType if !ctx.phase.isTyper && tp1.info <:< tp2.info => true
            case _ => true  // fuzzy
          }
        case tp1: ThisType =>
          val cls1 = tp1.cls
          tp2 match {
            case tp2: TermRef if (cls1 is Module) && (cls1 eq tp2.widen.typeSymbol) =>
              true  // fuzzy
            case _ =>
              true  // fuzzy
          }
        case tp1: WildcardType => tp1.optBounds match {
            case TypeBounds(lo, _) => isSubType(lo, tp2)
            case _ => true
          }
        case AndType(tp11, tp12) =>
          if (tp11.stripTypeVar eq tp12.stripTypeVar) isSubTypeAnnotated(tp11, tp2)
          else true  // fuzzy
        case OrType(tp11, tp12) => isSubTypeAnnotated(tp11, tp2) && isSubTypeAnnotated(tp12, tp2)
        case tp1: TypeProxy => isSubTypeAnnotated2(tp1.underlying, tp2)  // fuzzy
        case ErrorType => true
        case _ => true  // fuzzy
      }
    }

    private def isSubTypeNamedAnnotated(tp1: Type, tp2: NamedType): Boolean = {
      // If tp2.info is a TypeBounds, check that tp1 <:< tp2.info.lo
      tp2.info match {
        case TypeBounds(lo2, _) =>
          val gbounds2 = ctx.gadt.bounds(tp2.symbol)
          (gbounds2 != null && isSubTypeWhenFrozen(tp1, gbounds2.lo)) || isSubTypeAnnotated(tp1, lo2)  // fuzzy
        case _ => true // fuzzy
      }
    }

    /**
     * Finds the class symbol or abstract type symbol referred to by the given type.
     * The given type may be an aliased or refined type (c.f. Type#isRef).
     * @param tp the given type, possibly aliased or refined.
     * @return a class symbol or abstract type symbol, or NoSymbol if tp does not refer to a class or abstract type.
     */
    private def findRefSymbol(tp: Type): Symbol = tp.stripTypeVar match {
      case tp: TypeRef =>
        tp.info match {
          case TypeAlias(aliasTp) => findRefSymbol(aliasTp)
          case _ => tp.symbol
        }
      case tp: RefinedType => findRefSymbol(tp.parent)
      case _ => NoSymbol
    }

    // --------------------------------------------------------

    def rebuilt(tp1: Type, tp2: Type): Boolean = {
      tp2 match {
        case WildcardType(NoType) => true
        case WildcardType(TypeBounds(_, hi)) => rebuilt(tp1, hi)
        case AnnotatedType(tpe, annot) if isRecognizedTypeAnnotation(annot) =>
          if (annot.symbol eq defn.ReadonlyAnnot) true
          else {
            println(".. " + tp1 + " <:< " + tp2)
            false
          }
        // todo: bounds containment, named type hi/lo bounds
        case _ => subRebuilt(tp1, tp2)
      }
    }

    def subRebuilt(tp1: Type, tp2: Type): Boolean = {
      tp1 match {
        case WildcardType(NoType) => true
        case WildcardType(TypeBounds(lo, _)) => rebuilt(lo, tp2)
        case AnnotatedType(tpe, annot) if isRecognizedTypeAnnotation(annot) =>
          if (annot.symbol eq defn.MutableAnnot) true
          else {
            println(".. " + tp1 + " <:< " + tp2)
            false
          }
        case _ => true  // don't reject match
      }
    }

    /// Follow TypeRefs until we reach a ClassInfo, TypeBounds, or something else definitive.
    def typeDeref(tp: TypeRef): Type = {

    }

    override def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
      super.topLevelSubType(tp1, tp2) && rebuilt(tp1, tp2)
    }

    // --------------------------------------------------------

    //override def isSubType(tp1: Type, tp2: Type): Boolean = {
      // The mutability of tp1 is compatible with the mutability of tp2 if either:
      // tp1 and tp2 are the same type, or upperAnnotation(tp1) <: lowerAnnotation(tp2).
      //
      //ordinaryTypeComparer.topLevelSubType(tp1, tp2) && hasCompatibleMutability(tp1, tp2)
      //ordinaryTypeComparer.topLevelSubType(tp1, tp2) && isSubTypeAnnotated(tp1, tp2)
      //super.isSubType(tp1, tp2) && isSubTypeAnnotated(tp1, tp2)
    //}

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }

  class DotModTyper extends Typer {

    /// The function tree underlying the given tree (always returns a Select or Ident).
    def fun(tree: Tree): Tree = tree match {
      case _: Ident | _: Select => tree
      case tree: Apply => fun(tree.fun)
      case tree: TypeApply => fun(tree.fun)
      case tree: Typed => fun(tree.expr)
    }

    /// Takes a method selection/application tree and returns the type of the given receiver (TermRef or ErrorType).
    def getActualReceiverType(tree: Tree): Type = {
      val functionType = fun(tree).tpe
      if (functionType.isError) ErrorType
      else functionType.stripAnnots.asInstanceOf[TermRef].prefix
    }

    /// Takes a method symbol and returns an annotation symbol.
    def getDeclaredReceiverMutability(sym: Symbol)(implicit ctx: Context): Symbol = {
      if (!(sym is Method)) defn.RoThisAnnot    // treat field read like getter method--receiver is @rothis
      else if (isGetter(sym)) defn.RoThisAnnot  // getters have @rothis receivers
      else {
        getAnnotation(sym.info.finalResultType) // non-getter methods
      }
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      val tree = super.adapt(tree0, pt, original)

      // Do viewpoint adaptation
      val tree1 =
        if (!tree.hasType || tree.tpe.isError) tree
        else tree match {

          case tree: Ident =>
            // todo Viewpoint adapt term symbols to reflect outer-accessor usage
            tree.tpe match {
              case tpe: TypeRef => tree
              case _ => tree
            }

          case tree: This =>
            // todo Viewpoint adapt "this" references to reflect outer-accessor usage
            tree

          case tree: Select =>
            // todo Viewpoint adapt getter methods and field selections
            if (tree.symbol is Method) {
              // todo error if receiver is not compatible
              // todo viewpoint adaptation of final result type if required

              getActualReceiverType(tree)

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
              tree
            } else {
              tree
            }

          case tree: TypeBoundsTree =>
            // Add default annotations to type bounds
            tree.tpe match {
              case _: TypeAlias => tree
              case TypeBounds(lo, hi) =>
                // todo if "hi" is Any, then make it @readonly
                tree
            }
        }

      //if (tree0.isInstanceOf[Select]) println("Select " + tree0.symbol + ": " + tree0.tpe)
      //if (tree0.isInstanceOf[TypeDef]) println("TypeDef " + tree0.symbol + ": " + tree0.tpe)
      //if (tree0.isInstanceOf[TypTree]) println("TypTree " + tree0.symbol + ": " + tree0.tpe)

      def viewpointAdapt(tree: Tree): Tree = {
        if (!tree.hasType || tree.tpe.isError || !tree.isInstanceOf[Select])
          tree
        else if (isGetter(tree.symbol)) {  // viewpoint adapt getter methods
          //println(tree + " => " + fun(tree).tpe + " => " + tree.symbol.info.resultType)

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
        else if (tree.symbol is Method) tree  // don't viewpoint adapt ordinary methods
        else {  // todo viewpoint adapt term selections
          tree
        }
      }

      //val tree = viewpointAdapt(super.adapt(tree0, pt, original))

      val dontCheck = (
        pt == WildcardType
          || !tree1.tpe.exists
          || pt.isInstanceOf[ProtoType]
          || tree1.tpe <:< defn.AnyValType
        )

      if(dontCheck) tree1 else {
        // TODO check that tree.tpe <:< pt wrt. mutability
        tree1
      }
    }
  }

  /** This phase runs the regular Scala RefChecks with the DotMod type comparer to enforce necessary
    * subtyping relationships between symbols.
    */
  class DotModRefChecks extends RefChecks {
    println("Created " + phaseName)

    //override def run(implicit ctx: Context): Unit = {
    //  println("Running " + phaseName)
    //  super.run(ctx.fresh.setTypeComparerFn(new DotModTypeComparer(_)))
    //}
    override def phaseName: String = "dotmodrefchecks"
  }

  class DotModInfoPhase extends MiniPhaseTransform with InfoTransformer {
    override def phaseName = "dotmodinfo"

    override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
      // Do we really need anything in here?
      tp
    }
  }
}
