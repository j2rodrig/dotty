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
import core.Names._
import core.Symbols._
import core.SymDenotations._
import core.Types._
import transform.TreeTransforms._
import typer._
import typer.ErrorReporting._
import util.Positions._

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

/*


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
     * If this is a concrete mutable mutability, add an intersection of Mutable to the type tp.
     * Otherwise return tp unchanged.
     */
    def addMutableIntersectionIfRequired(tp: Type)(implicit ctx: Context): Type

    def upperConcreteBound(implicit ctx: Context): Symbol

    def lowerConcreteBound(implicit ctx: Context): Symbol

    /// that is, this mutability came from Mutable or RoThis types, not annotations.
    def isFromMutableClass: Boolean = false

    /**
     * Finds an upper bound of this and another mutability.
     */
    def lub(rhs: Mutability)(implicit ctx: Context): Mutability = {
      if (isInstanceOf[WildMutability]) this
      else if (this <:< rhs) rhs
      else if (rhs <:< this) this
      else ConcreteMutability(defn.ReadonlyAnnot)
    }

    /**
     * Finds a lower bound of this and another mutability.
     */
    def glb(rhs: Mutability)(implicit ctx: Context): Mutability = {
      if (isInstanceOf[WildMutability]) rhs
      else if (this <:< rhs) this
      else if (rhs <:< this) rhs
      else ConcreteMutability(defn.MutableAnnot)
    }
  }

  /** A marker trait for abstract mutability types */
  trait AbstractMutability extends Mutability

  case class WildMutability() extends Mutability {
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability = rhs
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean = true
    def upperConcreteBound(implicit ctx: Context): Symbol = defn.MutableAnnot
    def lowerConcreteBound(implicit ctx: Context): Symbol = defn.ReadonlyAnnot
    def addMutableIntersectionIfRequired(tp: Type)(implicit ctx: Context): Type = tp
  }

  /**
   * An abstract type.
   * The given type must be either a TypeRef to TypeBounds, or otherwise a PolyParam.
   */
  case class AbstractMutabilityType(tp: Type) extends AbstractMutability {
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      case rhs: ConcreteMutability =>
        if (rhs.symbol eq defn.ReadonlyAnnot) rhs else this
      case rhs: ReceiverMutability => this
      case rhs: AbstractMutabilityType =>
        if (tp <:< rhs.tp) rhs  // if LHS won't remove any permissions from RHS, then return RHS.
        else ConcreteMutability(defn.ReadonlyAnnot)  // default to removing all permissions from RHS. // todo is this precise enough?
        //if (isIdenticalTo(rhs)) rhs
        //else if (upperConcreteBound eq defn.ReadonlyAnnot) ConcreteMutability(defn.ReadonlyAnnot)
        //else rhs
      case rhs: WildMutability => this
    }
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean = rhs match {
      case rhs: ConcreteMutability => (rhs.symbol eq defn.ReadonlyAnnot) || (upperConcreteBound eq defn.MutableAnnot)
      case rhs: ReceiverMutability => upperConcreteBound eq defn.MutableAnnot
      case rhs: AbstractMutabilityType =>
        true // todo what stronger conditions do we actually need here?
        //isIdenticalTo(rhs) || (upperConcreteBound eq defn.MutableAnnot) || (rhs.lowerConcreteBound eq defn.ReadonlyAnnot)
      case rhs: WildMutability => true
    }
    def isIdenticalTo(rhs: AbstractMutabilityType)(implicit ctx: Context): Boolean = {
      // todo may improve precision with a more extensive equality check (but probably unnecessary)
      tp == rhs.tp || (tp.dealias.typeSymbol eq rhs.tp.dealias.typeSymbol)
    }
    /**
     * Returns true if Mutable <:< tp, which means it is ok to apply in viewpoint adaptation.
     * Returns false if Any <:< tp, since intersections with Any are redundant.
     */
    def isIndependentOfType(implicit ctx: Context): Boolean = {
      (defn.MutableAnyType <:< tp) && !(defn.AnyType <:< tp)
    }
    /** Returns either a TypeBounds or NoType (which indicates maximal bounds) */
    def getBounds(implicit ctx: Context): Type = tp match {
      case tp: TypeRef => tp.info.asInstanceOf[TypeBounds]
      case tp: PolyParam =>
        if (ctx.typerState.constraint.entry(tp) eq NoType) NoType
        else ctx.typerState.constraint.fullBounds(tp)
      case tp: WildcardType => tp.optBounds
    }
    def addMutableIntersectionIfRequired(tp: Type)(implicit ctx: Context): Type = {
      if ((upperConcreteBound eq defn.MutableAnnot) && tp.isInstanceOf[ValueType]) AndType(tp, defn.MutableAnyType)
      else tp
    }
    def upperConcreteBound(implicit ctx: Context): Symbol = {
      defn.ReadonlyAnnot  // conservatively say upper bound is readonly  // todo improve precision?
      /*val bounds = getBounds
      if (bounds eq NoType) defn.ReadonlyAnnot
      else {
        val mutHi = bounds.asInstanceOf[TypeBounds].hi.dealias.getMutabilityDep
        if (mutHi.isInstanceOf[AbstractMutabilityType]) defn.ReadonlyAnnot  // just return readonly to avoid recursion
        else mutHi.upperConcreteBound
      }*/
      //else bounds.asInstanceOf[TypeBounds].hi.getMutability.upperConcreteBound
    }
    def lowerConcreteBound(implicit ctx: Context): Symbol = {
      defn.MutableAnnot  // conservatively say lower bound is mutable  // todo improve precision?
      /*val bounds = getBounds
      if (bounds eq NoType) defn.MutableAnnot
      else {
        val mutLo = bounds.asInstanceOf[TypeBounds].lo.dealias.getMutabilityDep
        if (mutLo.isInstanceOf[AbstractMutabilityType]) defn.MutableAnnot  // just return mutable to avoid recursion
        else mutLo.lowerConcreteBound
      }*/
      //else bounds.asInstanceOf[TypeBounds].lo.getMutability.lowerConcreteBound
    }
  }

  /**
   * The receiver-polymorphic mutability RoThis.
   * RoThis in one method is not compatible with RoThis in any other method,
   * and all uses of RoThis within a method refer to the same mutability type.
   * The origin symbol is the method or class RoThis refers to.
   */
  case class ReceiverMutability(origin: Symbol, _fromMutableClass: Boolean = false) extends AbstractMutability {
    def viewpointAdapt(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      case rhs: ConcreteMutability => if (rhs.symbol eq defn.ReadonlyAnnot) rhs else this
      case rhs: ReceiverMutability => this
      case rhs: AbstractMutabilityType =>
        if (rhs.upperConcreteBound eq defn.MutableAnnot) this else ConcreteMutability(defn.ReadonlyAnnot)
      case rhs: WildMutability => this
    }
    def <:<(rhs: Mutability)(implicit ctx: Context): Boolean = rhs match {
      case rhs: ConcreteMutability => rhs.symbol eq defn.ReadonlyAnnot
      case rhs: ReceiverMutability => origin eq rhs.origin
      case rhs: AbstractMutabilityType => rhs.lowerConcreteBound eq defn.ReadonlyAnnot
      case rhs: WildMutability => true
    }
    def addMutableIntersectionIfRequired(tp: Type)(implicit ctx: Context): Type = {
    //  if (!isFromMutableClass && tp.isInstanceOf[ValueType]) AndType(tp, defn.RoThisType)
    //  else tp
      tp
    }
    def upperConcreteBound(implicit ctx: Context): Symbol = defn.ReadonlyAnnot
    def lowerConcreteBound(implicit ctx: Context): Symbol = defn.MutableAnnot
    override def isFromMutableClass: Boolean = _fromMutableClass
  }

  /**
   * A concrete mutability. The symbol must be either MutableAnnot or ReadonlyAnnot.
   */
  case class ConcreteMutability(symbol: Symbol, _fromMutableClass: Boolean = false) extends Mutability {
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
      case rhs: WildMutability => true
    }
    def addMutableIntersectionIfRequired(tp: Type)(implicit ctx: Context): Type = {
      if (!isFromMutableClass && (symbol eq defn.MutableAnnot) && tp.isInstanceOf[ValueType])
        AndType(tp, defn.MutableAnyType)
      else tp
    }
    def upperConcreteBound(implicit ctx: Context): Symbol = symbol
    def lowerConcreteBound(implicit ctx: Context): Symbol = symbol
    override def lub(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      // make sure the _fromMutableClass flag is set correctly
      case rhs: ConcreteMutability if (symbol eq defn.MutableAnnot) && (rhs.symbol eq defn.MutableAnnot) =>
        if (isFromMutableClass && rhs.isFromMutableClass) this
        else ConcreteMutability(defn.MutableAnnot, _fromMutableClass = false)
      case _ => super.lub(rhs)
    }
    override def glb(rhs: Mutability)(implicit ctx: Context): Mutability = rhs match {
      // always make sure to choose the mutability that's from the Mutable class
      case rhs: ConcreteMutability if (symbol eq defn.MutableAnnot) && (rhs.symbol eq defn.MutableAnnot) =>
        if (isFromMutableClass) this
        else rhs
      case _ => super.glb(rhs)
    }
    override def isFromMutableClass: Boolean = _fromMutableClass
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
    /** Gets the flags that have been set on this tree, or EmptyFlags if no flags have been set */
    def getFlags(implicit ctx: Context): FlagSet = tree match {
      case tree: untpd.MemberDef =>
        if (tree.symbol.isCompleted) tree.symbol.flags
        else untpd.modsDeco(tree).mods.flags
      case _ => EmptyFlags
    }
    /** Gets typed annotation trees from this tree, or empty list if no annotations */
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
    def getFormals: List[List[untpd.ValDef]] = tree match {
      case tree: untpd.DefDef => tree.vparamss
      case tree: untpd.TypeDef => tree.rhs.getFormals       // class or trait
      case tree: untpd.Template => tree.constr.getFormals   // look at class/trait constructor
      case _ => Nil
    }
    def hasNoMandatoryFormalLists(implicit ctx: Context): Boolean = {
      val formals = getFormals
      if (formals.isEmpty) true
      else formals.forall { formalList =>
        // formal lists containing only implicits are not considered mandatory
        formalList.nonEmpty && formalList.head.getFlags.is(Implicit)
      }
    }
    def isGetterLike(implicit ctx: Context): Boolean =
      tree.isInstanceOf[untpd.ValDef] || (tree.isInstanceOf[untpd.DefDef] &&
        (isGetterLikeName(name) || (hasNoMandatoryFormalLists && !isSetterLikeName(name))))
  }

  implicit class CompletedSymbolDecorator(val symbol: Symbol) extends AnyVal {
    /** Gets annotations as a list of typed trees (for consistency with UntypedTreeDecorator) */
    def getAnnotations(implicit ctx: Context): List[tpd.Tree] = symbol.annotations.map(_.tree)

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

  }

  sealed abstract class BoundSpec
  case object Hi extends BoundSpec
  case object Lo extends BoundSpec
  //case object Inferred extends BoundSpec

  implicit class TypeDecorator(val tp: Type) extends AnyVal {

    def requiresMutabilityComparison(implicit ctx: Context): Boolean = tp match {
      case tp: AnnotatedType => isRecognizedTypeAnnotation(tp.annot) || tp.tpe.requiresMutabilityComparison
      case tp: ClassInfo => true
      case tp: PolyParam => false
      case tp: TypeAlias => tp.underlying.requiresMutabilityComparison
      case tp: TypeBounds => false
      case tp: MethodicType => false
      case tp: TypeProxy => tp.underlying.requiresMutabilityComparison
      case _ => false
    }

    def getMutability(spec: BoundSpec, maxBoundsRecursion: Int = 12)(implicit ctx: Context): Mutability = tp match {
      // todo return an abstract mutability when a qualifying type (Mutable <:< tp) is encountered (regardless of spec)
      case tp: AnnotatedType =>
        if ((tp.annot.symbol eq defn.MutableAnnot) || (tp.annot.symbol eq defn.ReadonlyAnnot))
          ConcreteMutability(tp.annot.symbol)
        else if (tp.annot.symbol eq defn.RoThisAnnot)
          ReceiverMutability(ctx.owner.skipWeakOwner)
        else
          tp.tpe.getMutability(spec, maxBoundsRecursion)

      case tp: TypeBounds =>
        spec match {
          case Hi => if (maxBoundsRecursion > 0) tp.hi.getMutability(Hi, maxBoundsRecursion - 1) else ConcreteMutability(defn.ReadonlyAnnot)
          case Lo => if (maxBoundsRecursion > 0) tp.lo.getMutability(Lo, maxBoundsRecursion - 1) else ConcreteMutability(defn.MutableAnnot)
        }

      case tp: ClassInfo =>
        if (tp.cls.derivesFrom(defn.MutableAnyClass)) ConcreteMutability(defn.MutableAnnot, _fromMutableClass = true)
        //else if (tp.cls.derivesFrom(defn.RoThisClass)) ReceiverMutability(ctx.owner.skipWeakOwner, _fromMutableClass = true)
        else if (tp.cls eq defn.AnyClass) ConcreteMutability(defn.ReadonlyAnnot)   // assume Any is Readonly
        else ConcreteMutability(defn.MutableAnnot)  // default other classes to Mutable

      case tp: RefinedType =>
        // Refinements of Any are mutable, although Any itself is readonly.
        if (tp.parent.isRef(defn.AnyClass)) ConcreteMutability(defn.MutableAnnot)
        else tp.parent.getMutability(spec, maxBoundsRecursion)

      case AndType(tp1, tp2) => tp1.getMutability(spec, maxBoundsRecursion) glb tp2.getMutability(spec, maxBoundsRecursion)

      case OrType(tp1, tp2) => tp1.getMutability(spec, maxBoundsRecursion) lub tp2.getMutability(spec, maxBoundsRecursion)

      case tp: MethodicType => tp.finalResultType.getMutability(spec, maxBoundsRecursion)

      case tp: JavaArrayType => ConcreteMutability(defn.MutableAnnot)   // arrays are mutable by default

      case tp: WildcardType =>
        if (tp.optBounds eq NoType) WildMutability()
        else tp.optBounds.getMutability(spec, maxBoundsRecursion)

      case _: ImportType | NoType | NoPrefix | _: ErrorType =>
        WildMutability()

      case tp: TypeProxy => tp.underlying.getMutability(spec, maxBoundsRecursion)
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

    def withMutabilitySimple(mut: Mutability)(implicit ctx: Context): Type = mut match {
      case mut: ConcreteMutability => tp.withMutabilityAnnotation(mut.symbol)
      case mut: ReceiverMutability => tp.withMutabilityAnnotation(defn.RoThisAnnot)
      case mut: AbstractMutabilityType =>
        tp.withMutabilityAnnotation(mut.upperConcreteBound)
        //val readonlyTp = tp.withMutabilityAnnotation(defn.RoThisAnnot)
        //if (mut.isIndependentOfType) AndType(readonlyTp, mut.tp)   // add mutability permissions via a type intersection
        //else readonlyTp
    }

    def withMutability(mut: Mutability)(implicit ctx: Context): Type = tp match {
      case tp: MethodType =>
        tp.derivedMethodType(tp.paramNames, tp.paramTypes, tp.resultType.withMutability(mut))
      case tp: PolyType =>
        tp.derivedPolyType(tp.paramNames, tp.paramBounds, tp.resultType.withMutability(mut))
      case tp: ExprType =>
        tp.derivedExprType(tp.resultType.withMutability(mut))
      case _ =>
        tp.withMutabilitySimple(mut)
    }
  }

  def isRecognizedTypeAnnotation(annot: Annotation)(implicit ctx: Context) = {
    val sym = annot.symbol
    sym.eq(defn.MutableAnnot) || sym.eq(defn.RoThisAnnot) || sym.eq(defn.ReadonlyAnnot)
  }

  def createAnnotation(fromSym: Symbol)(implicit ctx: Context): Annotation = {
    new ConcreteAnnotation(tpd.New(fromSym.typeRef, Nil))
  }

  def viewpointAdapt(prefixMut: Mutability, tp: Type)(implicit ctx: Context): Type = {
    val currentMut = tp.getMutability(Hi)
    val adaptedMut = prefixMut.viewpointAdapt(currentMut)
    if (currentMut eq adaptedMut) tp else tp.withMutability(adaptedMut)

    // todo remove when ready
    //if (!currentMut.isInstanceOf[ReceiverMutability] && (adaptedMut <:< currentMut)) tp  // if viewpoint adaptation doesn't reduce permissions, then we don't need to do anything
    //else tp.withMutability(adaptedMut)
  }

  /**
   * Finds the mutability of the viewpoint-adapted path from the current context to
   * the given symbol (or owner of the given symbol).
   * @param toSymbol the given symbol
   * @param ctx the current context
   * @return
   */
  def getOuterAccessPathMutability(toSymbol: Symbol)(implicit ctx: Context): Mutability = {
    // todo finish
    ConcreteMutability(defn.MutableAnnot)
  }

  /**
   * Basic approach to receiver polymorphism involves:
   * - A PolyType subclass that wraps (methods/exprs) or derives from (native poly types).
   *    The outermost poly type has a type parameter that represents receiver mutability.
   * - augment methodic selections to reduce derived polytypes by substituting the receiver-mutability type parameter.
   */



  def glbAnnotSymbol(sym1: Symbol, sym2: Symbol): Symbol = {
    if ((sym1 eq defn.MutableAnnot) || (sym2 eq defn.MutableAnnot)) defn.MutableAnnot
    else if ((sym1 eq defn.RoThisAnnot) || (sym2 eq defn.RoThisAnnot)) defn.RoThisAnnot
    else defn.ReadonlyAnnot
  }

  def lubAnnotSymbol(sym1: Symbol, sym2: Symbol): Symbol = {
    if ((sym1 eq defn.ReadonlyAnnot) || (sym2 eq defn.ReadonlyAnnot)) defn.ReadonlyAnnot
    else if ((sym1 eq defn.RoThisAnnot) || (sym2 eq defn.RoThisAnnot)) defn.RoThisAnnot
    else defn.MutableAnnot
  }

*/


  def receiverParamName = "$ROTHIS"

  /** Remove top-level mutability annotations from the given type. */
  def stripAnnots(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: AnnotatedType =>
      val sym = tp.annot.symbol
      if ((sym eq defn.ReadonlyAnnot) || (sym eq defn.MutableAnnot) || (sym eq defn.RoThisAnnot)) stripAnnots(tp.tpe)
      else tp.derivedAnnotatedType(stripAnnots(tp.tpe), tp.annot)
    case _ => tp
  }

  /**
   * Convert T @readonly to T | ReadonlyNothing
   * Convert T @mutable to T & MutableAny
   */
  def convertAnnotationToType(tp: AnnotatedType)(implicit ctx: Context): Type = {
    val sym = tp.annot.symbol
    if (sym eq defn.ReadonlyAnnot)
      OrType(stripAnnots(tp), defn.ReadonlyNothingType)
    else if (sym eq defn.MutableAnnot)
      AndType(stripAnnots(tp), defn.MutableAnyType)
    // else if (sym eq defn.RoThisAnnot)  // todo find the correct polymorphic receiver parameter
    else tp
  }

  def viewpointAdaptTermRef(tp: TermRef)(implicit ctx: Context): Type = {
    // todo adaptation of polytypes with receiver type-params
    //val prefixMutability =
    //  if (tp.prefix eq NoPrefix) getOuterAccessPathMutability(tp.symbol)
    //  else tp.prefix.getMutability(Hi)   // todo adaptation with abstract mutability types?
    //viewpointAdapt(prefixMutability, tp)
    tp
  }

  def viewpointAdaptThisType(tp: ThisType)(implicit ctx: Context): Type = {
    //val prefixMutability = getOuterAccessPathMutability(tp.cls)
    //viewpointAdapt(prefixMutability, tp)
    tp
  }

  def viewpointAdaptSuperType(tp: SuperType)(implicit ctx: Context): Type = {
    //val prefixMutability = getOuterAccessPathMutability(tp.thistpe.asInstanceOf[ThisType].cls)
    //viewpointAdapt(prefixMutability, tp)
    tp
  }

  def makePolyTypeWithReceiver(tp: MethodicType)(implicit ctx: Context): Type = {

    // todo finish
    //val
    //tp match {
    //  case tp: PolyType => tp.derivedPolyType()
    //}

    tp
  }


  implicit class TypeDecorator(val tp: Type) extends AnyVal {
    def nonMut_<:<(tp2: Type)(implicit ctx: Context): Boolean =
      ctx.typeComparer.asInstanceOf[DotModTypeComparer].ordinaryTypeComparer.topLevelSubType(tp, tp2)

    /// Does the type refer to a single class symbol? Traverses aliases, but not non-parameter type refinements.
    def underlyingClassSymbol(implicit ctx: Context): Symbol = {
      val typeRef = tp.underlyingClassRef(refinementOK = false)
      typeRef match {
        case typeRef: TypeRef => typeRef.symbol
        case NoType => NoSymbol
      }
    }
  }



  /**
   * A TypeComparer that also compares mutability.
   * @param initCtx the context the comparer operates within
   */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    /**
     * We keep around an ordinary type comparer.
     * Mostly for when we don't want to special-case mutability comparisons.
     */
    val ordinaryTypeComparer = new TypeComparer(initCtx)

    override def topLevelSubType(tp1: Type, tp2: Type): Boolean = {
      val res = super.topLevelSubType(tp1, tp2)
      /*println()
      println("  " + tp1)
      println("    <:< ")
      println("  " + tp2)
      println("    == " + res)
      println()*/
      res
    }

    override def isSubType(tp1: Type, tp2: Type): Boolean = {
      //
      // Since we have two interrelated lattices (a mutable lattice and readonly lattice),
      // we have to make sure that isSubType never returns true if tp1 is readonly
      // and tp2 is mutable. By default, all classes except Any are mutable.
      // Basically this means that if tp2 is a MutableAny class,
      // then tp1 can be any class except ReadonlyNothing or Any.
      // We treat as special the case where tp2 is MutableAny.
      // Other simple cases are handled by ordinary subtyping relationships.
      //
      if (tp2.underlyingClassSymbol eq defn.MutableAnyClass) {
        /// Traverses all aliases, including TermRefs and annotated types.
        def traverseTermRefs(tp: Type): Type = tp.dealias match {
          case tp: TermRef => traverseTermRefs(tp.info)
          case tp: AnnotatedType => traverseTermRefs(tp.tpe)
          case tpd => tpd
        }
        val sym1 = traverseTermRefs(tp1).underlyingClassSymbol
        if (sym1 ne NoSymbol) return !((sym1 eq defn.AnyClass) || (sym1 eq defn.ReadonlyNothingClass))
      }

      // More complex cases need additional special treatment. For example, we expect:
      //   (T | ReadonlyNothing) & MutableAny <:< T
      // But to show this according to DOT subtyping rules, we must show either:
      //   T | ReadonlyNothing <:< T , or  MutableAny <:< T
      // neither of which are true.
      //
      // The ordinary type comparer will help solve this problem by rewriting the example above to:
      //   (T & MutableAny) | (ReadonlyNothing & MutableAny) <:< T
      // However, the rewriting still leaves us having to show that
      //   ReadonlyNothing & MutableAny <:< T
      // which cannot be done using ordinary type comparison rules.
      //
      // We understand MutableAny as containing a certain permission declaration, and
      // ReadonlyNothing as containing all but that declaration. We therefore expect that
      //   ReadonlyNothing & MutableAny <:< Nothing
      // and
      //   Any <:< ReadonlyNothing | MutableAny
      // So we treat these as special cases.
      //
      tp1 match {
        case AndType(tp11, tp12) =>
          val cls11 = tp11.underlyingClassSymbol
          val cls12 = tp12.underlyingClassSymbol
          if (((cls11 eq defn.MutableAnyClass) && (cls12 eq defn.ReadonlyNothingClass)) ||
              ((cls11 eq defn.ReadonlyNothingClass) && (cls12 eq defn.MutableAnyClass)))
            return true
        case _ =>
      }
      tp2 match {
        case OrType(tp21, tp22) =>
          val cls21 = tp21.underlyingClassSymbol
          val cls22 = tp22.underlyingClassSymbol
          if (((cls21 eq defn.MutableAnyClass) && (cls22 eq defn.ReadonlyNothingClass)) ||
              ((cls21 eq defn.ReadonlyNothingClass) && (cls22 eq defn.MutableAnyClass)))
            return true
        case _ =>
      }

      // All other cases: Handle with ordinary subtyping logic.
      super.isSubType(tp1, tp2)
    }

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }

  class DotModTyper extends Typer {

    var alreadyStarted = false

    override def selectionType(site: Type, name: Name, pos: Position)(implicit ctx: Context): Type = {
      //
      // One issue with viewpoint-adapting TermRefs is that when the type assigner tries
      // to query for the named member of a readonly type, it doesn't succeed.
      // The reason is that Type#findMember does not understand that ReadonlyNothing
      // should contain all declarations (except the mutability permission).
      //
      // One way to solve this problem is to subclass OrType with an overloaded findMember.
      // Unfortunately, Type#findMember is declared final, so it would have to be declared
      // non-final for this solution to work. Also, any other place that creates an OrType
      // may need to be changed to create the subclassed OrType instead.
      //
      // Another way to solve this problem is to subclass the TypeAssigner, and override
      // the selectionType method. We do this here.
      // This code is a bit fragile. We only expect some combination of AndType, OrType, and
      // TermRef. Other types we leave to super#selectType. If the AndType/OrType is wrapped
      // inside anything more complicated than TermRef, the implementation here may fail.
      // Second, calls to Type#findMember from other places may fail if they have underlying
      // AndTypes or OrTypes with mutability information.
      //
      site match {
        case OrType(site1, site2) =>
          if (site1.underlyingClassSymbol eq defn.ReadonlyNothingClass)
            return selectionType(site2, name, pos)
          if (site2.underlyingClassSymbol eq defn.ReadonlyNothingClass)
            return selectionType(site1, name, pos)
        case AndType(site1, site2) =>
          if (site1.underlyingClassSymbol eq defn.MutableAnyClass)
            return selectionType(site2, name, pos)
          if (site2.underlyingClassSymbol eq defn.MutableAnyClass)
            return selectionType(site1, name, pos)
        case site: TermRef =>
          return selectionType(site.underlying match {
            case mt: MethodType if mt.paramTypes.isEmpty && (site.symbol is Stable) => mt.resultType
            case tp1 => tp1
          }, name, pos)
        case _ =>
      }

      super.selectionType(site, name, pos)
    }


    def convertType(tpe: Type)(implicit ctx: Context): Type = {

      if (!alreadyStarted) {
        if (false) {
          println("Nothing <:< ReadonlyNothing == " + (defn.NothingType <:< defn.ReadonlyNothingType))
          println("Nothing <:< MutableAny == " + (defn.NothingType <:< defn.ReadonlyNothingType))
          println("ReadonlyNothing <:< Nothing == " + (defn.ReadonlyNothingType <:< defn.NothingType))
          println("ReadonlyNothing <:< MutableAny == " + (defn.ReadonlyNothingType <:< defn.MutableAnyType))
          println("ReadonlyNothing <:< ReadonlyNothing == " + (defn.ReadonlyNothingType <:< defn.ReadonlyNothingType))
          println("ReadonlyNothing <:< Any == " + (defn.ReadonlyNothingType <:< defn.AnyType))
          println("MutableAny <:< Nothing == " + (defn.MutableAnyType <:< defn.NothingType))
          println("MutableAny <:< MutableAny == " + (defn.MutableAnyType <:< defn.MutableAnyType))
          println("MutableAny <:< ReadonlyNothing == " + (defn.MutableAnyType <:< defn.ReadonlyNothingType))
          println("MutableAny <:< Any == " + (defn.MutableAnyType <:< defn.AnyType))
          println("Any <:< ReadonlyNothing == " + (defn.AnyType <:< defn.ReadonlyNothingType))
          println("Any <:< MutableAny == " + (defn.AnyType <:< defn.MutableAnyType))
          println()
          println("Nothing <:< (MutableAny | ReadonlyNothing) == " +
            (defn.NothingType <:< OrType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println("ReadonlyNothing <:< (MutableAny | ReadonlyNothing) == " +
            (defn.ReadonlyNothingType <:< OrType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println("MutableAny <:< (MutableAny | ReadonlyNothing) == " +
            (defn.MutableAnyType <:< OrType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println("Any <:< (MutableAny | ReadonlyNothing) == " +
            (defn.AnyType <:< OrType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println()
          println("(MutableAny | ReadonlyNothing) <:< Nothing == " +
            (OrType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.NothingType))
          println("(MutableAny | ReadonlyNothing) <:< ReadonlyNothing == " +
            (OrType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.ReadonlyNothingType))
          println("(MutableAny | ReadonlyNothing) <:< MutableAny == " +
            (OrType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.MutableAnyType))
          println("(MutableAny | ReadonlyNothing) <:< Any == " +
            (OrType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.AnyType))
          println()
          println("Nothing <:< (MutableAny & ReadonlyNothing) == " +
            (defn.NothingType <:< AndType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println("ReadonlyNothing <:< (MutableAny & ReadonlyNothing) == " +
            (defn.ReadonlyNothingType <:< AndType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println("MutableAny <:< (MutableAny & ReadonlyNothing) == " +
            (defn.MutableAnyType <:< AndType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println("Any <:< (MutableAny & ReadonlyNothing) == " +
            (defn.AnyType <:< AndType(defn.MutableAnyType, defn.ReadonlyNothingType)))
          println()
          println("(MutableAny & ReadonlyNothing) <:< Nothing == " +
            (AndType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.NothingType))
          println("(MutableAny & ReadonlyNothing) <:< ReadonlyNothing == " +
            (AndType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.ReadonlyNothingType))
          println("(MutableAny & ReadonlyNothing) <:< MutableAny == " +
            (AndType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.MutableAnyType))
          println("(MutableAny & ReadonlyNothing) <:< Any == " +
            (AndType(defn.MutableAnyType, defn.ReadonlyNothingType) <:< defn.AnyType))
          println()
        }
        alreadyStarted = true
      }

      val tpe0 = tpe match {
        case tpe: AnnotatedType => convertAnnotationToType(tpe)
        case tpe: MethodicType => makePolyTypeWithReceiver(tpe)
        case tpe: TermRef => viewpointAdaptTermRef(tpe)
        case tpe: ThisType => viewpointAdaptThisType(tpe)
        case tpe: SuperType => viewpointAdaptSuperType(tpe)
        case _ => tpe
      }

      //println("Conversion of " + tpe)
      //println(isMutable(tpe0))
      //println(isReadonly(tpe0))
      //println(tpe0.isInstanceOf[NamedType])

      if (tpe ne tpe0) {
        //println("Conversion of " + tpe)
        //println("           to " + tpe0)
        //println()
      }



      /*// Named types other than Any should be considered Mutable. Patch up if needed.
      // Constant types are also defaulted to Mutable to avoid a lot of errors due to unannotated libraries.
      val tpe1 = {
        if (isMutable(tpe0) || isReadonly(tpe0) || isAnnotation(tpe0)) tpe0
        else tpe0 match {

          case tpe0: TypeRef =>
            if (tpe0.symbol.is(ModuleClass | PackageClass | HigherKinded)) tpe0  // don't do anything with modules/packages/methods
            else AndType(tpe0, defn.MutableAnyType)     // default to Mutable

          case tpe0: ConstantType => AndType(tpe0, defn.MutableAnyType)  // also default to Mutable

          case _ => tpe0
        }
      }

      // CAREFUL! DenotingTrees strip annotations when calling denot.

      if (tpe1 ne tpe0) println("     to: " + tpe1) else println("No conversion")
      println()
      */

      /*val tpe1 = {
        if (tpe0 <:< defn.MutableAnyType || defn.ReadonlyNothingType <:< tpe0) tpe0
        else if (defn.AnyType <:< tpe0) tpe0   // Any already compares as Readonly, so no modification is needed.
        else tpe0 match {
          case tpe0: NamedType => AndType(tpe0, defn.MutableAnyType)  // default to Mutable
          case tpe0: ConstantType => AndType(tpe0, defn.MutableAnyType)  // also default to Mutable
          case _ => tpe0
        }
      }*/

      tpe0
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      val tree = super.adapt(tree0, pt, original)

      if (!tree.tpe.exists || tree.tpe.isError) return tree

      /*// Convert symbol info
      tree.tpe match {
        case tpe: NamedType => tpe.denot match {
          case denot: SymDenotation =>
            println("SymDenotation " + denot.name + ":::")
            val info1 = convertType(denot.info)
            //println("SymDenot of " + denot.name + ": " + denot.info)
            if (info1 ne denot.info) {
              denot.info = info1
              //println("                    to: " + denot.info)
            }
          //println("")
          case _ =>
        }
        case _ =>
      }*/

      //println("--> " + tree.tpe)
      //if (tpe0 ne tree.tpe) println(" 0> " + tpe0)
      //if (tpe1 ne tree.tpe) println(" 1> " + tpe1)
      //println("")

      val tpe1 =
        //if (tree.isInstanceOf[Select] || tree.isInstanceOf[Ident])
          convertType(tree.tpe)
        //else tree.tpe

      val tree1 =
        //if (false) tree
        if (tpe1 eq tree.tpe) tree
        else {
          val tree1 = tree.withType(tpe1)
          if ((ctx.mode is Mode.Pattern) || tpe1 <:< pt) tree1
          else err.typeMismatch(tree1, pt)
        }

      tree1
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
