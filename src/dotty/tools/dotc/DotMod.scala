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
*/

  def isGetterLikeName(name: Name): Boolean = name.startsWith("get") && (name.length == 3 || !name(4).isLower)

  def isSetterLikeName(name: Name): Boolean = name.startsWith("set") && (name.length == 3 || !name(4).isLower)

  def getAnnotationSymbols(annotations: List[Tree])(implicit ctx: Context): List[Symbol] = {
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

  /*
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
  }*/

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
      case tree: untpd.MemberDef => untpd.modsDeco(tree).mods.flags
      case _ => EmptyFlags
    }
    /** Gets typed annotation trees from this tree, or empty list if no annotations */
    def getAnnotations(implicit ctx: Context): List[tpd.Tree] = tree match {
      case tree: untpd.MemberDef =>
        if (tree.hasType && tree.symbol.isCompleted)   // if annotations are already typed, then don't bother retyping
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
    //
    // Returns true if this tree represents a method that should have a polymorphic environment-reference type.
    // todo deal with @readonly methods
    // todo anything to do with respect to synthetic accessor methods?
    //
    // todo get rid of special cases here. Just say the method is a getter if annotated with @getter or @polyread.
    // todo add special case to refchecks to allow mutual overriding of getters and valdefs.
    // Returns true if this is a method that should be considered a getter method even if not annotated.
    // A method is considered a getter here if it has a "getter-like" name,
    //  or is synthetic with no explicit parameter lists.
    // todo move check for @getter annotation into this class
    //
    def isGetterLike(implicit ctx: Context): Boolean = {
      tree.isInstanceOf[untpd.DefDef] && {
        val annots = getAnnotationSymbols(getAnnotations)
        val getterAnnots = List(defn.GetterMetaAnnot, defn.PolyReadAnnot, defn.RoThisAnnot)
        annots.exists(getterAnnots contains _)
      }
    }
      //tree.isInstanceOf[untpd.DefDef] && isGetterLikeName(name) // || (hasNoMandatoryFormalLists && getFlags.is(Synthetic)))
      //tree.isInstanceOf[untpd.ValDef] || (tree.isInstanceOf[untpd.DefDef] &&
      //  (isGetterLikeName(name) || (hasNoMandatoryFormalLists && !isSetterLikeName(name))))
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
    //
    // Returns true if this symbol is a method that should have a polymorphic environment-reference type.
    // todo deal with @readonly methods
    // todo anything to do with respect to synthetic accessor methods?
    //
    // Returns true if this is a method that should be considered a getter method even if not annotated.
    // A method is considered a getter here if it has a "getter-like" name,
    //  or is synthetic with no explicit parameter lists.
    // todo move check for @getter annotation into this class
    //
    def isGetterLike(implicit ctx: Context): Boolean = {
      (symbol is Method) && {
        val annots = getAnnotationSymbols(getAnnotations)
        val getterAnnots = List(defn.GetterMetaAnnot, defn.PolyReadAnnot, defn.RoThisAnnot)
        annots.exists(getterAnnots contains _)
      }
    }
      //(symbol is Method) && isGetterLikeName(symbol.name) // || (hasNoMandatoryFormalLists && (symbol is Synthetic)))
      //if (symbol is Method)  // methods are "getter-like" if they either have a getter-like name or no mandatory formal parameters
        //isGetterLikeName(symbol.name) || (hasNoMandatoryFormalLists && !isSetterLikeName(symbol.name))
      //else if (symbol.isTerm) true  // variables are considered "getter-like" (for uniformity with getter-like methods)
      //else false

  }

  /*
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

  /**
    * Given the name of a getter, returns the name of its environment-reference type parameter.
    */
  def envRefTypeName(ownerName: Name): TypeName = typeName("EnvRef_" + ownerName)

  /**
    * Returns true if the given symbol is a recognized mutability annotation symbol.
    */
  def isMutabilityAnnot(sym: Symbol)(implicit ctx: Context): Boolean =
    (sym eq defn.ReadonlyAnnot) ||
      (sym eq defn.MutableAnnot) ||
      (sym eq defn.RoThisAnnot) ||
      (sym eq defn.PolyReadAnnot) ||
      (sym eq defn.UncheckedMutabilityAnnot) ||
      (sym eq defn.MutabilityOfAnnot)

  /**
    * Removes top-level mutability annotations from the given type.
    * Annotations that are not mutability annotations are preserved.
    */
  def stripAnnots(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: AnnotatedType =>
      if (isMutabilityAnnot(tp.annot.symbol)) stripAnnots(tp.tpe)
      else tp.derivedAnnotatedType(stripAnnots(tp.tpe), tp.annot)
    case _ => tp
  }

  /**
    * Returns the type corresponding to the given type name.
    * Does this by creating a new tree with the given name, then typing it.
    * Also takes a position because the typer expects trees to have positions.
    *
    * @param tpnm
    * @param pos
    * @param ctx
    * @return
    */
  def namedTypeFromTree(tpnm: Name, pos: Position)(implicit ctx: Context): Type = {
    val tpTree = ctx.typer.typed(
      untpd.TypeTree(
        untpd.Ident(tpnm).withPos(pos)
      ).withPos(pos)
    )
    tpTree.tpe
  }

  /**
    * Finds the name of the environment-reference type of the innermost enclosing
    * getter method or class.
    *
    * @param ctx the context to look in
    * @return the name of the environment-reference type, or None if not found
    */
  def findNearestEnclosingPolyreadType(ctx: Context): Option[Name] = {
    implicit val ctx0: Context = ctx
    if (ctx.tree.isGetterLike) Some(envRefTypeName(ctx.tree.name))
    else if (ctx.outer eq NoContext) None
    else findNearestEnclosingPolyreadType(ctx.outer)
  }

  /**
    * Converts annotated types to "real" types.
    *
    * Performs the following kinds of conversions (where T is an arbitrary type):
    *   T @readonly   -->    T | ReadonlyNothing
    *   T @mutable    -->    T & MutableAny
    *   T @polyread   -->    (T | ReadonlyNothing) & EnvRef_X
    *      (where X is the name of an enclosing getter)
    */
  def convertAnnotationToType(tp: AnnotatedType)(implicit ctx: Context): Type = {
    val sym = tp.annot.symbol
    if (sym eq defn.ReadonlyAnnot)
      OrType(stripAnnots(tp), defn.ReadonlyNothingType)
    else if (sym eq defn.MutableAnnot)
      AndType(stripAnnots(tp), defn.MutableAnyType)
    // todo alternative to @rothis: use @mutabilityOf(this)?
    // else if (sym eq defn.RoThisAnnot)  // todo find the correct polymorphic receiver parameter
    else if (sym eq defn.PolyReadAnnot)
      // TODO IF REACHING THE ENV TYPE CROSSES A READONLY BOUNDARY, THE RETURNED TYPE SHOULD BE READONLY
      findNearestEnclosingPolyreadType(ctx) match {
        case Some(tpnm) =>
          val readonlyTp = OrType(stripAnnots(tp), defn.ReadonlyNothingType)
          val envRefTp = namedTypeFromTree(tpnm, tp.annot.tree.pos)
          AndType(readonlyTp, envRefTp)
        case None =>
          errorType("Cannot find enclosing getter", tp.annot.tree.pos)
      }
    else if (sym eq defn.GetterMetaAnnot) tp  // ignore @getter where bound to types
      // todo @(inline @getter) is found on some ValDefs -- do we want to check for this?
      // todo see tests/pos/rbtree.scala
      // todo see scala-scala/src/library/scala/collection/immutable/RedBlackTree.scala
      //errorType(s"@getter is not allowed as a type annotation", tp.annot.tree.pos)
    else if (isMutabilityAnnot(sym))
      errorType(s"Internal compiler error: Unhandled mutability annotation @${sym.name}", tp.annot.tree.pos)
    else tp
  }

  /**
    * Finds the innermost context that is a common owner of the given symbol
    * and the current context. Note that we return a context rather than a
    * symbol because the context owner may not have been completed yet.
    *
    * @param sym
    * @param ctx
    * @return
    */
  def findCommonOwnerContext(sym: Symbol)(implicit ctx: Context): Context = {

    assert(sym.exists)

    def findContextIfSymIsOwner(myCtx: Context): Context = {
      if (sym eq myCtx.owner) myCtx
      else if (myCtx.owner eq NoSymbol) NoContext
      else findContextIfSymIsOwner(myCtx.outer)
    }

    val ownerCtx = findContextIfSymIsOwner(ctx)
    if (ownerCtx ne NoContext) ownerCtx
    else findCommonOwnerContext(sym.owner)
  }

  /**
    * Computes the mutability of the accessor path from the current location
    * to the environment containing the given symbol.
    *
    * @param sym
    * @param ctx
    * @return
    */
  def accessorChainToSymbol(sym: Symbol, pos: Position)(implicit ctx: Context): Type = {

    //println(s"---accessor chain to $sym")

    var typ: Type = defn.MutableAnyType
    val ownerCtx = findCommonOwnerContext(sym)
    //println(s"---  found common owner ${ownerCtx.owner}")
    var currCtx = ctx
    while (currCtx ne ownerCtx) {
      //
      // If the common owner is getter-like, then do a viewpoint adaptation to
      // simulate access through an environment-reference field.
      //
      if (currCtx.tree.isGetterLike) {
        //
        // Create a type with the given name in the context where the name is created.
        // Using the context where the name was created prevents problems with name collisions,
        //  which would happen if two nested getter methods have the same name.
        //
        val envRefType = namedTypeFromTree(envRefTypeName(currCtx.tree.name), pos)(currCtx)
        //
        // Do a union of all getter-method environment-reference types.
        // Since we know that every environment reference type already has a lower
        //  bound of MutableAny, we can skip the usual intersection with ReadonlyNothing
        //  (as happens in general viewpoint adaptation).
        //
        typ =
          if (typ eq defn.MutableAnyType) envRefType
          else if (envRefType <:< typ) typ   // skip adding to the union if already in the union
          else OrType(typ, envRefType)
      }
      //
      // todo deal with @readonly methods
      //
      currCtx = currCtx.outer
    }
    typ
  }

  /*def viewpointAdaptTermRef(tp: TermRef)(implicit ctx: Context): Type = {
    tp.dealias.stripAnnots match {
      case tp1: MethodicType =>  // todo what to do here?
      case
    }
    // todo prefix mutability

    tp.prefix

    tp.info match {

    }


    // todo adaptation of polytypes with receiver type-params
    //val prefixMutability =
    //  if (tp.prefix eq NoPrefix) getOuterAccessPathMutability(tp.symbol)
    //  else tp.prefix.getMutability(Hi)   // todo adaptation with abstract mutability types?
    //viewpointAdapt(prefixMutability, tp)
    tp
  }*/

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

    /**
     * Finds the TermRef underneath AndTypes, OrTypes, and annotations.
     * If it doesn't exist, or is ambiguous, returns NoType.
     */
    def underlyingTermRef(implicit ctx: Context): Type = tp.dealias.stripAnnots match {
      case tp: TermRef => tp
      case tp: AndOrType =>
        val term1 = tp.tp1.underlyingTermRef
        val term2 = tp.tp2.underlyingTermRef
        if (term1.exists && !term2.exists) term1
        else if (!term1.exists && term2.exists) term2
        else NoType
      case _ => NoType
    }

    /**
     * Returns the extractable mutability portion of this type.
     * The returned list should be interpreted as a set of intersection types.
     * An empty list means readonly.
     */
    def mutability(implicit ctx: Context): List[Type] = {
      val wtp = tp.widenDealias.stripAnnots
      if (defn.ReadonlyNothingType <:< wtp) List()
      else if (wtp <:< defn.MutableAnyType) List(defn.MutableAnyType)
      else if (defn.MutableAnyType <:< wtp) List(tp)  // found a polymorphic mutability type
      else wtp match {
        case AndType(tp1, tp2) =>
          tp1.mutability ::: tp2.mutability   // include mutabilities present in either type
        case OrType(tp1, tp2) =>
          val tp2m = tp2.mutability
          tp1.mutability.filter(tp2m.contains(_))  // include mutabilities present in both types
        case _ => List()  // default to readonly if mutability cannot be extracted
      }
    }

    /**
     * Returns this type, but with the mutability intersections in the given type list.
     */
    def withMutability(tps: List[Type])(implicit ctx: Context): Type = {
      // If there are any intersections with MutableAny, then ignore all other mutabilities
      if (tps contains defn.MutableAnyType) {
        //println(s"---  is <:< Mutable == " + (tp.widenDealias <:< defn.MutableAnyType))
        if (tp.widenDealias <:< defn.MutableAnyType) tp     // if this type is already mutable, don't do anything
        else AndType(tp, defn.MutableAnyType)  // otherwise, return an intersection type
        // Note: Adding &MutableAny to self-types during viewpoint adaptation (which was happening
        //  before the tp <:< defn.MutableAnyType condition was added) was causing a failure of
        //  the findRef method in typedIdent to find the self-reference symbol.
        // This bug shouldn't crop up when looking for variables/methods.
      }
      else {
        //
        // We first do a union with ReadonlyNothing to negate the effects of any existing
        // mutability intersections. Then we add an intersection with all types in the types list.
        //
        var tp1 = tp.asReadonly
        tps.foreach { mut => tp1 = AndType(tp1, mut) }
        tp1
      }
    }

    def asReadonly(implicit ctx: Context): Type = OrType(tp, defn.ReadonlyNothingType)


    /**
     * The @mutabilityOf annotation.
     * When a type of the form T@mutabilityOf(x) is encountered, it is transformed into
     * (typeOf(x) & ReadonlyNothing) | T.
     *
     * Getter methods have a polymorphic receiver reference. E.g., for
     * class C {
     *   def m...
     * }
     * we have:
     *   def m[EnvRefType >: C <: C@readonly]...
     * So that any types inside the body of m of the form
     *   T@mutabilityOf(C.this)
     * are transformed to:
     *   (EnvRefType & ReadonlyNothing) | T
     *
     * Where there are multiple enclosing environments, viewpoint adaptation will
     * take care of multiple hops.
     * class C {
     *   class D[EnvRefType$1 >: C <: C@readonly] {  // we do $1 to visually differentiate types. In the implementation, the $1 doesn't exist.
     *     def m[EnvRefType$2 >: D <: D@readonly]... {
     *       T@mutabilityOf(C.this)
     *     }
     *   }
     * }
     * The type of D.this inside the body of m is:
     *   EnvRefType$2
     * so the type of C.this inside the body of m is D.this |> C.this, or:
     *   (EnvRefType$2 & ReadonlyNothing) | EnvRefTypes$1
     * so the type T@mutabilityOf(C.this) becomes:
     *   (((EnvRefType$2 & ReadonlyNothing) | EnvRefTypes$1) & ReadonlyNothing) | T
     *
     * Any reach into an enclosing environment must go through the correct environment accessors.
     * Inside the body of m,
     *   D.this
     * has not merely the type D, but rather EnvRef$2.
     *
     * If the polymorphic environment parameters are pure mutabilities, e.g.:
     *   def m[EnvRefType >: Nothing <: ReadonlyNothing]...
     * then the type expressions above can be simplified:
     *   EnvRefType | T
     *   EnvRefType$2 | EnvRefType$1 | T
     * but I'm not sure ATM if this simplification is useful.
     *
     *
     * The @envMut annotation.
     * The @envMut annotation reaches one level into the enclosing environment.
     * If the enclosing environment is a class C, then @envMut is the same as @mutabilityOf(C.this).
     * If the enclosing environment is a method, then @envMut is the mutability
     * of the frame reference as seen from the current location.
     * For example:
     * def m1... = {
     *   def m2[EnvRefType >: MutableAny <: Any]... = {
     *     T@envMut   // is transformed to (EnvRefType & ReadonlyNothing) | T.
     *   }
     * }
     * We don't care that we don't have a "type" for the frame of m1.
     * We only care about the frame's mutability as seen from inside m2,
     * so the bounds are MutableAny to Any (c.f. C to C@readonly).
     *
     * Access to a variable in the body of m1 from within the body of m2 requires
     * viewpoint adaptation through EnvRefType. E.g.:
     * def m1... = {
     *   var s: T
     *   def m2[EnvRefType >: MutableAny <: Any]... = {
     *     s
     *   }
     * }
     * From within the body of m1, variable s simply has the type T.
     * From within the body of m2, the type of s is viewed as:
     *   (EnvRefType & ReadonlyNothing) | T
     * which is the same as the type:
     *   T@mutabilityOf(s)
     *
     * In general, typeOf(s) == typeOf(s)@mutabilityOf(s).
     *
     *
     * In implementation, the EnvRefType parameters are generated during tree
     * adaptation. So we can't count on having access to these parameters, since
     * they may not yet have been generated! The solution is to use a LazyRef
     * type that resolves to the correct type parameter when the type's info is
     * requested. We would then just have to be careful not to ask for its info
     * before the underlying parameter has been created...
     *
     * Another part of the solution is to reshape unadapted DefDef/TypeDef trees
     * to include polymorphic EnvRefType parameters.
     *
     * Perhaps it is necessary to do an unforced symbol lookup to find the correct
     * EnvRefType parameter. But what is the correct mechanism for creating these
     * symbols and associating them with trees?
     *
     */


    /**
     * Performs viewpoint adaptation with respect to the given prefix type.
     *
     * Constructs types of the form: (prefix & ReadonlyNothing) | tp.
     *
     * Intersecting with ReadonlyNothing produces a lower bound of prefix and ReadonlyNothing,
     * which is either ReadonlyNothing or Nothing.
     * Then, creating a union with tp produces either a readonly version of tp
     * (where prefix&ReadonlyNothing =:= ReadonlyNothing), or merely tp
     * (where prefix&ReadonlyNothing =:= Nothing).
     *
     * If the prefix is mutable, or tp is already readonly, then viewpoint adaption
     * will result in tp unchanged. (These conditions are special-cased.)
     */
    def viewpointAdaptField(prefix: Type)(implicit ctx: Context): Type = {
      assert(prefix.exists)
      assert(prefix ne NoPrefix)
      assert(!tp.dealias.stripAnnots.isInstanceOf[MethodicType])
      // todo An idea for pressure-testing: Disable the following line so all field selection types get wrapped in And/OrTypes...
      if (prefix <:< defn.MutableAnyType || defn.ReadonlyNothingType <:< tp) {
        //if ((prefix ne defn.MutableAnyType) || !(defn.ReadonlyNothingType <:< tp)) println(s"!!! no-effect viewpoint adaption on $prefix |> $tp")
        tp  // adaptation does not change tp here
      }
      else {
        OrType(AndType(prefix, defn.ReadonlyNothingType), tp)

        // Attempt 1
        //val tpm = tp.mutability
        //val prefixm = prefix.mutability
        //val adapted = tpm.filter(prefixm.contains(_)) // union of mutabilities
        //tp.withMutability(adapted)
      }
    }

    //def viewpointAdaptPolymorphic?

    //
    // On viewpoint adaptation of methods: Every non-getter method is given a polymorphic receiver type
    //   with a lower bound that respects actual receiver mutability. Assume enclosing class C:
    //  @readonly m(x:S):T@readonly -->>   m[V >: Any <: Any](x:S): T|RoNothing ,
    //                                     C.this:(C|RoNothing)&V
    //  @rothis m(x:S):T@rothis     -->>   m[V >: MutableAny <: Any](x:S): (T|RoNothing)&V ,
    //                                     C.this:(C|RoNothing)&V
    //  @mutable m(x:S):T@mutable   -->>   m[V >: MutableAny <: MutableAny](x:S): (T|RoNothing)&MutableAny ,
    //                                     C.this:(C|RoNothing)&V
    // And at the call site:
    //  y = x.m(...)
    // Cases:
    //  x:C@mutable    y:C@mutable    -->>  No problem
    //  x:C@mutable    y:C@readonly   -->>  No problem
    //  x:C@readonly   y:C@mutable    -->>  Type mismatch
    //  x:C@readonly   y:C@readonly   -->>  No problem
    //
    // @mutabilityOf(this) or @polyread annotations are converted to intersection-type mutabilities.
    // The basic conversion (since these annotations override underlying mutabilities) is:
    //  U @mutabilityOf(this)  -->>  (U | ReadonlyNothing) & V
    //
    // todo alternative notations: @mutabilityOf[T] , @mutabilityOf(x) , @rothis , @polyread
    // todo  key considerations: wrt. nested methods, @mutabilityOf doesn't work because we have no
    // todo    explicit type or term to pass to it.
    //
    // Getter methods (viewpoint adaptation of): Same as ordinary methods with @rothis receiver and result.
    //
    //  m:T     -->>   m[V >: MutableAny <: Any]: (T|RoNothing)&V ,
    //                 C.this=(C|RoNothing)&V
    // At the call site:
    //  y: R = x.m[solve wrt R]
    //
    // Fields: Same as getter methods, but don't bother with the PolyType wrapper.
    // We set the mutability of the result to be an upper bound of the receiver mutability
    // and the result type mutability.
    //
    // todo Key consideration: when do we adapt wrt. given receiver, and when wrt. the prototype?
    // todo Possible answer: Let Scala decide. But we still need to check at the call site
    // todo  to make sure that a non-mutable prefix isn't used to select a method with an incompatible
    // todo  receiver-type parameter.
    //
    // Regardless of whether the type parameter V is resolved with respect to the prototype
    // or the receiver type, the receiver type must still be checked for substitutability
    // with V.
    // The main benefit of using a PolyType in implementation seems to be that we get
    // substitution of V within the body of the method for free.
    //
    // Defaults:
    // Getter methods default to @rothis receiver mutability and @rothis result mutability,
    // unless annotated differently. (I.e., annotations given by the programmer are applied
    // "outside" of any default annotations.)
    //

  }


  // TODO
  // What types go in the "Mutable Lattice" by default (stuff that's OK to wrap in Readonly/Mutable
  //  versus "reference types," which tend to break stuff).
  //

  // TODO
  // (prefixtype & ReadonlyNothing) | tp   to  perform viewpoint adaptation.
  //


  /**
   * A TypeComparer that also compares mutability.
    *
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
      // We have to make a special case for ImportType.
      // Import types do not resolve to TypeRefs. (See discussion under Viewpoint Adaptation.)
      //
      // todo what's a reasonable solution for avoiding the `stable symbol' errors
      // todo  that happen when mutability intersections are added to terms that refer to objects?
      //


      // todo Approach 4:
      //
      /*tp2.dealias.stripAnnots match {
        case tp2d: TypeRef => if (tp2d.symbol eq defn.MutableAnyClass) tp1.widenDealias.stripAnnots match {
            case tp1d: TypeRef =>
              val sym1 = tp1d.symbol
              return !(sym1 is Param) && !((sym1 eq defn.AnyClass) || (sym1 eq defn.ReadonlyNothingClass))
            case _ =>
          }
        case _ =>
      }*/

      // todo Approach 3:
      // This seems to work.
      // However, is it really necessary to do this now that we've modified baseTypeRefOf?
      //
      /*tp2.dealias.stripAnnots match {
        case tp2d: TypeRef => if (tp2d.symbol eq defn.MutableAnyClass) tp1.widenDealias.stripAnnots match {
            case tp1d: TypeRef =>
              val sym1 = tp1d.symbol
              return !((sym1 eq defn.AnyClass) || (sym1 eq defn.ReadonlyNothingClass))
            case _ =>
          }
        case _ =>
      }*/

      // todo Approach 2: is widening really what we want to do with tp2? Widening here may be unnecessarily expensive, since it is done on every call to isSubType.
      //tp2.widenDealias.stripAnnots match {
      //  case tp2w: TypeRef =>
      //    val sym2 = tp2w.symbol
      //    if (sym2 eq defn.MutableAnyClass) tp1.widenDealias.stripAnnots match {
      //      case tp1w: TypeRef =>
      //        val sym1 = tp1w.symbol
      //        return !((sym1 eq defn.AnyClass) || (sym1 eq defn.ReadonlyNothingClass))
      //      case _ =>
      //    }
      //  case _ =>
      //}

      // todo Approach 1: I am suspicious of how underlyingClassRef operates wrt. RefinedTypes.
      //if (tp2.underlyingClassSymbol eq defn.MutableAnyClass) {
        //val sym1 = tp1.widenDealias.stripAnnots.underlyingClassSymbol
        //if (sym1 ne NoSymbol) return !((sym1 eq defn.AnyClass) || (sym1 eq defn.ReadonlyNothingClass))
      //}

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
          val cls11 = tp11.underlyingClassSymbol  // todo change underlyingClassSymbol to something a little more tractable?
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

    /*override def glb(tp1: Type, tp2: Type): Type = {
      if (tp1 eq tp2) tp1
      else if (!tp1.exists) tp2
      else if (!tp2.exists) tp1
      else super.glb(tp1, tp2)
    }

    override def lub(tp1: Type, tp2: Type): Type = {
      if (tp1 eq tp2) tp1
      else if (!tp1.exists) tp1
      else if (!tp2.exists) tp2
      //else if ((tp1 isRef defn.MutableAnyClass) && (tp2 isRef defn.ReadonlyNothingClass))
      else super.lub(tp1, tp2)
    }*/

    /** The greatest lower bound of two types */
    /*override def glb(tp1: Type, tp2: Type): Type = {
      if (tp2 isRef defn.ReadonlyNothingClass) {
        if (tp1 <:< defn.MutableAnyType) NothingType else tp2
      }
      else super.glb(tp1, tp2)
    }*/

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }

  def isAssignable(tp: TermRef, pos: Position)(implicit ctx: Context): Boolean = {
    //println(s"---checking mutability of ${tp.prefix} , == ${tp.prefix <:< defn.MutableAnyType}")
    val prefix =
      if (tp.prefix eq NoPrefix) accessorChainToSymbol(tp.symbol, pos)
      else tp.prefix
    prefix <:< defn.MutableAnyType
  }

  class DotModTyper extends Typer {

    var alreadyStarted = false

    /*override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
      val ddef1 = if (ddef.isGetterLike) {
        val envParamName = envRefTypeName(ddef.name)
        val bounds = untpd.TypeBoundsTree(untpd.TypeTree(defn.MutableAnyType), untpd.TypeTree(defn.AnyType)).withPos(ddef.pos) // reduction of: typeParamBounds(name)  todo mutability bounds
        val envParam = untpd.TypeDef(envParamName, Nil, bounds).withMods(Modifiers(Param)).withPos(ddef.pos) // c.f. typeParam in typeParamClause method. todo When doing this for a classdef, check modifiers function for more mods

        val envSym = ctx.newSymbol(sym, envParam.name, Param, typed(bounds).tpe)
        envParam.putAttachment(SymOfTree, envSym)
        ctx.scope.openForMutations.enter(envSym)

        untpd.cpy.DefDef(ddef)(ddef.name, envParam :: ddef.tparams, ddef.vparamss, ddef.tpt, ddef.rhs)
      } else ddef
      super.typedDefDef(ddef1, sym)
    }*/


    //
    // We override typedUnadapted to find untyped DefDefs. Then we add a type parameter named
    // "EnvRef_m" to the type parameter list, where m is the name of the method. We put _m in
    // the type parameter name to distinguish it from the EnvRef of enclosing methods.
    //
    /*override def typedUnadapted(initTree: untpd.Tree, pt: Type)(implicit ctx: Context): Tree = {
      //
      // We call expanded and getAttachment to make sure we're looking at exactly the
      // same tree as in the super implementation.
      val xtree = expanded(initTree)
      xtree.getAttachment(TypedAhead) match {
        case None =>
          xtree match {
            case xtree: untpd.TypeDef =>
              if (xtree.name.toString == "X" || xtree.name.toString == "EnvRef_n") {
                true
              }
            case xtree: untpd.DefDef if xtree.isGetterLike =>
              //
              // For every DefDef tree, we add a type parameter.
              //
              /*val paramBound = untpd.TypeBoundsTree(untpd.TypeTree(defn.MutableAnyType), EmptyTree)
              val envParam = untpd.TypeDef(typeName("EnvRef_" + xtree.name), paramBound)
              envParam.withMods(untpd.Modifiers(Param|Synthetic))
              enterSymbol(createSymbol(envParam))
              //index(envParam)

              val xtree1 = untpd.cpy.DefDef(xtree)(xtree.name, envParam :: xtree.tparams, xtree.vparamss, xtree.tpt, xtree.rhs)
              //
              // Copy all attachments to new tree.
              //
              xtree.allAttachments.foreach { case (k, v) =>
                xtree1.pushAttachment(k, v)
              }*/

              val envParamName = envRefTypeName(xtree.name)
              val bounds = untpd.TypeBoundsTree(untpd.TypeTree(defn.MutableAnyType), untpd.TypeTree(defn.AnyType)).withPos(xtree.pos) // reduction of: typeParamBounds(name)  todo mutability bounds
              val envParam = untpd.TypeDef(envParamName, Nil, bounds).withMods(Modifiers(Param)).withPos(xtree.pos) // c.f. typeParam in typeParamClause method. todo When doing this for a classdef, check modifiers function for more mods
              //index(envParam)
              //enterSymbol(createSymbol(envParam))
              //recordSym(ctx.newSymbol(xtree.attachment(SymOfTree), envParam.name, Param, typed(bounds).tpe), envParam)
              val envSym = ctx.newSymbol(xtree.attachment(SymOfTree), envParam.name, Param, typed(bounds).tpe)
              //envParam.pushAttachment(SymOfTree, envSym)

              val xtree1 = untpd.cpy.DefDef(xtree)(xtree.name, envParam :: xtree.tparams, xtree.vparamss, xtree.tpt, xtree.rhs)
              //
              // Copy all attachments to new tree.
              //
              xtree.allAttachments.foreach { case (k, v) =>
                xtree1.putAttachment(k, v)
              }

              xtree1.tparams.head.putAttachment(SymOfTree, envSym)

              val att = xtree1.tparams.head.attachment(SymOfTree)

              return super.typedUnadapted(xtree1, pt)
            case _ =>  // not a tree we care about here, so fall through
          }
        case _ =>  // this tree has already been typed, so we do nothing here
      }
      super.typedUnadapted(initTree, pt)
    }*/


    //override def selectionType(site: Type, name: Name, pos: Position)(implicit ctx: Context): Type = {
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
      // This code is fragile. We only expect some combination of AndType, OrType, and
      // TermRef. Other types we leave to super#selectType. If the AndType/OrType is wrapped
      // inside anything more complicated than TermRef, the implementation here may fail.
      // (And does fail. Testing uncovered an inability to deal correctly with both TermRefs
      // and ExprTypes, and possibly others.)
      // Second, calls to Type#findMember from other places may fail if they have underlying
      // AndTypes or OrTypes with mutability information.
      //
      // A third possibility is to special-case OrTypes containing ReadonlyNothing within
      // findMember itself. The compiler shape does not seem to lend itself well to giving
      // special meaning to some classes, as we are doing here with ReadonlyNothing.
      //
      //site match {
        /*case OrType(site1, site2) =>
          if (site1.underlyingClassSymbol eq defn.ReadonlyNothingClass)
            return selectionType(site2, name, pos)
          if (site2.underlyingClassSymbol eq defn.ReadonlyNothingClass)
            return selectionType(site1, name, pos)
        case AndType(site1, site2) =>
          if (site1.underlyingClassSymbol eq defn.MutableAnyClass)
            return selectionType(site2, name, pos)
          if (site2.underlyingClassSymbol eq defn.MutableAnyClass)
            return selectionType(site1, name, pos)*/
        /*case site: TermRef =>
          return selectionType(site.underlying match {
            case mt: MethodType if mt.paramTypes.isEmpty && (site.symbol is Stable) => mt.resultType
            case tp1 => tp1
          }, name, pos)*/
        //case _ =>
      //}

      //super.selectionType(site, name, pos)
    //}


    def convertType(tpe: Type, tree: Tree)(implicit ctx: Context): Type = {

      if (!alreadyStarted) {
        if (false) {
          val t0 = OrType(tpe, defn.ReadonlyNothingType)
          val t1 = AndType(t0, defn.ReadonlyNothingType)
          println(s"($t1 <:< MutableAny == " + (t1 <:< defn.MutableAnyType))

          var _nxt = 0
          def nxt() = { _nxt += 1; _nxt.toString + " " }

          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(defn.ReadonlyNothingType, true) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(defn.ReadonlyNothingType, false) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(tpe, true) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(tpe, false) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(t0, true) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(t0, false) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(t1, true) )
          println( nxt() + defn.MutableAnyClass.baseTypeRefOf(t1, false) )


          /*println(s"($t0 <:< MutableAny == " + (t0 <:< defn.MutableAnyType))
          println()
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
          */
        }
        alreadyStarted = true
      }

      //
      // Don't do anything with import types.
      // Other places in the compiler expect references to import types to not have And/OrTypes wrapping them.
      // Furthermore, what does it mean for an import type to have a mutability?
      // An import type just seems to the be the underlying type of an Import tree.
      // As far as I know, an import reference cannot be used inside an ordinary expression
      // or assignment. Therefore, the mutability of an import type should not matter.
      //
      if (tpe.widenDealias.stripAnnots.isInstanceOf[ImportType]) return tpe

      val tpe0 = tpe match {
        case tpe: AnnotatedType => convertAnnotationToType(tpe)
        case tpe: TermRef =>
          //viewpointAdaptTermRef(tpe)
          //
          // If we're selecting a method, then just check that the receiver mutability matches
          // the declared receiver mutability. We do not do viewpoint adaptation on methodic
          // types ourselves--we let the constraint solver take care of that.
          //
          // If we're selecting a field, then we perform viewpoint adaptation with respect to
          // the prefix mutability.
          //
          // If we're selecting a variable local to the current method, then viewpoint adaptation
          // unnecessary because the local frame reference is assumed mutable.
          //
          if (tpe.symbol.is(Method)) {
            tpe
          } else if (tpe.prefix eq NoPrefix) {
            val prefix = accessorChainToSymbol(tpe.symbol, tree.pos) // find the mutability of the outer accessor chain to this symbol's environment
            if (prefix <:< defn.MutableAnyType && (prefix ne defn.MutableAnyType)) assert(false, s"!!! bad test of $prefix")
            val tpe0 = tpe.viewpointAdaptField(prefix)
            tpe0
          } else {
            // todo is it ok to assume prefix mutability is already taken care of by the time we get here?
            //println("---adapting " + tpe)
            val tpe0 = tpe.viewpointAdaptField(tpe.prefix)
            //println(s"s---adapt ${tpe.symbol} in ${ctx.owner} to $tpe0")
            //println("---to " + tpe0)
            //println()
            tpe0
          }

        case tpe: ThisType => viewpointAdaptThisType(tpe)
        case tpe: SuperType => viewpointAdaptSuperType(tpe)
        //case tpe: MethodicType => makePolyTypeWithReceiver(tpe)
        case _ => tpe
      }

      // CAREFUL! DenotingTrees strip annotations when calling denot.

      tpe0
    }


    //override def assignType(tree0: untpd.TypeApply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    override def typedTypeApply(tree0: untpd.TypeApply, pt: Type)(implicit ctx: Context): Tree = {
      //
      // See if we need to add an argument for the environment type to this type application.
      //
      // A type argument needs to be added only if the called method is getter-like,
      //  and exactly one argument is missing.
      //
      // see pt.instantiate(argTypes) in super.assignType for how type args get instantiated
      //
      var tree1 = tree0
      val typedArgs = tree0.args mapconserve (typedType(_))
      val typedFn = typedExpr(tree0.fun, ProtoTypes.PolyProto(typedArgs.tpes, pt))

      //println(s"--- Testing getter-likeness of ${typedFn.symbol} ")
      if (typedFn.symbol.isGetterLike) {
        typedFn.tpe.widen match {
          case pt: PolyType =>

            // Get the receiver's mutability
            val envType = methPart(typedFn).tpe.asInstanceOf[TermRef].prefix
            //println(s"---call to ${typedFn.symbol} with receiver type $envType")

            //println(s"--- ${typedFn.symbol} is getter-like in application")
            //
            // Add the argument only if we are missing exactly one argument
            // todo Check receiver type... what ought to be done to account for receiver mutability?
            //
            if (typedArgs.length == pt.paramNames.length - 1) {

              // Set environment arg to receiver's mutability
              val envArg = untpd.TypeTree(
                if (envType eq NoPrefix) defn.MutableAnyType
                else AndType(envType, defn.ReadonlyNothingType)
              )
              //println(s"--- adding type argument in call to ${typedFn.symbol}")
              tree1 = untpd.cpy.TypeApply(tree0)(tree0.fun, envArg :: tree0.args)
            }
            else if (envType ne NoPrefix) {
              // Check that the receiver mutability matches the first type parameter
              //println(s"--- Checking $envType vs. ${typedArgs.head.tpe}")
              //println(s"--- ${envType <:< typedArgs.head.tpe}")
              if (!(envType <:< typedArgs.head.tpe)) {
                 val msg = d"""Receiver mutability mismatch:
                    | found   : $envType
                    | required: ${typedArgs.head.tpe}""".stripMargin
                return errorTree(tree0, msg)
              }
            }
          case _ =>
        }
      }

      /*val tree1 = typedFn.tpe.widen match {
        case pt: PolyType if pt.termSymbol.isGetterLike =>
          println(s"--- ${pt.termSymbol} is getter-like in application")
          //
          // Add the argument only if we are missing exactly one argument
          // todo Check receiver type... what ought to be done to account for receiver mutability?
          //
          if (typedArgs.length == pt.paramNames.length - 1) {
            println(s"--- adding type argument in call to ${pt.termSymbol.name}")
            val envArg = untpd.TypeTree(defn.MutableAnyType)
            untpd.cpy.TypeApply(tree0)(tree0.fun, envArg :: tree0.args)

            // add a type arg if getter-like. todo only add type arg if argument count mismatch would otherwise occur??? cf. assignType?
            //if (typedArgs.length <= pt.paramBounds.length)
            //  typedArgs = typedArgs.zipWithConserve(pt.paramBounds)(adaptTypeArg)
            //val argTypes = typedArgs.tpes
            //if (sameLength(argTypes, pt.paramNames) || ctx.phase.prev.relaxedTyping) pt.instantiate(argTypes)
            //else errorType(d"wrong number of type parameters for ${typedFn.tpe}; expected: ${pt.paramNames.length}", tree0.pos)
          } else tree0
        case _ => tree0
      }*/

      /*//println(s"--- in typedTypeApply ${tree0.fun} ")
      val tree1 = if (tree0.fun.isGetterLike) {
        //
        // Add environment type argument to type applications.
        // see pt.instantiate(argTypes) in super.assignType for how type args get instantiated
        //
        val envParam = untpd.TypeTree(defn.MutableAnyType)
        untpd.cpy.TypeApply(tree0)(tree0.fun, envParam :: tree0.args)
      } else tree0*/
      //super.assignType(tree1, fn, typed(envParam) :: args)
      // todo override assignType to get a better error message for wrong number of type parameters?
      super.typedTypeApply(tree1, pt)
    }

    /*override def typed(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
      val tree1 = tree match {
        case _ => tree
      }
      super.typed(tree1, pt)
    }*/

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      //
      // Add environment type argument to type applications.
      // todo here, or after super.adapt?
      //
      /*val tree01 = tree0 match {
        case tree0: TypeApply =>
          val envParam = typedTypeBoundsTree(untpd.TypeBoundsTree(untpd.TypeTree(defn.MutableAnyType), untpd.TypeTree(defn.MutableAnyType)))
          tpd.cpy.TypeApply(tree0)(tree0.fun, envParam :: tree0.args)
        case _ => tree0
      }*/

      val tree = super.adapt(tree0, pt, original)

      if (!tree.tpe.exists || tree.tpe.isError) return tree

      //
      // Check Assignability
      //
      tree match {
        case tree: Assign =>
          val termRef = tree.lhs.tpe.underlyingTermRef.asInstanceOf[TermRef]
          if (!isAssignable(termRef, tree.pos))
            return errorTree(tree, s"${termRef.symbol.name} is not assignable here")
        case _ =>
      }

      //
      // r = o.m(...)
      // where m(...): RT
      // converted to: m[T >: MutableAny <: Any](...): (RT | RONothing) & T
      //
      // (RT | RONothing) & T <: type(r)
      //
      // type(r) is the prototype
      //

//      tree.tpe match {
//        case
//      }

      val tpe1 =
        //if (tree.isInstanceOf[Select] || tree.isInstanceOf[Ident])
          convertType(tree.tpe, tree)
        //else tree.tpe

      val tree1 =
        //if (false) tree
        if (tpe1 eq tree.tpe) tree
        else {
          val tree1 = tree.withType(tpe1)
          if ((ctx.mode is Mode.Pattern) || tpe1 <:< pt) tree1
          else {
            err.typeMismatch(tree1, pt)
          }
        }

      tree1
    }

    /**
     * This is the method that returns the type of a DefDef symbol during completion.
     *
     * If we want to add an extra polymorphic type parameter, this is where we would
     * do it.
      *
      * NOTE: Actually, we can't just add a type parameter here.
      *   The DefDef tree is not modified here, so the tree pickler never sees the new parameter,
      *   causing uses of the type parameter to be interpreted as forward references by the pickler.
      *   It seems I will have to modify the DefDef tree directly.
      *
      * @param ddef
     * @param sym
     * @param ctx
     * @return
     */
    /*override def defDefSig(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context): Type = {
      if (ddef.isGetterLike) {

        //  FROM ClassfileParser#parseMember
        //val member = ctx.newSymbol(
        //  getOwner(jflags), name, sflags, memberCompleter, coord = start)
        //getScope(jflags).enter(member)

        //  FROM Definitions#newTypeField
        //scope.enter(newSymbol(cls, name, flags, TypeBounds.empty))

        //   FROM Typer#typedBind
        //val flags = if (tree.isType) BindDefinedType else EmptyFlags
        //val sym = ctx.newSymbol(ctx.owner, tree.name, flags, body1.tpe, coord = tree.pos)

        // see Symbols#newTypeParams
        // used in tpd.polyDefDef
        // used by tpd.DefDef


        val envParamName = envRefTypeName(ddef.name)
        val bounds = untpd.TypeBoundsTree(untpd.TypeTree(defn.MutableAnyType), untpd.TypeTree(defn.AnyType)).withPos(ddef.pos) // reduction of: typeParamBounds(name)  todo mutability bounds
        val envParam = untpd.TypeDef(envParamName, Nil, bounds).withMods(Modifiers(Param)).withPos(ddef.pos) // c.f. typeParam in typeParamClause method. todo When doing this for a classdef, check modifiers function for more mods

        //val flags = EmptyFlags
        //recordSym(ctx.newSymbol(ctx.owner, envParam.name, flags, typed(bounds).tpe), envParam)

        //enterSymbol(createSymbol(envParam))

        val ddef1 = untpd.cpy.DefDef(ddef)(ddef.name, envParam :: ddef.tparams, ddef.vparamss, ddef.tpt, ddef.rhs)
        val defsig = super.defDefSig(ddef1, sym)

        originalTyper.enterSymbol(defsig.typeParamNamed(envParamName))

        defsig
      }
      else super.defDefSig(ddef, sym)
    }*/

    var originalTyper: Typer = null

    override def newLikeThis: Typer = {
      val typer1 = new DotModTyper
      typer1.originalTyper = this
      typer1
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

// FUTURE WORK
//
// A method is parallelizable if it does not write to its prestate or read from
// its prestate. This condition can be implemented by adding a "readable" permission
// as well as a "mutable" permission. However, reading of immutable objects may
// be allowed (in the object-immutability sense).
//