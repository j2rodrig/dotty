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
import core.Types._
import transform.TreeTransforms._
import typer._
import typer.ErrorReporting._

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
      (defn.MutableType <:< tp) && !(defn.AnyType <:< tp)
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
      if ((upperConcreteBound eq defn.MutableAnnot) && tp.isInstanceOf[ValueType]) AndType(tp, defn.MutableType)
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
      if (!isFromMutableClass && tp.isInstanceOf[ValueType]) AndType(tp, defn.RoThisType)
      else tp
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
        AndType(tp, defn.MutableType)
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
        if (tp.cls.derivesFrom(defn.MutableClass)) ConcreteMutability(defn.MutableAnnot, _fromMutableClass = true)
        else if (tp.cls.derivesFrom(defn.RoThisClass)) ReceiverMutability(ctx.owner.skipWeakOwner, _fromMutableClass = true)
        else if (tp.cls eq defn.AnyClass) ConcreteMutability(defn.ReadonlyAnnot)   // assume Any is Readonly
        else ConcreteMutability(defn.MutableAnnot)  // default other classes to Mutable

      case tp: RefinedType =>
        // Refinements of Any are mutable, although Any itself is readonly.
        if (tp.parent.isRef(defn.AnyClass)) ConcreteMutability(defn.MutableAnnot)
        else tp.parent.getMutability(spec, maxBoundsRecursion)

      case AndType(tp1, tp2) => tp1.getMutability(spec, maxBoundsRecursion) glb tp2.getMutability(spec, maxBoundsRecursion)

      case OrType(tp1, tp2) => tp1.getMutability(spec, maxBoundsRecursion) lub tp2.getMutability(spec, maxBoundsRecursion)

      case tp: MethodicType => tp.finalResultType.getMutability(spec, maxBoundsRecursion)

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
   * A TypeComparer that also compares mutability.
   * @param initCtx the context the comparer operates within
   */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    /**
     * We keep around an ordinary type comparer, which is used when a non-mutability-related
     * type comparison is required.
     */
    //val ordinaryTypeComparer = new TypeComparer(initCtx)

    //override def isSubType(tp1: Type, tp2: Type): Boolean = {
    override def topLevelSubType(tp1: Type, tp2: Type): Boolean = {

      if (ctx.phase.erasedTypes || tp2.isInstanceOf[ProtoType] || (!tp1.requiresMutabilityComparison && !tp2.requiresMutabilityComparison)) {
        //super.isSubType(tp1, tp2)
        super.topLevelSubType(tp1, tp2)
      } else {
        val m1 = tp1.getMutability(Hi)
        val m2 = tp2.getMutability(Lo)


        //if (!(m1 <:< m2)) println("MUTABILITY COMPARED FALSE: " + m1 + " <:< " + m2)

        //val bothFromClass = m1.isFromMutableClass && m2.isFromMutableClass
        //val eitherFromClass = m1.isFromMutableClass || m2.isFromMutableClass
        //val insertIntersection = eitherFromClass && !bothFromClass

        // We call addMutableIntersectionIfRequired to ensure that T@mutable <:< T with Mutable
        //val tpe1 = if (insertIntersection) m1.addMutableIntersectionIfRequired(tp1) else tp1  // todo reenable when ready
        //val tpe2 = if (insertIntersection) m2.addMutableIntersectionIfRequired(tp2) else tp2


        val tpe1 = if (m2.isFromMutableClass) m1.addMutableIntersectionIfRequired(tp1) else tp1  // tp1
        val tpe2 = tp2

        //super.isSubType(tpe1, tpe2) && m1 <:< m2  // todo reenable when ready
        super.topLevelSubType(tpe1, tpe2) && m1 <:< m2  // todo reenable when ready
      }
      //super.topLevelSubType(tp1, tp2) && (
      //  !tp1.exists || !tp2.exists || tp2.isInstanceOf[ProtoType] ||
      //  tp1.getMutability <:< tp2.getMutability
      //  )
    }

    //override def isSubType(tp1: Type, tp2: Type): Boolean = {
    //  super.isSubType(tp1, tp2) && tp1.getMutability <:< tp2.getMutability
    //}

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }


  class DotModTyper extends Typer {

    /// The function tree underlying the given tree (always returns a Select or Ident).
    /*def fun(tree: Tree): Tree = tree match {
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
    }*/

    def viewpointAdaptTermRef(tp: TermRef)(implicit ctx: Context): Type = {
      val prefixMutability =
        if (tp.prefix eq NoPrefix) getOuterAccessPathMutability(tp.symbol)
        else tp.prefix.getMutability(Hi)   // todo adaptation with abstract mutability types?
      viewpointAdapt(prefixMutability, tp)
    }

    def viewpointAdaptThisType(tp: ThisType)(implicit ctx: Context): Type = {
      val prefixMutability = getOuterAccessPathMutability(tp.cls)
      viewpointAdapt(prefixMutability, tp)
    }

    def viewpointAdaptSuperType(tp: SuperType)(implicit ctx: Context): Type = {
      val prefixMutability = getOuterAccessPathMutability(tp.thistpe.asInstanceOf[ThisType].cls)
      viewpointAdapt(prefixMutability, tp)
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      val tree = super.adapt(tree0, pt, original)

      val tpe1 = tree.tpe match {
        case tpe: TermRef => viewpointAdaptTermRef(tpe)
        case tpe: ThisType => viewpointAdaptThisType(tpe)
        case tpe: SuperType => viewpointAdaptSuperType(tpe)
        case tpe => tpe
      }

      if (tpe1 eq tree.tpe) tree
      else {
        val tree1 = tree.withType(tpe1)
        if ((ctx.mode is Mode.Pattern) || tpe1 <:< pt) tree1
        else err.typeMismatch(tree1, pt)
      }

      /*val dontCheck = (
        pt == WildcardType
          || !tree1.tpe.exists
          || pt.isInstanceOf[ProtoType]
          // || tree1.tpe <:< defn.AnyValType   // classes may extend AnyVal, so we do have to check
        )

      if(dontCheck) tree1 else {
        // TODO check that tree.tpe <:< pt wrt. mutability
        tree1
      }*/
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
