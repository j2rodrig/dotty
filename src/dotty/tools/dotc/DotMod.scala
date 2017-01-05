package dotty.tools
package dotc

import ast.{tpd, untpd}
import core._
import core.Annotations._
import core.Contexts._
import core.Decorators._
import core.Denotations._
import core.Flags._
import core.Names._
import core.Symbols._
import core.SymDenotations._
import core.TypeOpHooks
import core.Types._
import transform._
import transform.TreeTransforms._
import typer._
import typer.ErrorReporting._
import util._

import scala.collection.Set


object DotMod {

  // Flags
  val enableFrontEnd: Boolean = true
  val enableRefChecks: Boolean = true

  // FRONT-END INITIALIZATION

  def customInit(ctx: FreshContext): FreshContext = {
    if (enableFrontEnd) {
      ctx.setTypeComparerFn(new DotModTypeComparer(_))
      ctx.setTypeOpHooks(new DotModTypeOpHooks(ctx))
      ctx.setTyper(new DotModTyper)
    }
    ctx
  }

  // DECORATORS

  implicit class SymbolDecorator(val sym: Symbol) extends AnyVal {
    /**
      * Gets annotations on a symbol without forcing the symbol's completion.
      * @param filterByNamesIfUncompleted A list of annotation names to filter by -- if
      *                                   the symbol is uncompleted, then only annotations
      *                                   named in this list will be typed and returned.
      */
    def annotationsWithoutCompleting(filterByNamesIfUncompleted: List[TypeName])(implicit ctx: Context): List[Annotation] = sym.infoOrCompleter match {
      case inf: Typer#Completer =>
        // we've got a completer here, so look at the original untyped tree, and type the annotations directly
        // See Namer#addAnnotations to see how Dotty types annotations. (Also Typer#completeAnnotations.)
        untpd.modsDeco(inf.original.asInstanceOf[untpd.MemberDef]).mods.annotations
          .filter(filterByNamesIfUncompleted contains _.annotationClassName)
          .map { annotTree =>
            // We look up the typer/context of the completer to do the annotation typing.
            // See <DISCUSSION:ANNOTATION-TYPER-CONTEXT> in notes.
            var typeInContext = inf.originalOuterCtx.outer  // We also type in the next-outer context. See <DISCUSSION: CONTEXT-FOR-ANNOT-TYPING> in notes.
            Annotation(typeInContext.typer.typedAnnotation(annotTree)(typeInContext))  // create a typed annotation
          }
      case _: LazyType =>
        // See <DISCUSSION:LAZY-NON-COMPLETER> in notes.
        assert(!(sym is Method) && !sym.isClass, em"Unexpected LazyType as completer for $sym")
        Nil // if I really do want to process annotations on fields or variables, I maybe should force completions here (instead of returning Nil).
      case _ =>  // no completer, so return already-typed annotations.
        sym.annotations
    }

    /** Finds out whether a symbol is a parameterless method (or not a method at all) without unnecessarily forcing the symbol's completion. */
    def isParameterless(implicit ctx: Context): Boolean = sym.infoOrCompleter match {
      case inf: Typer#Completer =>
        // we've got a completer here, so look at the original untyped tree, and check the value parameter list
        inf.original match {
          case original: untpd.DefDef =>
            original.vparamss.isEmpty
          case _ =>
            true
        }
      case _: LazyType if !(sym is Method) =>  // not sure if we need this case, but I put it here anyway to avoid forcing unnecessary completions (ref: case LazyType in annotationsWithoutCompleting)
        true
      case inf =>  // no completer, so look at the type.
        inf.isParameterless
    }

    def isField(implicit ctx: Context): Boolean = {
      sym.denot.info.isInstanceOf[ValueType]
    }

    def isPure(implicit ctx: Context): Boolean = {
      if (sym.isConstructor)
        sym.owner.isPure
      else
        assumePure(sym) ||
        annotationsWithoutCompleting(pureAnnotationNames).exists(_.symbol eq defn.PureAnnot)
    }

    /** Finds the innermost symbol that is annotated @pure in this symbol's ownership chain.
      * NoSymbol if no @pure symbol exists in the ownership chain. */
    def purityBoundary(implicit ctx: Context): Symbol = {
      if ((sym eq NoSymbol) || (sym is Package)) NoSymbol
      else if (isPure) sym
      else sym.owner.purityBoundary
    }

    def isInside(boundary: Symbol)(implicit ctx: Context): Boolean = {
      if (sym eq NoSymbol) false
      else if ((sym is PackageClass) && !(boundary is PackageClass)) false
      else if (sym eq boundary) true
      else if (sym.isPure && (sym.derivesFrom(boundary) || boundary.derivesFrom(sym))) true
      else sym.owner.isInside(boundary)
    }

    def isContainedInPurityBoundaryOf(sym2: Symbol)(implicit ctx: Context): Boolean = {
      val boundary = sym2.purityBoundary
      if (boundary eq NoSymbol) true   // sym2 has no boundary (or alternatively, its boundary is infinitely high on the owners chain).
      else isInside(boundary)
    }
  }

  implicit class TypeDecorator(val tp: Type) extends AnyVal {

    def withoutMutabilityMembers(implicit ctx: Context): Type =
      dropRefinementsNamed(tp, List(MutabilityMemberName, PrefixMutabilityName))

    /** Finds a version of tp that's either a covariant alias, or a type bounds (if already a real type bounds). */
    def covAlias(implicit ctx: Context): TypeBounds = tp match {
      case tp: TypeAlias => tp.withVariance(1)
      case tp: TypeBounds => tp
      case _ => TypeAlias(tp, 1)
    }
  }

  implicit class UntypedTreeDecorator(val tree: untpd.Tree) extends AnyVal {
    // Finds the class name of an annotation application tree.
    def annotationClassName: Name = untpd.methPart(tree) match {
      case tr: untpd.Ident => tr.name
      case tr: untpd.New => tr.tpt.annotationClassName
      case tr: untpd.Select => tr.qualifier.annotationClassName
      case _ =>
        assert(false, s"Expected an annotation-class application tree, got $tree")
        typeName("<none>")
    }

    // HACK!!! If the tree has a type, returns that tree. Otherwise, returns a version of the tree that is typed in the given context.
    def forcedTyped(implicit ctx: Context): tpd.Tree = {
      if (tree.hasType)
        tree.asInstanceOf[tpd.Tree]
      else
        ctx.typer.typed(tree)
    }
  }


  // CONSTANTS

  val MutabilityMemberName = typeName("__MUTABILITY__")
  val PrefixMutabilityName = typeName("__PREFIX_MUTABILITY__")
  def ReadonlyType(implicit ctx: Context) = TypeAlias(defn.ReadonlyAnnotType, 1)  // info for a readonly mutability member
  def MutableType(implicit ctx: Context) = TypeAlias(defn.MutableAnnotType, 1)    // info for a mutable mutability member

  class BoundSpec
  object LowerBound extends BoundSpec
  object UpperBound extends BoundSpec

  lazy val defaultPureArrayMethods: List[TermName] = List (
    "length", "apply", "clone"
  ).map(termName).map(NameTransformer.encode)

  val methodNamesDefaultingToPolyreadReceiver: List[TermName] = List (
    "==", "!=", "asInstanceOf", "isInstanceOf",
    "clone", "eq", "equals", "finalize", "getClass", "hashCode", "ne", "toString"
  ).map(termName).map(NameTransformer.encode)

  val receiverAnnotationNames = List("polyread", "readonly", "mutable", "mutabilityOf", "mutabilityOfRef").map(typeName)
  val finalAnnotationNames = List("asFinal").map(typeName)
  val typeviewAnnotationNames = List("asType").map(typeName)
  val pureAnnotationNames = List("pure", "pureAt").map(typeName)
  val outerMutabilityAnnotNames = List("pure", "mutableIn").map(typeName)

  val allCustomAnnotations = receiverAnnotationNames ::: finalAnnotationNames ::: typeviewAnnotationNames ::: pureAnnotationNames


  /******************
    * TYPE COMPARER *
    ******************
    * A TypeComparer that also compares mutability.
    * @param initCtx the context the comparer operates within
    */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    def isSubMutability(mut1: Type, mut2: Type): Boolean = {

      // Try to reduce the mutabilities to simpler forms.
      def reduce(mut: Type, whichBound: BoundSpec): Type = mut match {
        case TypeRef(prefix@TermRef(_,_), MutabilityMemberName) => mutabilityOf2(prefix, whichBound)
        case TypeRef(prefix, MutabilityMemberName) if isAbstract(prefix) => goAbstract(prefix.dealias.stripAnnots)
        case _ => mut
      }
      val m1 = reduce(mut1, UpperBound)
      val m2 = reduce(mut2, LowerBound)

      //if (m1 ne mut1) System.err.println(em"$mut1 reduced to $m1")
      //if (m2 ne mut2) System.err.println(em"$mut2 reduced to $m2")

      if ((m1 ne mut1) || (m2 ne mut2))
        // recurse until mutability reductions reach a steady state
        isSubMutability(m1, m2)

      else
        // Compare mutabilities.
        (m1 eq m2) || (m1 eq defn.MutableAnnotType) || (m2 eq defn.ReadonlyAnnotType) || m1.isError || m2.isError || {  // simple cases.
          m2 match {   // if both m1 and m2 select __MUTABILITY__, they are equivalent if they have equivalent prefixes.
            case TypeRef(prefix2, MutabilityMemberName) => m1 match {
              case TypeRef(prefix1, MutabilityMemberName) =>
                super.isSubType(prefix1, prefix2) && super.isSubType(prefix2, prefix1)
              case _ =>
                //System.err.println(em"False mutability: $mut1 <: $mut2")
                false
            }
            case _ =>
              //System.err.println(em"False mutability: $mut1 <: $mut2")
              false
          }
        }
    }

    def isMutability(tp: Type): Boolean = tp match {
      case tp: TypeRef =>
        (tp eq defn.MutableAnnotType) || (tp eq defn.ReadonlyAnnotType) || (tp.name eq MutabilityMemberName)
      case _ =>
        false
    }

    def isMutabilityRefinement(tp: Type): Boolean = tp match {
      case tp: RefinedType => (tp.refinedName eq MutabilityMemberName) || isMutabilityRefinement(tp.parent)
      case _ => false
    }

    override def isSubType(tp1: Type, tp2: Type): Boolean = {

      // Pass basic cases. We assume that Any is a readonly type.
      if ((tp1 eq tp2) || (tp2 eq defn.AnyType) || (tp2 eq WildcardType) || (tp1 eq defn.NothingType))
        return true

      // Since we do custom logic for term types only, pass the prototypes, class infos, bounds, wilds, and errors to the default subtype logic.
      if ((tp2 eq NoType) || tp2.isInstanceOf[ProtoType] || tp2.isInstanceOf[TypeType] || tp2.isInstanceOf[WildcardType] || tp2.isError)
        return super.isSubType(tp1, tp2)

      // Basic approach: We perform a mutability comparison wherever it may be needed.
      //
      // Rationale:
      //    We are defaulting "type __MUTABILITY__ >: mutable <: readonly" inside all classes,
      //   refining "__MUTABILITY__ = mutable" for all class references p.C, "__MUTABILITY__ = readonly" for scala.Any,
      //   declared mutability for all p.C.this/super,
      //   abstract mutability for abstract types.
      //   We want to do this without actually calling findMember. Since __MUTABILITY__ doesn't always exist literally,
      //   we need to take control in all cases where a query for that member may take place.
      //

      val tp1wd = tp1.widenDealias

      // The mutability cases: If we're comparing mutabilities rather than non-mutability types...
      if (isMutability(tp2)) {
        if (isMutability(tp1wd)) isSubMutability(tp1wd, tp2)  //
        else super.isSubType(tp1wd, tp2)
      }
      else if (isMutability(tp1wd)) {
        super.isSubType(tp1wd, tp2)
      }

      // The refinement case: If either type refines __MUTABILITY__, then compare mutabilities and non-mutability types.
      else if (isMutabilityRefinement(tp1wd) || isMutabilityRefinement(tp2)) {
        val mut1wd = mutabilityOf2(tp1wd, UpperBound)
        val mut2 = mutabilityOf2(tp2, LowerBound)
        isSubType(mut1wd, mut2) && {
          // continue the subtyping check on underlying types. To make sure we don't recurse badly, we strip the mutability members first.
          val dropped1 = dropRefinementsNamed(tp1wd, List(MutabilityMemberName))
          val dropped2 = dropRefinementsNamed(tp2.widen, List(MutabilityMemberName))
          super.isSubType(dropped1, dropped2)
        }
      }

      else
        super.isSubType(tp1, tp2)
    }

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }

  /// Finds the first owner of s that is either a class or non-weak. If s itself is a class or non-weak, returns s.
  def skipWeakNonClassOwners(s: Symbol)(implicit ctx: Context): Symbol =
    if (!s.isClass && s.isWeakOwner) skipWeakNonClassOwners(s.owner) else s

  /*
  Algorithm:
    First, Search for a pair of symbols that meets the following criteria:
      1. The first element of the pair is a method. (Or other non-weak owner.)
      2. The second element of the pair is a class.
      3. The class is a base class of C.
      4. The class is an owner of the method.
      5. There are no non-weak owners of the method that are also owned by the class.
    Second, find and return the receiver mutability declared on the first element of the pair.
   */
  final def findThisMutability(refSym: Symbol, ofClass: Symbol)(implicit ctx: Context): Type = {
    //assert(refSym.exists, em"Attempt to find the mutability of ${ofClass.name}.this outside of $ofClass")
    assert(ofClass.isClass, em"Expected a class in findThisMutability, got $ofClass")
    if (ofClass derivesFrom refSym)  // if we're already at the class we're looking for, return @mutable
      return defn.MutableAnnotType
    if (!refSym.exists)  // if we're outside of the class we're looking for, assume @mutable
      return defn.MutableAnnotType

    // Find the owning class of refSym
    val classOfSym = skipWeakNonClassOwners(refSym.owner)
    if (!classOfSym.isClass) return findThisMutability(classOfSym, ofClass)   // owner isn't a class, so look at the owner of the owner

    if (ofClass.derivesFrom(classOfSym))  // is the owning class a base of the class we're looking for?
      declaredReceiverType(refSym)   // found it!
    else
      findThisMutability(classOfSym, ofClass)   // keep looking... starting at fromSym's owner
  }

  /// Translates any @mutableIn[C] or @pure annotations into
  def findOuterThisMutability(refSym: Symbol, ofClass: Symbol)(implicit ctx: Context): Type = {
    ???
  }

  /**
    * If tpe is a refinement of a type parameter, returns the refinedInfo's alias type.
    * Note: We shouldn't be getting a type bounds here. (I'm assuming an instantiated type argument is always a TypeAlias because it should be fully defined.)
    * Otherwise, returns NoType.
    */
  def getLastTypeArgOf(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case RefinedType(_, _, TypeAlias(tp)) =>
      tp
    case RefinedType(_, _, TypeBounds(_, _)) =>
      assert(false, em"Unexpected TypeBounds in refinedInfo of $tpe")
      NoType
    case _ =>
      NoType
  }

  def polymorphicMutability(cls: Symbol)(implicit ctx: Context) =
    TypeRef(cls.thisType, MutabilityMemberName)

  def canHaveAnnotations(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp: TypeRef if !tp.symbol.isValueClass => true  // don't do annotations on value classes
    case _: RefinedOrRecType => true
    case _: AndOrType => true
    case _: PolyParam => true
    case _: HKApply => true
    case _: TypeVar => true
    case tp: AnnotatedType => canHaveAnnotations(tp.underlying)
    case _ => false
  }

  def isViewpointAdaptable(tp: Type)(implicit ctx: Context): Boolean = canHaveAnnotations(tp) || (tp match {
    // We automatically adapt (possibly polymorphic) ExprTypes (but not MethodTypes).
    // OR NOT - see <DISCUSSION: EXPR-ADAPTATION> in notes.
    //case tp: ExprType => isViewpointAdaptable(tp.resultType)
    //case tp: PolyType => isViewpointAdaptable(tp.resultType)
    case tp: AnnotatedType => isViewpointAdaptable(tp.underlying)
    case tp: TypeAlias => isViewpointAdaptable(tp.alias)
    case tp: TypeBounds => isViewpointAdaptable(tp.hi)
    case tp: TypeLambda => isViewpointAdaptable(tp.resultType)
    case tp: WildcardType => !tp.optBounds.exists || isViewpointAdaptable(tp.optBounds)
    case _ => false
  })

  def refineResultMember(tp: Type, name: Name, info: TypeBounds, otherMembersToDrop: List[Name] = Nil)(implicit ctx: Context): Type = {
    if (canHaveAnnotations(tp)) {
      RefinedType(dropRefinementsNamed(tp, name :: otherMembersToDrop), name, info)
    } else tp match {
      case tp: ExprType =>
        tp.derivedExprType(refineResultMember(tp.resultType, name, info))
      case tp: MethodType =>
        tp.derivedMethodType(tp.paramNames, tp.paramTypes, refineResultMember(tp.resultType, name, info))
      case tp: PolyType =>
        tp.derivedPolyType(tp.paramNames, tp.paramBounds, refineResultMember(tp.resultType, name, info))
      case tp: AnnotatedType =>
        tp.derivedAnnotatedType(refineResultMember(tp.underlying, name, info), tp.annot)
      case tp: TypeAlias =>
        tp.derivedTypeAlias(refineResultMember(tp.alias, name, info))
      case tp: TypeBounds =>
        tp.derivedTypeBounds(tp.lo, refineResultMember(tp.hi, name, info))
      case tp: TypeLambda =>
        tp.derivedTypeLambda(tp.paramNames, tp.paramBounds, refineResultMember(tp.resultType, name, info))
      case tp: WildcardType =>
        if (tp.optBounds.exists)
          tp.derivedWildcardType(refineResultMember(tp.optBounds, name, info))
        else
          tp.derivedWildcardType(TypeBounds(defn.NothingType, RefinedType(defn.AnyType, name, info)))
      case _ =>
        tp
    }
  }

  def dropRefinementsNamed(tp: Type, names: List[Name])(implicit ctx: Context): Type = tp match {
    case tp: RefinedType if names.contains(tp.refinedName) =>
      dropRefinementsNamed(tp.underlying, names)
    case tp: RefinedType =>
      tp.derivedRefinedType(dropRefinementsNamed(tp.underlying, names), tp.refinedName, tp.refinedInfo)
    case _ =>
      tp
  }

  /**
    * Returns the assignability (prefix mutability) of the given type.
    * See <ASSIGNABILITY-REVISITED> in notes.
    */
  def assignabilityOf(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TermRef =>
      mutabilityOf2(tp.prefix)
    case _ =>
      System.err.println(em"POSSIBLE ERROR: Unexpected type in assignability check: $tp")
      defn.MutableAnnotType
  }

  /*
   Finds the mutability of the given type tp.
   The mutability is itself a type, which has one of the following forms:
     mutable
     readonly
     p.C.this.__MUTABILITY__    // for some class C on path p
     p.A#__MUTABILITY__         // for some abstract type A on path p
     m1 | m2                    // union of mutabilities
     m1 & m2                    // intersection of mutabilities
     <error>                    // ErrorType (only if tp is an ErrorType)

   The boundSpec parameter determines, in certain cases, whether the upper bound or lower bound is returned. boundSpec makes a difference in the following cases:
     tp refines __MUTABILITY__ to bounds >: L <: U where L != U.
     tp is an intersection or union type.

   Cases we address here:
   1. tp is abstract, and its upper bound is not mutable -
   2. tp is this/super - the mutability is assumed to be refined according to a receiver-mutability annotation.
   3. tp is concrete, and defines __MUTABILITY__ -
   4. tp is concrete, and does not define __MUTABILITY -
   5. tp is Any -

  TYPE REFINEMENTS:
   A type that assigns to the __MUTABILITY__ member has that mutability.

  CLASS REFERENCES:
   For a class C defined in p, the path-dependent type p.C is equivalent to p.C @mutable.
   The type scala.Any is equivalent to scala.Any @readonly.

  THIS-TYPES:
   The type p.C.this for an arbitrary class C is treated as having a __MUTABILITY__ member assigned to
    a particular mutability.
    The @polyread annotation denotes the lack of a mutability assignment. Since we don't know what
    concrete mutability will be assigned, we leave the "mutability of p.C.this" in the
    unsimplified form "p.C.this.__MUTABILITY__".

  ABSTRACT TYPE REFERENCES:
   For an abstract-type reference p.A (either an unassigned type member or an uninstantiated type parameter),
    we don't know what its concrete mutability will be.
    the mutability is p.A#__MUTABILITY__.

  CLASSES:
   All classes are assumed to contain the definition "type __MUTABILITY__ >: mutable <: readonly".
  */
  def mutabilityOf2(tp: Type, whichBound: BoundSpec = UpperBound)(implicit ctx: Context): Type = {

    def go(tp: Type): Type = {
      // We have to check for This/Super before widening. Otherwise, since ThisType is a singleton, widening would eliminate it.
      tp.dealias match {
        // tp is C.this for some class C. Basically, we act as though C.this widens to a C that is refined
        //  by the mutability declared for C within the current context.
        // The mutability of super is the same as the mutability of this. See <DISCUSSION: SUPER_THIS_MUT> in the notes.
        case tp: ThisType => findThisMutability(ctx.owner, tp.cls)
        case tp: SuperType => go(tp.thistpe)
        case _ =>
          // Widen to check for other kinds of types.
          val tpWidened = tp.widenDealias.stripAnnots
          tpWidened match {

            // tp refines __MUTABILITY__; return the aliased / simplified-bounded mutability
            case tp: RefinedType =>
              if (tp.refinedName eq MutabilityMemberName) queryMutabilityMember(tp, isClassRef = false)
              else go(tp.parent)

            // tp is (or widens to) a particular class reference.
            // Act as though the class reference was refined to a particular mutability.
            case tp: TypeRef if tp.symbol.isClass =>
              if (tp.symbol eq defn.AnyClass) defn.ReadonlyAnnotType
              else queryMutabilityMember(tp, isClassRef = true)

            // tp widens to an abstract type member -
            case tp: TypeRef if !tp.symbol.isClass => goAbstract(tp /*, abstractRecursions*/)
            case tp: TypeVar => goAbstract(tp /*, abstractRecursions*/)
            case tp: PolyParam => goAbstract(tp /*, abstractRecursions*/)

            // Handle intersections and unions. Since we're returning the upper bound, the mutabilities are intersected or unioned respectively.
            case tp: AndType =>
              if (whichBound eq UpperBound) goIntersection(tp.tp1, tp.tp2)
              else goUnion(tp.tp1, tp.tp2)
            case tp: OrType =>
              if (whichBound eq UpperBound) goUnion(tp.tp1, tp.tp2)
              else goIntersection(tp.tp1, tp.tp2)

            case tp: ErrorType => tp

            case NoPrefix => defn.MutableAnnotType

            case tp: RecType => mutabilityOf2(tp.parent, whichBound)

            case tp: HKApply => mutabilityOf2(tp.superType, whichBound)

            case _ =>
              assert(false, s"Unexpected type in mutabilityOf2: $tpWidened" + (if (tp ne tpWidened) s" (widened from $tp)" else ""))
              defn.MutableAnnotType
          }
      }
    }

    // Queries the type for the __MUTABILITY__ member.
    // If it exists, and it is a type member, returns the specified bound (whichBound).
    // If it doesn't exist, or is not a type member, returns the default mutability (mutable).
    def queryMutabilityMember(tp: Type, isClassRef: Boolean): Type = tp.member(MutabilityMemberName) match {

      // There is an explicit __MUTABILITY__ member.
      //  (or an ErrorDenotation, which has TypeAlias(ErrorType) info; or NoDenotation, which has NoType info.)
      case denot: SingleDenotation =>
        denot.info match {

          // is a type assignment
          case inf: TypeAlias =>
            inf.alias

          // is a type definition
          case inf: TypeBounds =>
            if (inf.hi eq inf.lo)
              inf.hi
            else if (inf.hi eq defn.MutableAnnotType)
              inf.hi
            else if (inf.lo eq defn.ReadonlyAnnotType)
              inf.lo
            else
              // Return the upper bound.
              // * One thing to be careful of when doing type comparisons:
              // mutabilityOf(tp1) <:< mutabilityOf(tp2) may incorrectly return true if tp2's mutability is a RealTypeBounds.
              // An alternative is to return TypeRef(tp, MutabilityMemberName) here, but then the type comparision would
              // have to be careful to avoid infinite recursion.
              // * Regarding isClassRef: if isClassRef is true, then don't return the upper bound.
              // Instead, return the default mutability, which is the lower bound (normally, mutable).
              // The reason for isClassRef is to allow distinction between __MUTABILITY__ type refinements
              // (where we do want to return the upper bound), and class references, where we want to
              // implicitly assume the existence of an appropriate type refinement).
              if ((whichBound eq UpperBound) && !isClassRef) {
                // We've also got a stupidity check here: if the upper bound was defaulted, produce readonly
                if (inf.hi eq defn.AnyType) defn.ReadonlyAnnotType else inf.hi
              } else {
                // We've also got a stupidity check here: if the lower bound was defaulted, produce mutable
                if (inf.lo eq defn.NothingType) defn.MutableAnnotType else inf.lo
              }

          // doesn't exist (NoType) or is something else - since there's no /type member/ named __MUTABILITY__,
          // assume it doesn't exist (there would be a name conflict otherwise). So return the default mutability.
          case _ =>
            defn.MutableAnnotType
        }

      // There is not an explicit __MUTABILITY__ member. Return the default mutability.
      // (Explanation: The only thing we should get here is a MultiDenotation.
      //  We should never get a MultiDenotation from a type member query,
      //  since dotty's intersection-type handling should have resolved the type-member query into a SingleDenotation.
      //  A MultiDenotation here should mean that __MUTABILITY__ is defined as something
      //  other than a type member, so the mutability type member is not defined by the programmer.)
      case _: MultiDenotation =>
        defn.MutableAnnotType
    }

    // Mutability intersection.
    // We do a few basic simplifications here (but we don't use TypeComparer#glb because we might already be inside a type comparison).
    def goIntersection(t1: Type, t2: Type): Type = {
      val mut1 = go(t1)
      val mut2 = go(t2)
      if (mut1 eq mut2)
        mut1
      else if ((mut1 eq defn.MutableAnnotType) || (mut2 eq defn.MutableAnnotType))
        defn.MutableAnnotType
      else if ((mut1 eq defn.ReadonlyAnnotType) && (mut2 eq defn.ReadonlyAnnotType))
        defn.ReadonlyAnnotType
      else
        AndType(mut1, mut2)
    }

    // Mutability union.
    // We do a few basic simplifications here (but we don't use TypeComparer#lub because we might already be inside a type comparison).
    def goUnion(t1: Type, t2: Type): Type = {
      val mut1 = go(t1)
      val mut2 = go(t2)
      if (mut1 eq mut2)
        mut1
      else if ((mut1 eq defn.MutableAnnotType) && (mut2 eq defn.MutableAnnotType))
        defn.MutableAnnotType
      else if ((mut1 eq defn.ReadonlyAnnotType) || (mut2 eq defn.ReadonlyAnnotType))
        defn.ReadonlyAnnotType
      else
        OrType(mut1, mut2)
    }

    go(tp)
  }

  // Is type tp an (alias to an) abstract type?
  def isAbstract(tp: Type)(implicit ctx: Context): Boolean = tp.dealias.stripAnnots match {
    case tp: TypeRef if !tp.symbol.isClass => true
    case tp: PolyParam => true
    case tp: TypeVar if !tp.isInstantiated => true
    case _ => false
  }

  // Abstract type.
  // If the upper bound of the type is definitely mutable, returns mutable; otherwise, returns tpAbstract#__MUTABILITY__.
  def goAbstract(tpAbstract: Type)(implicit ctx: Context): Type = {
    def findBounds(tp: Type): Type = tp match {
      case tp: TypeVar => findBounds(tp.underlying)
      case tp: PolyParam => tp.underlying  // polymorphic parameter -- return underlying bounds
      case tp: TypeRef =>
        val d = tp.denot  // find the referred-to member denotation
        if ((d eq NoDenotation) || d.isOverloaded) NoType else d.info  // return bounds if member exists and is not overloaded
      case _ => NoType   // not an abstract type
    }
    // Return mutable if the upper bound of tpAbstract is definitely mutable.
    findBounds(tpAbstract) match {
      case bounds: TypeBounds
        if mutabilityOf2(bounds.hi, UpperBound) eq defn.MutableAnnotType =>
          defn.MutableAnnotType
      case _ =>
        TypeRef(tpAbstract, MutabilityMemberName)
    }
  }


  //-----OUTER-MUTABILITY TRY 2B------//

  def findHighestMutableLevel(tp: Type)(implicit ctx: Context): Type = {

    // Finds the first (innermost) reference on a prefixes list where the next element is non-mutable or non-existent.
    // Note: The mutability of the first element of the list is ignored. That is, the first element of the list may
    //   be returned even if that element is non-mutable. This is a reasonable behaviour because we are testing/checking
    //   for outer references, not the basic receiver mutability.
    def lastMutable(prefixes: List[Type]): Type = {
      if (prefixes.tail eq Nil)
        defn.RootClass.typeRef  // reached the end of the prefix chain. It's possible that prefixes.head is not <root> if we're calling a nested method, but return <root> anyway (for compatibility with outer reference chains).
      else if (mutabilityOf2(prefixes.tail.head) eq defn.MutableAnnotType)
        lastMutable(prefixes.tail)
      else prefixes.head  // next item in prefixes is not mutable.
    }

    lastMutable(findPrefixReferences(tp))
  }

  def findHighestMutableLevel(atSym: Symbol)(implicit ctx: Context): Type = {

    // Climbs the ownership chain to find the innermost @mutableIn[] annotation.
    def lastMutable(sym: Symbol): Type = {
      sym.annotationsWithoutCompleting(outerMutabilityAnnotNames).foreach { annot =>

        // @mutableIn[p.C];
        if (annot.symbol eq defn.MutableInAnnot) {
          return getLastTypeArgOf(annot.tree.tpe)  // todo do we care about polymorphism in this parameter? (No. - but we do want to update receiver-mutability searching to be compatible with this annotation.)
        }

      }
      val owner = sym.owner //skipValDefs(sym.owner)
      if (!owner.exists) sym.typeRef  // base case: root reference
      else lastMutable(owner)
    }

    def skipValDefs(s: Symbol): Symbol =
      // Skip weak owners, except for packages and modules.
      if (s.isWeakOwner && !s.is(Package) && !s.is(Module)) skipValDefs(s)
      else s

    lastMutable(skipValDefs(atSym))
  }

  def symIsContainedInOwnerOfType(sym: Symbol, tp: Type)(implicit ctx: Context): Boolean = tp match {

    // Should we check to see of tp's class contains the symbol directly, or whether tp's owner class contains the symbol?
    // I think we have to check both.
    // It is possible for either tp's class or enclosing class to be unrelated to the given symbol sym,
    //   so we have to check both cases - if either the class or enclosing class contains sym, we return true.
    case tp: TypeRef =>
      sym.isContainedIn(tp.symbol) ||
      sym.isContainedIn(tp.symbol.lexicallyEnclosingClass)

    // tp is an intersection of (otherwise-unrelated) classes.
    // The reference referred to by tp is known to be mutable.
    // If either branch of the intersection contains sym, then we return true.
    // (Note: I expect that an intersection type here will be a very rare occurrence.)
    case tp: AndType =>
      symIsContainedInOwnerOfType(sym, tp.tp1) || symIsContainedInOwnerOfType(sym, tp.tp2)

    case tp: ErrorType => true

    case _ => false
  }

  def checkPrefixToOuterCompatibility(prefix: Type, atSym: Symbol)(implicit ctx: Context): Boolean = {
    val highestPrefix = unwrapToConcreteClass(findHighestMutableLevel(prefix))
    val highestOuter = unwrapToConcreteClass(findHighestMutableLevel(atSym))
    val isContained = symIsContainedInOwnerOfType(highestOuter.classSymbol, highestPrefix)

    //System.err.println(em"at $atSym: highest mutable outer == $highestOuter")

    //if (!isContained)
    //  assert(false, em"Containment error: Prefix $highestPrefix does not contain $highestOuter")  // todo change to error message instead of assert
    isContained
  }

  //-----OUTER-MUTABILITY TRY 2------//

  // New approach to outer mutability:
  // 1. Find the mutability of the current type.
  // 2. Unwrap the current type to expose its prefix.
  // 3. Repeat from 1, setting the current type to the prefix type.

  // Finds underlying named-class references.
  // May return: p.C, where C is a named class, or an intersection of such references.
  //   If tp is NoType, NoPrefix, or an error type, returns tp unchanged.
  def unwrapToConcreteClass(tp: Type)(implicit ctx: Context): Type = tp.widenDealias.stripAnnots match {
    case tp: TypeRef if tp.symbol.isClass => tp
    case tp: TypeProxy => unwrapToConcreteClass(tp.underlying)
    case AndType(tp1, tp2) => unwrapToConcreteClass(tp1) & unwrapToConcreteClass(tp2)
    case tp: OrType => unwrapToConcreteClass(tp.approximateUnion)
    case NoType => NoType
    case NoPrefix => NoPrefix
    case tp: ErrorType => tp
    case _ =>
      assert(false, s"Unexpected type $tp in unwrapToConcreteClass")
      tp
  }

  // Finds valid prefixes of underlying named-class references.
  def unwrapToPrefix(tp: Type)(implicit ctx: Context): Type = unwrapToConcreteClass(tp) match {
    case tp: TypeRef => tp.prefix
    case AndType(tp1, tp2) => unwrapToPrefix(tp1) & unwrapToPrefix(tp2)
    case _ => tp
  }

  // Finds a list of all prefix references, starting at tp and ending at <root>.
  def findPrefixReferences(tp: Type)(implicit ctx: Context): List[Type] = {
    val prefix = unwrapToPrefix(tp)
    if (prefix.exists && (prefix ne NoPrefix) && !prefix.isError)
      tp :: findPrefixReferences(prefix)
    else
      List(tp)
  }

  case class PairedMutability(tp: Type, mutability: Type)

  def findPrefixMutabilities(tp: Type)(implicit ctx: Context): List[PairedMutability] = {
    val current = PairedMutability(tp, mutabilityOf2(tp))
    val prefix = unwrapToPrefix(tp)
    if (prefix.exists && (prefix ne NoPrefix) && !prefix.isError)
      current :: findPrefixMutabilities(prefix)
    else
      List(current)
  }

  def findNonMutableOuters(atSym: Symbol)(implicit ctx: Context): Set[Type] = {
    var outers = Set[Type]()
    var sym = atSym
    while (sym ne NoSymbol) {
      sym.annotationsWithoutCompleting(pureAnnotationNames).foreach { annot =>
        val annotSymbol = annot.symbol

        // @pureAt[p.C];
        if (annotSymbol eq defn.PureAtAnnot) {
          val tpArg = getLastTypeArgOf(annot.tree.tpe)
          outers ++= findPrefixReferences(tpArg).map(unwrapToConcreteClass)
        }

        // @pure; todo
      }
      sym = sym.owner
    }
    outers
  }

  def findMutableOuters(atSym: Symbol)(implicit ctx: Context): List[Type] = {
    if (atSym.effectiveOwner is Method) Nil   // atSym is a nested method - ignore outer references
    else {
      val allOuters = findPrefixReferences(atSym.enclosingClass.thisType).tail // do a .tail to remove immediate receiver reference
      val nonMutableOuters = findNonMutableOuters(atSym)
      allOuters.filter(!nonMutableOuters.contains(_)).map(unwrapToConcreteClass)
    }
  }

  def prefixListToString(tpList: List[Type])(implicit ctx: Context): String = {
    val strs = tpList.map { tp => em"$tp" }
    strs.mkString(" & ")
  }

  def isCallableByPrefix(prefix: Type, atSym: Symbol)(implicit ctx: Context): Boolean = {
    val allPrefixes = findPrefixReferences(prefix).tail  // do a .tail to remove immediate receiver reference
    val mutablePrefixes = allPrefixes.map(unwrapToConcreteClass).filter(mutabilityOf2(_) eq defn.MutableAnnotType)
    val mutableOuters = findMutableOuters(atSym)
    val callable = makeAndType(mutablePrefixes) <:< makeAndType(mutableOuters)
    if (!callable)
      System.err.println(em"Call to $atSym has incompatible prefix.\n   Got: ${prefixListToString(mutablePrefixes)}\n   Expected: ${prefixListToString(mutableOuters)}\n")
    callable
  }

  def checkPrefixes(tree: tpd.Tree, prefixes: List[PairedMutability], nonMutableOuters: Set[Type])(implicit ctx: Context): tpd.Tree = {
    // Look at prefix tail (to skip immediate receiver reference)
    prefixes.tail.foreach { prefix =>
      if (prefix.mutability ne defn.MutableAnnotType) {
        val found = nonMutableOuters.exists { tpOuter =>
          prefix.tp <:< tpOuter
        }
        if (!found) return errorTree(tree, em"Outer-reference mutability error:\n   Receiver has a non-mutable prefix ${prefix.tp}, but a mutable prefix was expected")
      }
    }
    tree
  }


  //-----OUTER-MUTABILITY TRY 1------//

  def makeAndType(tpList: List[Type])(implicit ctx: Context): Type = {
    if (tpList eq Nil) defn.AnyType
    else if (tpList.tail eq Nil) tpList.head
    else AndType(tpList.head, makeAndType(tpList.tail))
  }

  // STEP 1: Find outer references
  def findOuterReferences(atSym: Symbol)(implicit ctx: Context): List[Type] = {
    val owner = atSym.enclosingClass
    val tp =
      if (owner is Module) owner.termRef
      else owner.thisType
    findPrefixReferences_dep(tp)
  }
  def findOuterReferences_dep(implicit ctx: Context): List[Type] = findOuterReferences_dep(ctx.owner)
  def findOuterReferences_dep(atSym: Symbol, skipEnclosingClass: Boolean = false)(implicit ctx: Context): List[Type] = {
    if (atSym eq NoSymbol) Nil
    else if (atSym.isClass) {
      if (skipEnclosingClass) findOuterReferences_dep(atSym.owner, skipEnclosingClass = false)
      else atSym.thisType :: findOuterReferences_dep(atSym.owner, skipEnclosingClass)
    }
    else findOuterReferences_dep(atSym.owner, skipEnclosingClass)
  }
  def findOuterReferenceMutabilities(implicit ctx: Context): List[Type] = findOuterReferenceMutabilities(ctx.owner)
  def findOuterReferenceMutabilities(atSym: Symbol, skipEnclosingClass: Boolean = false)(implicit ctx: Context): List[Type] = {
    if (atSym eq NoSymbol) Nil
    else if (atSym.isClass) {
      if (skipEnclosingClass) findOuterReferenceMutabilities(atSym.owner, skipEnclosingClass = false)
      else defn.MutableAnnotType :: findOuterReferenceMutabilities(atSym.owner, skipEnclosingClass)  // TODO: replace defn.MutableAnnotType with actual mutability
    }
    else findOuterReferenceMutabilities(atSym.owner)
  }

  // STEP 2: Discover prefix-type references
  def findPrefixReferences_dep(tp: Type)(implicit ctx: Context): List[Type] = tp match {

    // got a term p.x; add p.x to the prefix list, then look at p
    case tp: TermRef if tp.prefix ne NoPrefix => tp :: findPrefixReferences_dep(tp.prefix)

    // got a variable x; widen it to find higher prefixes
    case tp: TermRef => findPrefixReferences_dep(tp.widen)

    // got a type p.C.this; add it, then look at outer references of p.C
    case tp: ThisType => tp :: findPrefixReferences_dep(tp.tref)
      // /*tp :: */ findOuterReferences(tp.cls) //findPrefixReferences(tp.tref)

    // got a type q.D.super[p.C.this]; look at p.C.this
    case tp: SuperType => findPrefixReferences_dep(tp.underlying)

    // got a type p.C; look at p
    case tp: TypeRef => findPrefixReferences_dep(tp.prefix)

    // got a type proxy; look at underlying type
    case tp: TypeProxy => findPrefixReferences_dep(tp.underlying)

    // got a ground type; we've either reached the root class, gotten an error, or gotten something unexpected
    case _ => Nil
  }
  // todo: findPrefixReferenceMutabilities



  /**
    * Returns the mutability of a symbol's declared receiver-mutability type.
    * If no RI annotations are present on the symbol, defaults to @mutable.
    */
  def declaredReceiverType(sym: Symbol)(implicit ctx: Context): Type = {
    if (!(sym is Method) && !sym.isClass)  // ignore annotations on field symbols
      return defn.MutableAnnotType   // todo: not sure if filtering for methods is the right thing to do here (what I want to make sure of is that RI annotations on fields don't get processed in the same way as method annotations)

    // Return the first receiver-mutability annotation declared on the symbol.
    sym.annotationsWithoutCompleting(receiverAnnotationNames).foreach { annot =>
      val mut = annotationToMutabilityType(annot, sym.lexicallyEnclosingClass)
      if (mut ne NoType)
        return mut
    }

    // If there's a @pure on the symbol, and the symbol is a member of a class, default to a polymorphic mutability.
    if (sym.effectiveOwner.isClass && sym.isPure)
      return TypeRefUnlessNoPrefix(sym.effectiveOwner.thisType, MutabilityMemberName)

    // No RI annotations found.
    defn.MutableAnnotType
  }

  def annotationToPurityType(annot: Annotation, classSym: Symbol)(implicit ctx: Context): Type = {
    assert(classSym.isClass, em"Expected a class, got $classSym")
    val symbol = annot.symbol

    if (symbol eq defn.PureAtAnnot) getLastTypeArgOf(annot.tree.tpe)
    else NoType
  }

  def annotationToMutabilityType(annot: Annotation, classSym: Symbol)(implicit ctx: Context): Type = {
    assert(classSym.isClass, em"Expected a class, got $classSym")
    val symbol = annot.symbol

    // @readonly and @mutable
    if (symbol eq defn.ReadonlyAnnot) defn.ReadonlyAnnotType
    else if (symbol eq defn.MutableAnnot) defn.MutableAnnotType

    // @polyread - same as @mutabilityOfRef(this)
    else if (annot.symbol eq defn.PolyreadAnnot) {
      if (!classSym.isClass)
        errorType(em"Only class/trait members may be annotated @polyread (owner $classSym is not a class/trait)", symbol.pos)
      else
        TypeRefUnlessNoPrefix(classSym.thisType, MutabilityMemberName)
    }

    // @mutabilityOfRef(r) - generates type r.__MUTABILITY__
    else if (annot.symbol eq defn.MutabilityOfRefAnnot)
      TypeRefUnlessNoPrefix(annot.arguments.head.tpe.widenIfUnstable, MutabilityMemberName)

    // @mutabilityOf[T] - generates type T#__MUTABILITY__
    else if (annot.symbol eq defn.MutabilityOfAnnot)
      TypeRefUnlessNoPrefix(getLastTypeArgOf(annot.tree.tpe), MutabilityMemberName)

    // @mutability[M] - returns M
    else if (annot.symbol eq defn.MutabilityDirectAnnot)
      getLastTypeArgOf(annot.tree.tpe)

    else
      NoType
  }

  def TypeRefUnlessNoPrefix(prefix: Type, name: TypeName)(implicit ctx: Context) =
    if ((prefix eq NoPrefix) || (prefix eq NoType) || prefix.isError)
      NoType
    else
      TypeRef(prefix, name)

  def assumePure(sym: Symbol)(implicit ctx: Context): Boolean = {
    val owner = if (sym.isClass) sym else sym.effectiveOwner

    (
      // Assume everything in Any/AnyVal/Object is @pure (AnyRef is an alias of Object)
      (owner eq defn.AnyClass) ||
      (owner eq defn.AnyValClass) ||
      (owner eq defn.ObjectClass) ||

      // Assume everything in Function/Tuple/Product is @pure
      defn.isFunctionClass(owner) ||
      defn.isAbstractFunctionClass(owner) ||
      defn.isTupleClass(owner) ||
      defn.isProductClass(owner) ||

      // Assume all methods in Byte/Int/Double/Boolean/etc. are @pure
      owner.isPrimitiveValueClass ||

      // Assume annotation constructors are @pure. Amendment: Assume only certain annotation constructors are pure.
      (owner eq defn.PureAnnot) ||
      (owner eq defn.AnnotationClass) ||
      (owner eq defn.StaticAnnotationClass) ||
      (owner eq defn.ClassfileAnnotationClass) ||
      //(owner derivesFrom defn.AnnotationClass) ||

      // Arrays: only certain methods are @pure
      ((owner eq defn.ArrayClass) && defaultPureArrayMethods.contains(sym.name)) ||

      // Assume everything in the Predef module is @pure
      (owner eq defn.DottyPredefModule.moduleClass) ||
      (owner eq defn.ScalaPredefModule.moduleClass) ||

      // Assume certain key standard-lib methods are @pure
      (sym eq defn.Sys_error)
    )
  }

  /**
    * Replaces a recursive this-mutability with its equivalent in the current context.
    * For example, when calling a polyread method d from a non-polyread method e:
    * {{{
    *   class C {
    *     @polyread def d: Any @mutabiliyOf(this) = ???   // here, @mutabilityOfRef(this) means { __MUTABILITY__ = C.this.__MUTABILITY__ }
    *     def e: Any = d   // here, @mutabilityOfRef(this) should mean { __MUTABILITY__ = mutable }
    *   }
    * }}}
    */
  def substThisMutability(tp: Type)(implicit ctx: Context): Type = tp.finalResultType.member(MutabilityMemberName) match {
    case NoDenotation =>
      tp  // mutability member doesn't exist. No substitution necessary.
    case d =>
      // Find the mutability of the denotation. (We take the upper bound here. See <DISCUSSION: MUT_BOUNDS> in notes.)
      val m = d.info.bounds.hi
      // Replace a recursive this-mutability with its equivalent in the current context.
      // If we did not do this, then the mutability may not match the current this-mutability.
      m match {
        case TypeRef(thisTpe, MutabilityMemberName) if thisTpe.isInstanceOf[ThisType] =>
          // Also, we don't just replace the ThisType of the nearest enclosing class. We replace any ThisType.
          // We want to replace the this-mutability only if it is different than what the this-mutability should
          // be inside the current context. But no special case is actually needed here -- if the mutability
          // really shouldn't be changed, then mutabilityOf(thisTpe) should be equivalent to thisTpe,
          // producing a refinement that's identical to the original tp.
          // UPDATE: I think we really do need to substitute only when we're in a method of class thisTpe.
          // Otherwise, this-mutabilities referring to an outer class get unfairly reduced; we want to
          // keep outer-class mutabilities in their original forms for as long as possible.
          if (skipWeakNonClassOwners(ctx.owner.owner).derivesFrom(thisTpe.asInstanceOf[ThisType].cls)) {
            val thisMut = mutabilityOf2(thisTpe)
            refineResultMember(tp, MutabilityMemberName, TypeAlias(thisMut, 1), otherMembersToDrop = List(PrefixMutabilityName))
            // We don't bother keeping the __PREFIX_MUTABILITY__ (or assignability) member.
            // It only matters during assignment, and it should be put back automatically if this method is followed by a viewpoint adaptation.
          } else
            tp
        case _ =>
          tp  // mutability member is not a ThisType. No substitution necessary. (Unless we allow more complicated mutability types that contain this-types!)
      }
  }

  /** Used to patch an issue where the mutability we expected actually refers to a superclass of the mutability we got.
    * See {{{<DISCUSSION:RECEIVER-MUTABILITY-SAME-OBJECT>}}} in the notes. */
  def substThisMutability2(here: Type, there: Type)(implicit ctx: Context): Type = there match {
    case TypeRef(thereThis, MutabilityMemberName)
      if thereThis.isInstanceOf[ThisType] || thereThis.isInstanceOf[SuperType] => here
    case _ => there
  }

  def viewpointAdapt(prefix: Type, target: Type, origSym: Symbol)(implicit ctx: Context): Type = {
    val preMut = mutabilityOf2(prefix) //mutabilityOf(prefix, origSym)
    val target1 = substThisMutability(target)   // translate this-mutabilities on the target
    val tpMut = mutabilityOf2(target1) //mutabilityOf(target1, ctx.owner)
    val finalMut = preMut | tpMut
    //if (prefix ne NoPrefix) {
    //  System.err.println(em"pre: $prefix mut $preMut")
    //  System.err.println(em"tp:  $target1 mut $tpMut" + (if (target ne target1) em" (tp subst from $target)" else ""))
    //}

    // Refine the mutability if the final mutability is different than target mutability.
    val tp1 = if (finalMut ne tpMut)
      refineResultMember(target1, MutabilityMemberName, TypeAlias(finalMut, 1), otherMembersToDrop = List(PrefixMutabilityName))
    else
      target1

    //// Set the assignability member if the prefix is non-mutable.
    //if (preMut ne defn.MutableAnnotType)
    //  refineResultMember(tp1, PrefixMutabilityName, TypeAlias(preMut, 1))
    //else
      tp1
  }

  /** Returns a list of @asFinal annotation arguments on fromSym and all enclosing symbols. */
  def allFinals(fromSym: Symbol)(implicit ctx: Context): List[tpd.Tree] = {
    if (fromSym is Package) Nil
    else
      asFinalList(fromSym) ::: allFinals(fromSym.owner)
  }

  /** Returns a list of arguments found in @asFinal annotations on the given symbol. */
  def asFinalList(onSym: Symbol)(implicit ctx: Context): List[tpd.Tree] = {
    var tpes = List[tpd.Tree]()
    onSym.annotationsWithoutCompleting(finalAnnotationNames).foreach { annot =>
      if (annot.symbol eq defn.AsFinalAnnot)
        tpes ::= annot.arguments.head
    }
    tpes
  }

  /** Searches for an @asFinal annotation referring to sym. If no eligible @asFinal annotation is found, returns false.
    * Searches the symbol ownership chain starting at fromSym. */
  def isViewedAsFinal(sym: Symbol, fromSym: Symbol)(implicit ctx: Context): Boolean = {
    assert(sym.isTerm && !(sym is Method), em"Expected a value term in isViewedAsFinal")

    // Stop searching if we reach the package level or fromSym contains sym.
    // The containment check is important -- we don't want a @pure annotation to apply to symbols inside the annotated method.
    if ((fromSym is Package) || sym.isContainedIn(fromSym)) return false

    // Look through annotations on fromSym.
    // If there is an @asFinal(ref) present, and the symbol of ref is the given symbol, return true.
    fromSym.annotationsWithoutCompleting(finalAnnotationNames).foreach { annot =>
      if ((annot.symbol eq defn.AsFinalAnnot) && (annot.arguments.head.symbol eq sym))
        return true
    }

    // If the symbol is:
    // either static or a bona fide variable (not a field),
    // and there is a @pure annotation present,
    // then the symbol should be viewed as final.
    if (sym.isStatic || (sym.effectiveOwner is Method))
      fromSym.annotationsWithoutCompleting(pureAnnotationNames).foreach { annot =>
        if (annot.symbol eq defn.PureAnnot)
          return true
      }

    // Look up the ownership chain for more annotations.
    isViewedAsFinal(sym, fromSym.owner)
  }

  /** Returns a list of @asType annotation arguments on fromSym and all enclosing symbols. */
  def allTypeViews(fromSym: Symbol)(implicit ctx: Context): List[(tpd.Tree, Type)] = {
    if (fromSym is Package) Nil
    else
      viewedTypeList(fromSym) ::: allTypeViews(fromSym.owner)
  }

  /** Returns a list of arguments found in @asType annotations on the given symbol.
    * Use nonViewedNamedType to find original symbol type. */
  def viewedTypeList(onSym: Symbol)(implicit ctx: Context): List[(tpd.Tree, Type)] = {
    var tpes = List[(tpd.Tree, Type)]()
    onSym.annotationsWithoutCompleting(typeviewAnnotationNames).foreach { annot =>
      if (annot.symbol eq defn.AsTypeAnnot)
        tpes ::= (annot.arguments.head, getLastTypeArgOf(annot.tree.tpe))
    }
    tpes
  }

  /** Searches for an @asType annotation referring to sym. If no eligible @asType annotation is found, returns NoType.
    * Searches the symbol ownership chain starting at fromSym.
    * NOTE: We pass the original type of the symbol's denotation as a parameter.
    *  The reason is: it seems that denot.info and denot.sym.info do not always yield the same type...
    *  For example, in the sjs_ScopedVar test, there is a private field "value" whose denot.info
    *  is a TypeRef, but whose denot.sym.info is an ExprType, which is a non-value type (that I do not expect here). */
  def findViewedType(sym: Symbol, fromSym: Symbol, origType: Type)(implicit ctx: Context): Type = {
    // It is possible for sym to be a class (and origType to be a ClassInfo) if "this" is used as an argument to @asType.
    // A ClassInfo is not a ValueType, but we don't want to trigger an assertion (below).
    if (sym.isClass) return NoType

    //assert(sym.isTerm && !sym.info.isInstanceOf[MethodicType], em"Expected a value term in findViewedType")
    assert(origType.isInstanceOf[ValueType], em"Expected a value term in findViewedType")

    // Stop searching if we reach the package level or fromSym contains sym.
    // The containment check is important -- we don't want a @pure annotation to apply to symbols inside the annotated method.
    if ((fromSym is Package) || sym.isContainedIn(fromSym)) return NoType

    // Look through annotations on fromSym.
    // If there is an @asType[T](ref) present, and the symbol of ref is the given symbol, return T.
    fromSym.annotationsWithoutCompleting(typeviewAnnotationNames).foreach { annot =>
      if ((annot.symbol eq defn.AsTypeAnnot) && (annot.arguments.head.forcedTyped.symbol eq sym))
        return getLastTypeArgOf(annot.tree.tpe)
    }

    // If the symbol is:
    // either static or a bona fide variable (not a field),
    // and there is a @pure annotation present,
    // and there is no @asType for the variable,
    // then the viewed type is a @readonly version of the variable's type (as viewed from the owner of fromSym).
    if (sym.isStatic || (sym.effectiveOwner is Method))
      fromSym.annotationsWithoutCompleting(pureAnnotationNames).foreach { annot =>
        if (annot.symbol eq defn.PureAnnot) {
          val viewed = findViewedType(sym, fromSym.owner, origType) match {
            case NoType => origType
            case t => t
          }
          return refineResultMember(viewed, MutabilityMemberName, ReadonlyType, otherMembersToDrop = List(PrefixMutabilityName))
        }
      }

    // Look up the ownership chain for more annotations.
    findViewedType(sym, fromSym.owner, origType)
  }

  /** Searches for an @asType annotation referring to sym in the current context.
    * If no eligible @asType annotation is found, returns origType. */
  def viewedTypeOf(sym: Symbol, origType: Type)(implicit ctx: Context): Type = {
    val viewedTp = findViewedType(sym, ctx.owner, origType)
    if (viewedTp eq NoType)
      origType
    else
      viewedTp
  }

  /** Finds the innermost enclosing @pure-annotated symbol, if any. */
  def pureOwner(sym: Symbol)(implicit ctx: Context): Symbol = {
    if (sym is Package) return NoSymbol

    sym.annotationsWithoutCompleting(pureAnnotationNames).foreach { annot =>
      if (annot.symbol eq defn.PureAnnot)
        return sym
    }

    pureOwner(sym.owner)
  }

  /** If tp is a named type, returns the non-viewpoint-adapted type of the symbol tp refers to. */
  def nonViewedNamedType(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: NamedType =>
      tp.denot.info  // see <DISCUSSION:GETTING-ORIGINAL-SYMBOL-TYPE> in notes.
    case _ =>
      assert(false, em"Expected NamedType, got $tp")
      tp
  }

  def isAssignable(tp: Type)(implicit ctx: Context): Boolean = {
    val a = assignabilityOf(tp)
    a <:< defn.MutableAnnotType

    //val prefixMutDenot = tp.member(PrefixMutabilityName)
    //!prefixMutDenot.exists || (prefixMutDenot.info <:< MutableType)
  }

  class DotModTypeOpHooks(initCtx: Context) extends TypeOpHooks(initCtx) {

    /** The info of the given denotation, as viewed from the given prefix. */
    override def denotInfoAsSeenFrom(pre: Type, denot: Denotation): Type = {

      var target = denot.info

      // Don't do custom logic in a types-erased phase
      if (!ctx.erasedTypes) {

        //if (denot.symbol.name eq termName("w"))
        //  false

        // Do viewpoint adaption for terms only.
        // Note: Changed to canHaveAnnotations from isViewpointAdaptable.
        //   Current theory is that only non-methodic types should be adapted - see <DISCUSSION: EXPR-ADAPTATION> in notes.
        if (denot.isTerm && !(denot.symbol is Module) && canHaveAnnotations(target)) {
          target = viewedTypeOf(denot.symbol, target)         // handle @asType[T](ref) annotations
          target = viewpointAdapt(pre, target, denot.symbol)  // viewpoint-adapt a prefixed term reference
        }

        // Do a substitution of this-type mutabilities on method results.
        else if ((denot.symbol is Method) && canHaveAnnotations(target.finalResultType))
          target = substThisMutability(target)

      }

      target
    }

    /** A new object of the same type as this one, using the given context. */
    override def copyIn(ctx: Context) = new DotModTypeOpHooks(ctx)
  }


  /**********
    * TYPER *
    **********
    */
  class DotModTyper extends Typer {

    def checkedReceiver(prefix: Type, tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
      if (tree.symbol.isConstructor)  // it's always OK to call a constructor (regardless of receiver mutability) because the receiver reference is unique
        return tree
      // adding a substThis to the declared mutability
      val declaredMut = declaredReceiverType(tree.symbol).substThis(tree.symbol.lexicallyEnclosingClass.asClass, prefix)
      // When asking for mutabilityOf(prefix) here, we search from the current context owner (rather than tree.symbol).
      // The reason is that when checkedReceiver is called, this-types are valid with respect to the current
      //  context, but the called method may be in a different tree. E.g., see core/Symbols,
      //  where a search for method Symbols.this.ctx is performed from within Symbols, although ctx is
      //  defined in Contexts (Symbols and Contexts are mixed together).
      val preMut = mutabilityOf2(prefix)
      // Make sure to do a this-substitution on the declared receiver type.
      // Before substitution, @polyread methods have a declared mutability like C.this.__MUTABILITY__,
      // which would not otherwise compare equal to the receiver mutability.
      // NOTE: I suppose another way to handle this problem is to have the type comparer
      // pry open mutabilities of the form C.this.__MUTABILITY__ to see what those mutabilities actually
      // evaluate to in the comparison context. There would be a cost in type comparer complexity,
      // and I would have to make sure to open up these mutabilities by only one extra level to avoid infinite recursion.
      // Issue: see <DISCUSSION:RECEIVER-MUTABILITY-SAME-OBJECT> in notes.
      // Trying to fix it with substThisMutability instead of substThis.
      val declared2 = substThisMutability2(preMut, declaredMut)
      if (!(preMut <:< declared2)) {
        errorTree(tree, em"Incompatible receiver mutability in call to method ${tree.symbol.name}:\n" +
          em"  Expected: $declared2\n" +
          em"  Got: $preMut")
      } else
        tree
    }

    def checkedPurity(prefix: Type, tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
      tree.denot.alternatives.foreach { denot =>
        val selectedSym = denot.symbol

        // Check that all @asFinal annotations in effect in the current context are also in effect on the called method.
        allFinals(ctx.owner).foreach { arg =>
          if (!isViewedAsFinal(arg.symbol, selectedSym))
            return errorTree(tree, em"Cannot select $selectedSym from here: $arg must be declared @asFinal")
        }

        // Check that all type views in the current context are compatible with all type views at the called method.
        allTypeViews(ctx.owner).foreach { case (arg, tpe) =>
          val argDenot = arg.forcedTyped.denot
          val tpe2 = findViewedType(argDenot.symbol, selectedSym, argDenot.info) match {
            case NoType => argDenot.info
            case tp => tp
          }
          if (!(tpe <:< tpe2))
            return errorTree(tree, em"Cannot select $selectedSym from here: $arg is $tpe here, which is not compatible with expected type $tpe2.")
        }

        // Check that the called method conforms to the purity of the caller (i.e., the purity of the current context owner).
        // Conformance is present if either of the two following conditions holds:
        // 1. The called method is @pure.
        // 2. The called method is inside the purity boundary of the caller.
        if (!(selectedSym.isPure || selectedSym.isContainedInPurityBoundaryOf(ctx.owner))) {
          if (assumePure(ctx.owner.purityBoundary))
            ctx.warning(em"$selectedSym should be annotated @pure due to the assumed purity of ${ctx.owner.purityBoundary}", tree.pos)
          else {
            //selectedSym.isContainedInPurityBoundaryOf(ctx.owner)
            return errorTree(tree, em"Cannot select $selectedSym from here: It must be @pure due to the @pure on ${ctx.owner.purityBoundary}.")
          }
        }
      }

      tree
    }

    /**
      * preChecks: Checks the following:
      *  - Receiver compatibility on method selections.
      */
    def preChecks(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: tpd.RefTree if (tree.symbol is Method) && !assumePure(tree.symbol) =>
        tree.tpe match {
          case TermRef(prefix, underlying) =>

            // Prefix validation
            //val prefixes = findPrefixMutabilities(prefix)
            //val outers = findNonMutableOuters(tree.symbol)
            //val prefixes = findPrefixReferences(prefix)
            //val andPrefix = makeAndType(prefixes)
            //System.err.println(em"Prefix type: $prefix")
            //System.err.println(em"Selecting ${tree.symbol}; prefixes are: $andPrefix")

            //val y = findPrefixMutabilities(tree.symbol.owner.thisType)
            //val outers = findOuterReferences(tree.symbol)
                //findOuterReferences_dep(tree.symbol, tree.symbol.isConstructor)
            //val andOuters = makeAndType(outers)
            //System.err.println(em" outers at ${tree.symbol} are: $andOuters")

            //System.err.println(em"$andPrefix (in call to ${tree.symbol})")

            //System.err.println("")

            //assert (x.length == y.length, em"Mismatch on prefix $prefix calling ${tree.symbol}.")

            //val tre1 = checkPrefixes(tree, prefixes, outers)
            //if (tre1.tpe.isError) return tre1
            //isCallableByPrefix(prefix, tree.symbol)

            //checkPrefixToOuterCompatibility(prefix, tree.symbol)

            // TODO: the checkPrefixToOuterCompatibility seems to be very slow (see, e.g., the pos_rbtree test). Try an approach that uses less compute time...

            val tree1 = checkedReceiver(prefix, tree)
            checkedPurity(prefix, tree1)
          case _ =>
            System.err.println(em"WEIRD WARNING: Selection of ${tree.symbol} does not have a TermRef type. Type is: ${tree.tpe}")
            tree
        }

      case _ => tree
    }

    /**
      * customChecks: Checks the following:
      *  - Assignability.
      *  - Legality of @asType and @asFinal annotation arguments.
      */
    def customChecks(tree: tpd.Tree, pt: Type)(implicit ctx: Context): tpd.Tree = tree match {

      case tree: tpd.MemberDef =>
        // A symbol's type as viewed from outside a context must be compatible with its type as viewed within the context.
        // We do this to prevent illegal downcasts.
        // Basically, we can use @asType to force a more restrictive (greater/upcast) static type on a symbol,
        // but we cannot use @asType to force arbitrary downcasts.
        viewedTypeList(tree.symbol).foreach { case (arg, typeInside) =>
          val typedArg = arg.forcedTyped  // HACK!!! I don't know why I don't always have a typed tree here. See <DISCUSSION: ANNOTATION TYPING ISSUE related to @asType/@asFinal>.
          typedArg.tpe match {
            case _: NamedType =>
              val typeOutside = viewedTypeOf(typedArg.symbol, typedArg.denot.info)(ctx)
              if (!(typeOutside <:< typeInside))
                return errorTree(arg, em"Illegal type view for ${typedArg.symbol}:\n" +
                  em" required at least: $typeOutside\n" +
                  em" got: $typeInside")
            case _ =>
              return errorTree(arg, em"Illegal @asType annotation: Expected a field or variable, got $arg")
          }
        }
        // Also check validity of @asFinal arguments.
        asFinalList(tree.symbol).foreach { arg =>
          arg.tpe match {
            case _: NamedType =>  // nothing to do -- argument is OK
            case _ =>
              return errorTree(arg, em"Illegal @asFinal annotation: Expected a field or variable, got $arg")
          }
        }
        tree

      case tree: tpd.Assign =>
        if (!isAssignable(tree.lhs.tpe))
          errorTree(tree.lhs, em"Cannot perform assignment; ${tree.lhs.symbol} is effectively final due to a non-mutable prefix type.")
        else if (isViewedAsFinal(tree.lhs.symbol, ctx.owner))
          errorTree(tree.lhs, em"Cannot perform assignment; ${tree.lhs.symbol} is effectively final due to a @pure or @asFinal annotation.")
        // Make sure the RHS type is compatible with the original (non-viewed) LHS type.
        // Essentially, even though the RHS is compatible with the viewed LHS, it is not necessarily legal to assign to the variable.
        // In basic RI, we have a special case of this problem--namely, that a field cannot be assignable if
        // if its viewed mutability type is greater than its original mutability type--but the only way a viewed mutability
        // can be greater than the original mutability is if the prefix is @mutable. So for mere RI, making
        // a @mutable prefix a necessary condition for assignment is sufficient to prevent problems.
        // Here, however, we generalize--for maximal utility, we declare a variable unassignable only where
        // there is a genuine incompatibility with the variable's original declared type.
        else if (!(tree.rhs.tpe <:< nonViewedNamedType(tree.lhs.tpe)))
          err.typeMismatch(tree.rhs, nonViewedNamedType(tree.lhs.tpe))
        else
          tree

      case _ =>
        tree
    }

    def convertAnnotationToRefinement(tp: AnnotatedType)(implicit ctx: Context): Type = {
      // NOTE: We no longer do mutability-type reduction/simplification here.
      // Eager simplification can reduce this-mutabilities to @mutable in places where they should be polymorphic.
      val mut = annotationToMutabilityType(tp.annot, ctx.owner.lexicallyEnclosingClass)
      if (mut ne NoType)
        refineResultMember(tp.underlying, MutabilityMemberName, TypeAlias(mut, 1))  // strip annotation, replace with refinement
      else
        tp   // not a valid RI annotation - return type unchanged
    }

    override def completeAnnotations(mdef: untpd.MemberDef, sym: Symbol)(implicit ctx: Context): Unit = {
      // Complete annotations in the outer context (rather than current).
      // See <DISCUSSION:LAZY-NON-COMPLETER> and <DISCUSSION:SYM-ANNOT-COMPLETER> in notes.
      // Update: We're completing _some_ annotations in the outer context, and also forcing typing of annotation arguments.
      // We re-type the needed annotations here.
      // See <DISCUSSION: ANNOTATION TYPING ISSUE related to @asType/@asFinal> in notes.
      super.completeAnnotations(mdef, sym)(ctx.outer)
    }

    override def adaptInterpolated(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      val tree01 = preChecks(tree0)
      if (tree01.tpe.isError)
        return tree01

      // Find out what type the default typer thinks this tree has.
      val tree = super.adaptInterpolated(tree01, pt, original)
      if (tree.tpe.isError)
        return tree

      // Add refinements to certain types.
      val tpe1 = tree.tpe match {

        // if there's an annotation, convert it to a mutability
        case tp: AnnotatedType =>
          val tp1 = convertAnnotationToRefinement(tp)
          if ((tp1 ne tp) && !canHaveAnnotations(tree.tpe))
            errorType("Reference immutability annotations are not allowed here", tree.pos)
          else
            tp1

        // if we've got type bounds with upper bound Any, make the upper bound readonly
        //case tp: RealTypeBounds if tp.hi eq defn.AnyType =>
        //  tp.derivedTypeBounds(tp.lo, refineResultMember(tp.hi, MutabilityMemberName, ReadonlyType))

        case tp =>
          tp
      }

      customChecks(tree.withType(tpe1), pt)
    }

    override def newLikeThis: Typer = new DotModTyper
  }


  /**************
    * REFCHECKS *
    **************
    * This phase runs the regular Scala RefChecks with the DotMod type comparer to enforce necessary
    * subtyping relationships among symbols. This phase should either be run as its own phase
    * so that the type comparer can be changed, or otherwise the root context should have the
    * correct type comparer set.
    */
  class DotModRefChecks extends RefChecks {
    override def phaseName: String = "dotmodrefchecks"
    override val treeTransform = new MyTransform

    override def run(implicit ctx: Context): Unit = {
      if (enableRefChecks)
        super.run(ctx.fresh.setTypeComparerFn(new DotModTypeComparer(_)))
      else
        super.run
    }

    class MyTransform extends Transform {

      /*
        Really, the only thing we've got to check here is that overridden receiver mutabilities match overriding mutabilities.
        Incompatible mutabilities of parameters and result types are caught automatically due to the custom type comparer.
       */

      def customOverrideCheck(overriding: Symbol, overridden: Symbol)(implicit ctx: Context): Unit = {
        val riddenMut = declaredReceiverType(overridden)
            .substThis(overridden.effectiveOwner.asClass, overriding.effectiveOwner.thisType)
        var ridingMut = declaredReceiverType(overriding)

        // Where a field is overriding a method, readjust its receiver to @mutabilityOfRef(C.this) where C is the overriding symbol's class.
        // (See <DISCUSSION: OVERRIDE_VAL_DEF> in notes.)
        if ((overridden is Method) && !(overriding is Method))
          ridingMut = polymorphicMutability(overriding.effectiveOwner)

        if (!(riddenMut <:< ridingMut)) {
          // If the receiver incompatibility is likely due to an assumed @pure annotation, issue a warning later instead of an error here
          if (overriding.isPure || !assumePure(overridden))
            ctx.error(em"Cannot override $overridden due to receiver mutability.\n" +
              em"   Overridden mutability: $riddenMut\n" +
              em"   Overriding mutability: $ridingMut", overriding.pos)
        }
      }

      def checkFinals(overriding: Symbol, overridden: Symbol)(implicit ctx: Context): Unit = {
        if (overriding is Method) {   // synthetic getter methods are assumed-pure, so nothing must be done if overriding is a field
          // If the overridden method declares a symbol to be effectively final, then it must also be effectively final in the overriding method.
          asFinalList(overridden).foreach { arg =>
            if (!isViewedAsFinal(arg.symbol, overriding))
              ctx.error(em"Cannot override $overridden: $arg must be declared @asFinal", overriding.pos)
          }
        }
      }

      def checkTypeViews(overriding: Symbol, overridden: Symbol)(implicit ctx: Context): Unit = {
        if (overriding is Method) {   // synthetic getter methods are assumed-pure, so nothing must be done if overriding is a field
          // If the overridden method declares a type view, then the viewed type must be compatible with the viewed type in the overriding method.
          viewedTypeList(overridden).foreach { case (arg, tpe) =>
            val argDenot = arg.forcedTyped.denot
            val tpe2 = findViewedType(argDenot.symbol, overriding, argDenot.info) match {
              case NoType => argDenot.info
              case tp => tp
            }
            if (!(tpe <:< tpe2))
              ctx.error(em"Cannot override $overridden: $arg is $tpe2 here, but must be at least $tpe", overriding.pos)
          }
        }
      }

      def checkPurityEffects(overriding: Symbol, overridden: Symbol)(implicit ctx: Context): Unit = {
        // Check purity of overriding methods only.
        // Fields (synthetic getters) are assumed @pure, and type members cannot be @pure.
        //  (A @pure on a class/trait denotes that the constructor method is pure, not the class/trait itself.)
        if (overriding is Method) {
          // Make sure the overriding method conforms to the purity of the overridden method.
          // Conformance is present if either of the two following conditions holds:
          // 1. The overriding method is @pure.
          // 2. The overriding method is inside the purity boundary of the overridden method.
          if (!(overriding.isPure || overriding.isContainedInPurityBoundaryOf(overridden))) {
            // If the override error is due to a purity assumption, issue a warning instead of an error.
            if (assumePure(overridden.purityBoundary))
              ctx.warning(em"$overriding should have a @pure annotation (in ${overriding.lexicallyEnclosingClass}) " +
                em"due to the assumed purity of ${overridden.purityBoundary}.", overriding.pos)
            else
              ctx.error(em"Cannot override with $overriding (in ${overriding.lexicallyEnclosingClass}): " +
                em"It must have a purity level compatible with @pure ${overridden.purityBoundary}.", overriding.pos)
          }
        }
      }

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        if (enableRefChecks) {  // todo: ideally, enableRefChecks==true should prevent a DotModRefChecks from being instantiated in the first place...
          val cls = ctx.owner
          val opc = new OverridingPairs.Cursor(cls)
          while (opc.hasNext) {
            customOverrideCheck(opc.overriding, opc.overridden)
            checkFinals(opc.overriding, opc.overridden)
            checkTypeViews(opc.overriding, opc.overridden)
            checkPurityEffects(opc.overriding, opc.overridden)
            opc.next()
          }
        }
        super.transformTemplate(tree)
      }
    }

  }
}
