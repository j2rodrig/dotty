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


/*
 TODO: Purity. What purity implementation means practically is that closed-over variables will be readonly when seen from within a pure method.
 */

object DotMod {

  // FRONT-END INITIALIZATION

  def customInit(ctx: FreshContext): FreshContext = {
    ctx.setTypeComparerFn(new DotModTypeComparer(_))
    ctx.setTypeOpHooks(new DotModTypeOpHooks(ctx))
    ctx.setTyper(new DotModTyper)
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
            Annotation(typeInContext.typer.typedAnnotation(annotTree)(typeInContext))
          }
      case inf: LazyType =>
        // See <DISCUSSION:LAZY-NON-COMPLETER> in notes.
        assert(!(sym is Method) && !sym.isClass, em"Unexpected LazyType as completer for $sym")
        Nil // if I really do want to process annotations on fields or variables, I maybe should force completions here (instead of returning Nil).
      case _ =>  // no completer, so return already-typed annotations.
        sym.annotations
    }

    /** Finds out whether a symbol is parameterless without forcing the symbol's completion. */
    def isParameterless(implicit ctx: Context): Boolean = sym.infoOrCompleter match {
      case inf: Typer#Completer =>
        // we've got a completer here, so look at the original untyped tree, and check the value parameter list
        inf.original match {
          case original: untpd.DefDef =>
            original.vparamss.isEmpty
          case _ =>
            true
        }
      case info: Type =>  // no completer, so look at the type.
        info.isParameterless
      case _ =>
        assert(false, s"Expected Type or Completer when looking at info of $sym")
        true
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
  }


  // CONSTANTS

  val MutabilityMemberName = typeName("__MUTABILITY__")
  val PrefixMutabilityName = typeName("__PREFIX_MUTABILITY__")
  val OuterMutabilityName  = typeName("__OUTER_MUTABILITY__")
  def ReadonlyType(implicit ctx: Context) = TypeAlias(defn.ReadonlyAnnotType, 1)  // info for a readonly mutability member
  def MutableType(implicit ctx: Context) = TypeAlias(defn.MutableAnnotType, 1)    // info for a mutable mutability member

  lazy val defaultPolyreadArrayMethods: List[TermName] = List (
    "length", "apply", "clone"
  ).map(termName).map(NameTransformer.encode)

  val methodNamesDefaultingToPolyreadReceiver: List[TermName] = List (
    "==", "!=", "asInstanceOf", "isInstanceOf",
    "clone", "eq", "equals", "finalize", "getClass", "hashCode", "ne", "toString"
  ).map(termName).map(NameTransformer.encode)

  val receiverAnnotationNames = List("polyread", "readonly", "mutable", "mutabilityOf", "mutabilityOfRef").map(typeName)
  val namelist_asFinal = List("asFinal").map(typeName)
  val namelist_asType = List("asType").map(typeName)


  /******************
    * TYPE COMPARER *
    ******************
    * A TypeComparer that also compares mutability.
    * @param initCtx the context the comparer operates within
    */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    override def isSubType(tp1: Type, tp2: Type): Boolean = {

      /*
      val show = (tp2 match {
        case RefinedType(_, MutabilityMemberName, _) => true
        case TypeRef(_, _) => true
        case _ => false
      }) || (tp1 match {
        case RefinedType(_, MutabilityMemberName, _) => true
        case TypeRef(_, _) => true
        case _ => false
      })
      if (show) System.err.println(em"comparing $tp1 to $tp2...")
      */

      // Pass basic cases. We assume that Any is a readonly type.
      if ((tp1 eq tp2) || (tp2 eq defn.AnyType) || (tp2 eq WildcardType) || (tp1 eq defn.NothingType))
        return true

      // Since we do custom logic for term types only, pass the prototypes, class infos, bounds, wilds, and errors to the default subtype logic.
      if ((tp2 eq NoType) || tp2.isInstanceOf[ProtoType] || tp2.isInstanceOf[TypeType] || tp2.isInstanceOf[WildcardType] || tp2.isError)
        return super.isSubType(tp1, tp2)


      // Special case logic any time tp1 (or its widening) refines the mutability member.
      // We need to check this here because otherwise the ordinary subtyping logic may
      // assume that tp2 does not contain this member, and discard it from tp1.
      // We use a widenDealias here -- see <DISCUSSION: WIDEN-DEALIAS> in notes.
      tp1.widenDealias.stripAnnots match {
        case tp1w: RefinedType =>
          val denot1 = tp1w.member(MutabilityMemberName)
          if (denot1.exists) {
            val info2 = TypeAlias(mutabilityOf(tp2), 1)
            if (isSubType(denot1.info, info2)) {  // check refined member
              val dropped1 = dropRefinementsNamed(tp1w, List(MutabilityMemberName, PrefixMutabilityName))
              val dropped2 = dropRefinementsNamed(tp2.widen, List(MutabilityMemberName, PrefixMutabilityName))
              return super.isSubType(dropped1, dropped2) // check parents
            } else {
              return false   // subtype failed due to mutability
            }
          }
        case _ =>
      }


      // Special case logic any time tp2 refines the mutability member, but tp1 does not.
      // If tp1 does not have the mutability member, we default it to mutable.
      // Practically, defaulting tp1's member to mutable just means we ignore the
      // member on tp2 (and allow the type comparison to proceed on tp2's parent).
      tp2 match {
        case tp2: RefinedType if tp2.refinedName eq MutabilityMemberName =>
          val info1 = TypeAlias(mutabilityOf(tp1), 1)
          if (isSubType(info1, tp2.refinedInfo)) {  // check refined member
            val dropped2 = dropRefinementsNamed(tp2, List(MutabilityMemberName, PrefixMutabilityName))
            return isSubType(tp1, dropped2) // member's OK, but still have to check the parent.
          } else
            return false

        // Also make sure the presence of a prefix mutability member doesn't affect subtyping
        case tp2: RefinedType if tp2.refinedName eq PrefixMutabilityName =>
          return isSubType(tp1, tp2.parent)    // member's OK, but still have to check the parent.

        case _ =>
      }

      val tp1defaulted = tp1 match {
        // If tp1 refers to the mutability member of another type, but that member doesn't exist, find its default mutability.
        case tp1: TypeRef if (tp1.name eq MutabilityMemberName) && !tp1.denot.exists =>
          tp1.prefix match {
            case _: ThisType =>  // we've got a this-type with polymorphic mutability: don't reduce
              if (tp2 eq defn.ReadonlyAnnotType)  // all this-type mutabilities are <: readonly
                return true
              tp1
            case tr: TypeRef =>  // we've got a mutability of type T#__MUTABILITY__. See <DISCUSSION: POLYMORPHIC_2> in notes.
              if (isSubType(defn.AnyType, tr.info.bounds.hi))  // default to readonly if tr's upper bound is Any
                defn.ReadonlyAnnotType
              else
                defn.MutableAnnotType  // default to mutable if tr's upper bound is any other type
            case _ =>  // something else: default to mutable
              defn.MutableAnnotType
          }
        case _ =>
          tp1
      }
      val tp2defaulted = tp2 match {
        // If tp2 refers to the mutability member of another type, but that member doesn't exist, then default to mutable.
        case tp2: TypeRef if (tp2.name eq MutabilityMemberName) && !tp2.denot.exists =>
          tp2.prefix match {
            case _: ThisType =>  // we've got a this-type with polymorphic mutability: don't reduce
              if (tp1 eq defn.MutableAnnotType)  // all this-type mutabilities are >: mutable
                return true
              tp2
            case tr: TypeRef =>  // we've got a mutability of type T#__MUTABILITY__.
              if (isSubType(defn.AnyType, tr.info.bounds.lo))
                defn.ReadonlyAnnotType  // default to readonly if tr's lower bound is Any
              else
                defn.MutableAnnotType  // reduce to mutable if tr's lower bound is any other type
            case _ =>  // something else: default to mutable
              defn.MutableAnnotType
          }
        case _ =>
          tp2
      }

      super.isSubType(tp1defaulted, tp2defaulted)
    }

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }

  /*
  Found the following in SymDenotation. Could contain useful information for helping avoid cyclic references.
    final def isValueClass(implicit ctx: Context): Boolean = {
      val di = this.initial.asSymDenotation
      di.isClass &&
      di.derivesFrom(defn.AnyValClass)(ctx.withPhase(di.validFor.firstPhaseId))
        // We call derivesFrom at the initial phase both because AnyVal does not exist
        // after Erasure and to avoid cyclic references caused by forcing denotations
    }
   */

  /// Mutability type reduction.
  def reduceMut(m: Type)(implicit ctx: Context): Type = m match {
    case m: NamedType if m.name eq MutabilityMemberName =>
      // We've got a mutability member selection.
      // Reduce by doing a mutabilityOf on the prefix.
      mutabilityOf(m.prefix)
    case _ => m
  }

  /// The mutability of "this" when used by itself in the source program (not as part of a member selection.
  final def findStandaloneThisMutability(tp: ThisType)(implicit ctx: Context): Type =
    findThisMutability(ctx.owner, tp.cls)

  /// The mutability of "this" when used as a prefix, selecting refSym.
  final def findPrefixThisMutability(tp: ThisType, refSym: Symbol)(implicit ctx: Context): Type =
    findThisMutability(refSym, tp.cls)

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
    assert(refSym.exists, em"Attempt to find the mutability of ${ofClass.name}.this outside of $ofClass")
    assert(ofClass.isClass, em"Expected a class in findThisMutability, got $ofClass")
    if (ofClass derivesFrom refSym)  // if we're already at the class we're looking for, return @mutable
      return defn.MutableAnnotType

    // Find the owning class of refSym
    def skipWeakNonClassOwners(s: Symbol): Symbol = if (!s.isClass && s.isWeakOwner) skipWeakNonClassOwners(s.owner) else s
    val classOfSym = skipWeakNonClassOwners(refSym.owner)
    if (!classOfSym.isClass) return findThisMutability(classOfSym, ofClass)   // owner isn't a class, so look at the owner of the owner

    if (ofClass.derivesFrom(classOfSym))  // is the owning class a base of the class we're looking for?
      declaredReceiverType(refSym)   // found it!
    else
      findThisMutability(classOfSym, ofClass)   // keep looking... starting at fromSym's owner
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
    * Returns the mutability of the given type. Optionally, takes the referred-to symbol (assuming tp is a prefix type).
    */
  def mutabilityOf(tp: Type, origSym: Symbol = NoSymbol)(implicit ctx: Context): Type = {
    val tpstable = tp.widenIfUnstable.stripAnnots
    tpstable match {

      // tp is C.this for some class C
      case tp: ThisType =>
        // If we're starting from a static symbol, default to @mutable
        if (origSym.exists && origSym.isStatic || ctx.owner.isStatic)
          defn.MutableAnnotType
        else
          findStandaloneThisMutability(tp)
      case tp: SuperType =>
        // The mutability of super is the same as the mutability of this. See <DISCUSSION: SUPER_THIS_MUT> in the notes.
        mutabilityOf(tp.thistpe, origSym)

      // Basically, we want to avoid doing a member query if tp depends on an abstract/uninstantiated type member.
      //   In such cases, we want tp's mutability to depend directly on that type member,
      //     not default to @mutable. See <DISCUSSION: POLYMORPHIC_2> in notes.
      case tpw =>
        val tpwd = tpw.widenDealias.stripAnnots
        tpwd match {

          // tp widens to an abstract type member - return tp#__MUTABILITY__
          case tp: TypeRef if !tp.symbol.isClass =>
            TypeRef(tp, MutabilityMemberName)
          case tp: TypeVar =>
            TypeRef(tp, MutabilityMemberName)
          case tp: PolyParam =>
            TypeRef(tp, MutabilityMemberName)

          // tp refers to a specific class for which we have an unusual default
          case tp: TypeRef if tp.symbol eq defn.AnyClass =>  // Any is assumed to be readonly
            defn.ReadonlyAnnotType

          case tp1 =>  // got something else - look for mutability member - default to mutable
            val mutDenot = tp1.member(MutabilityMemberName)
            if (mutDenot.exists)
              mutDenot.info.bounds.hi  // (We take the upper bound here. See <DISCUSSION: MUT_BOUNDS> in notes.)
            else
              defn.MutableAnnotType
        }
    }
  }

  /**
    * Returns the mutability of a symbol's declared receiver-mutability type.
    * If no RI annotations are present on the symbol, defaults to @mutable.
    */
  def declaredReceiverType(sym: Symbol)(implicit ctx: Context): Type = {
    if (!(sym is Method) && !sym.isClass)  // ignore annotations on field symbols
      return defn.MutableAnnotType   // todo: not sure if filtering for methods is the right thing to do here (what I want to make sure of is that RI annotations on fields don't get processed in the same way as method annotations)
    // Return the first annotation declared on the symbol
    sym.annotationsWithoutCompleting(receiverAnnotationNames).foreach { annot =>
      val mut = annotationToMutabilityType(annot, sym.lexicallyEnclosingClass)
      if (mut ne NoType)
        return mut
    }
    // No RI annotations found.
    defn.MutableAnnotType
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

    else
      NoType
  }

  def TypeRefUnlessNoPrefix(prefix: Type, name: TypeName)(implicit ctx: Context) =
    if ((prefix eq NoPrefix) || (prefix eq NoType) || prefix.isError)
      NoType
    else
      TypeRef(prefix, name)

  def ignoreReceiverMutabilityWhenCalled(sym: Symbol)(implicit ctx: Context): Boolean = {
    val ownerClassIfExists = sym.effectiveOwner
    (ownerClassIfExists eq defn.AnyClass) ||
    (ownerClassIfExists eq defn.AnyValClass) ||
    (ownerClassIfExists eq defn.ObjectClass) ||
    ((ownerClassIfExists eq defn.ArrayClass) && defaultPolyreadArrayMethods.contains(sym.name))
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
        // Also, we don't just replace the ThisType of the nearest enclosing class. We replace any ThisType.
        case TypeRef(thisTpe, MutabilityMemberName) if thisTpe.isInstanceOf[ThisType] =>
          //System.err.println(em"$thisTpe")
          // tp.termSymbol is passed to mutabilityOf because any this-types we want to replace refer to classes enclosing the method symbol.
          val thisMut = mutabilityOf(thisTpe, tp.termSymbol)
          refineResultMember(tp, MutabilityMemberName, TypeAlias(thisMut, 1), otherMembersToDrop = List(PrefixMutabilityName))
          // We don't bother keeping the __PREFIX_MUTABILITY__ (or assignability) member.
          // It only matters during assignment, and it should be put back automatically if this method is followed by a viewpoint adaptation.
        case _ =>
          tp  // mutability member is not a ThisType. No substitution necessary. (Unless we allow more complicated mutability types that contain this-types!)
      }
  }

  def viewpointAdapt(prefix: Type, target: Type, origSym: Symbol)(implicit ctx: Context): Type = {
    val preMut = mutabilityOf(prefix, origSym)
    val tpMut = mutabilityOf(target, ctx.owner)
    val finalMut = substThisMutability(preMut | tpMut)

    // refine the mutability if the final mutability is different than target mutability
    val tp1 = if (finalMut ne tpMut)
      refineResultMember(target, MutabilityMemberName, TypeAlias(finalMut, 1), otherMembersToDrop = List(PrefixMutabilityName))
    else
      target

    // set the assignability member if the prefix is non-mutable
    if (preMut ne defn.MutableAnnotType)
      refineResultMember(tp1, PrefixMutabilityName, TypeAlias(preMut, 1))
    else
      tp1
  }

  def isViewedAsFinal(sym: Symbol, fromSym: Symbol)(implicit ctx: Context): Boolean = {
    assert(sym.isTerm && !(sym is Method), em"Expected a value term in isViewedAsFinal")
    if (fromSym is Package) return false

    // Look through annotations on fromSym.
    // If there is an @asFinal(ref) present, and the symbol of ref is the given symbol, return true.
    fromSym.annotationsWithoutCompleting(namelist_asFinal).foreach { annot =>
      if ((annot.symbol eq defn.AsFinalAnnot) && (annot.arguments.head.symbol eq sym))
        return true
    }

    // Look up the ownership chain for more annotations.
    isViewedAsFinal(sym, fromSym.owner)
  }

  def findViewedType(sym: Symbol, fromSym: Symbol)(implicit ctx: Context): Type = {
    if (fromSym is Package) return NoType

    // Look through annotations on fromSym.
    // If there is an @asType[T](ref) present, and the symbol of ref is the given symbol, return T.
    fromSym.annotationsWithoutCompleting(namelist_asType).foreach { annot =>
      if ((annot.symbol eq defn.AsTypeAnnot) && (annot.arguments.head.symbol eq sym))
        return getLastTypeArgOf(annot.tree.tpe)
    }

    // Look up the ownership chain for more annotations.
    findViewedType(sym, fromSym.owner)
  }

  def viewedTypeOf(sym: Symbol, origType: Type)(implicit ctx: Context): Type = {
    val viewedTp = findViewedType(sym, ctx.owner)
    if (viewedTp eq NoType)
      origType
    else
      viewedTp
  }

  /*
  def isFinalDueToView(sym: Symbol, rhsTp: Type)(implicit ctx: Context): Boolean = {
    // Do a compatibility check: is tp compatible with the original type of sym (not the viewed type)?
    // >>> Currently a bit hacky -- returns false if both: sym's type has been modified due a type view,
    //     and the RHS type of the assignment is possibly incompatible with sym's original type.
    isViewedAsFinal(sym, ctx.owner) || !(viewedTypeOf(sym, sym.info) <:< sym.info) && !(rhsTp <:< sym.info)
  }
  */

  /** If tp is a named type, returns the non-viewpoint-adapted type of the symbol tp refers to. */
  def nonViewedNamedType(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: NamedType =>
      tp.denot.info  // see <DISCUSSION:GETTING-ORIGINAL-SYMBOL-TYPE> in notes.
    case _ =>
      assert(false, em"Expected NamedType, got $tp")
      tp
  }

  def isAssignable(tp: Type)(implicit ctx: Context): Boolean = {
    val prefixMutDenot = tp.member(PrefixMutabilityName)
    !prefixMutDenot.exists || (prefixMutDenot.info <:< MutableType)
  }

  class DotModTypeOpHooks(initCtx: Context) extends TypeOpHooks(initCtx) {

    /** The info of the given denotation, as viewed from the given prefix. */
    override def denotInfoAsSeenFrom(pre: Type, denot: Denotation): Type = {

      var target = denot.info

      // Don't do custom logic in a types-erased phase
      if (!ctx.erasedTypes) {

        // Do viewpoint adaption for terms only.
        // Note: Changed to canHaveAnnotations from isViewpointAdaptable.
        //   Current theory is that only non-methodic types should be adapted - see <DISCUSSION: EXPR-ADAPTATION> in notes.
        if (denot.isTerm && !(denot.symbol is Module) && canHaveAnnotations(target)) {
          target = viewedTypeOf(denot.symbol, target)         // handle @asType[T](ref) annotations
          target = viewpointAdapt(pre, target, denot.symbol)  // viewpoint-adapt a prefixed term reference
        }

        // Do a substitution of this-type mutabilities.
        if ((denot.symbol is Method) && canHaveAnnotations(target.finalResultType))
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
      // When asking for mutabilityOf(prefix) here, we search from the current context owner (rather than tree.symbol).
      // The reason is that when checkedReceiver is called, this-types are valid with respect to the current
      //  context, but the called method may be in a different tree. E.g., see core/Symbols,
      //  where a search for method Symbols.this.ctx is performed from within Symbols, although ctx is
      //  defined in Contexts (Symbols and Contexts are mixed together).
      val (preMut, declaredMut) = (mutabilityOf(prefix, tree.symbol), declaredReceiverType(tree.symbol))
      //val (preMut, declaredMut) = (mutabilityOf(prefix, prefix.termSymbol), declaredReceiverType(tree.symbol))
      // Make sure to do a this-substitution on the declared receiver type.
      // Before substitution, @polyread methods have a declared mutability like C.this.__MUTABILITY__,
      // which would not otherwise compare equal to the receiver mutability.
      val declared2 = if (prefix ne NoPrefix) declaredMut.substThis(tree.symbol.effectiveOwner.asClass, prefix) else declaredMut
      if (!(preMut <:< declared2)) {
        errorTree(tree, em"Incompatible receiver mutability in call to method ${tree.symbol.name}:\n" +
          em"  Expected: $declaredMut\n" +
          em"  Got: $preMut")
      } else
        tree
    }

    /**
      * preChecks: Checks the following:
      *  - Receiver compatibility on method selections.
      */
    def preChecks(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: tpd.RefTree if (tree.symbol is Method) && !tree.symbol.isStatic && !ignoreReceiverMutabilityWhenCalled(tree.symbol) =>
        tree.tpe match {
          case TermRef(prefix, underlying) =>
            checkedReceiver(prefix, tree)
          case _ =>
            System.err.println(em"WEIRD WARNING: Selection of ${tree.symbol} does not have a TermRef type. Type is: ${tree.tpe}")
            tree
        }
      case _ => tree
    }

    /**
      * customChecks: Checks the following:
      *  - Assignability.
      *  // TODO: check that variable types are compatible with types given in @asType (DefDef and TypeDef trees)
      */
    def customChecks(tree: tpd.Tree, pt: Type)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: tpd.ValDef =>
        tree

      case tree: tpd.Assign =>
        if (!isAssignable(tree.lhs.tpe))
          errorTree(tree.lhs, em"Cannot perform assignment; ${tree.lhs.symbol} is effectively final due to a non-mutable prefix type.")
        else if (isViewedAsFinal(tree.lhs.symbol, ctx.owner))
          errorTree(tree.lhs, em"Cannot perform assignment; ${tree.lhs.symbol} is effectively final due to an @asFinal annotation.")
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
      val mut = reduceMut(annotationToMutabilityType(tp.annot, ctx.owner.lexicallyEnclosingClass))
      if (mut eq NoType)
        tp   // not a valid RI annotation - return type unchanged
      else
        refineResultMember(tp.underlying, MutabilityMemberName, TypeAlias(mut, 1))  // strip annotation, replace with refinement
    }

    override def completeAnnotations(mdef: untpd.MemberDef, sym: Symbol)(implicit ctx: Context): Unit = {
      // Complete annotations in the outer context (rather than current).
      // See <DISCUSSION:LAZY-NON-COMPLETER> and <DISCUSSION:SYM-ANNOT-COMPLETER> in notes.
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
      super.run(ctx.fresh.setTypeComparerFn(new DotModTypeComparer(_)))
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

        if (!(riddenMut <:< ridingMut))
          ctx.error(em"Cannot override $overridden due to receiver mutability.\n" +
            em"   Overridden mutability: $riddenMut\n" +
            em"   Overriding mutability: $ridingMut", overriding.pos)
      }

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo) = {
        val cls = ctx.owner
        val opc = new OverridingPairs.Cursor(cls)
        while (opc.hasNext) {
          customOverrideCheck(opc.overriding, opc.overridden)
          // TODO: check overrides of @asType annotations
          opc.next()
        }
        super.transformTemplate(tree)
      }
    }

  }
}
