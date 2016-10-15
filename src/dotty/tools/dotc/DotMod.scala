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
  val pureAnnotationNames = List("pure").map(typeName)

  val allCustomAnnotations = receiverAnnotationNames ::: finalAnnotationNames ::: typeviewAnnotationNames ::: pureAnnotationNames


  /******************
    * TYPE COMPARER *
    ******************
    * A TypeComparer that also compares mutability.
    * @param initCtx the context the comparer operates within
    */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    override def isSubType(tp1: Type, tp2: Type): Boolean = {

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

          // Check to see if tp2 selects the mutability member of an equivalent type
          tp2 match {
            case tp2: TypeRef if (tp2.name eq MutabilityMemberName) && !tp2.denot.exists =>
              if (isSubType(tp1.prefix, tp2.prefix) && isSubType(tp2.prefix, tp1.prefix))
                return true
            case _ =>
          }

          tp1.prefix match {
            case _: ThisType =>  // we've got a this-type with polymorphic mutability: don't reduce
              if (tp2 eq defn.ReadonlyAnnotType)  // all this-type mutabilities are <: readonly
                return true
              tp1
            case tr: TypeRef =>  // we've got a mutability of type T#__MUTABILITY__. See <DISCUSSION: POLYMORPHIC_2> in notes.

              // New approach as per <DISCUSSION: ABSTRACT-MUT-2> in notes:
              mutabilityOf(tr.info.bounds.hi)

              //if (isSubType(defn.AnyType, tr.info.bounds.hi))  // default to readonly if tr's upper bound is Any
              //  defn.ReadonlyAnnotType
              //else
              //  defn.MutableAnnotType  // default to mutable if tr's upper bound is any other type
            case _ =>  // something else: default to mutable
              defn.MutableAnnotType
          }
        case _ =>
          tp1
      }
      val tp2defaulted = tp2 match {
        // If tp2 refers to the mutability member of another type, but that member doesn't exist, then default to mutable.
        case tp2: TypeRef if (tp2.name eq MutabilityMemberName) && !tp2.denot.exists =>
          System.err.println(em"Comparing: $tp1 <:? $tp2")
          tp2.prefix match {
            case _: ThisType =>  // we've got a this-type with polymorphic mutability: don't reduce
              if (tp1 eq defn.MutableAnnotType)  // all this-type mutabilities are >: mutable
                return true
              tp2
            case tr: TypeRef =>  // we've got a mutability of type T#__MUTABILITY__.

              // New approach as per <DISCUSSION: ABSTRACT-MUT-2> in notes:
              mutabilityOf(tr.info.bounds.lo)

              //if (isSubType(defn.AnyType, tr.info.bounds.lo))
              //  defn.ReadonlyAnnotType  // default to readonly if tr's lower bound is Any
              //else
              //  defn.MutableAnnotType  // reduce to mutable if tr's lower bound is any other type
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

  // Mutability type reduction.
  // WE ARE NO LONGER DOING MUTABILITY REDUCTION on explicit annotations in the source code.
  // Mutability reduction loses important information, e.g. reducing this.__MUTABILITY__ to mutable.
  /* def reduceMut(m: Type)(implicit ctx: Context): Type = m match {
    case m: NamedType if m.name eq MutabilityMemberName =>
      // We've got a mutability member selection.
      // Reduce by doing a mutabilityOf on the prefix.
      mutabilityOf(m.prefix)
    case _ => m
  } */

  /// The mutability of "this" when used by itself in the source program (not as part of a member selection).
  final def findStandaloneThisMutability(tp: ThisType)(implicit ctx: Context): Type =
    findThisMutability(ctx.owner, tp.cls)

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
    assert(refSym.exists, em"Attempt to find the mutability of ${ofClass.name}.this outside of $ofClass")
    assert(ofClass.isClass, em"Expected a class in findThisMutability, got $ofClass")
    if (ofClass derivesFrom refSym)  // if we're already at the class we're looking for, return @mutable
      return defn.MutableAnnotType

    // Find the owning class of refSym
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
    * Returns the assignability (prefix mutability) of the given type.
    * See <ASSIGNABILITY-REVISITED> in notes.
    */
  def assignabilityOf(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TermRef =>
      mutabilityOf(tp.prefix, tp.symbol)
    case _ =>
      System.err.println(em"POSSIBLE ERROR: Unexpected type in assignability check: $tp")
      defn.MutableAnnotType
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

      // Assume annotation constructors are @pure
      (owner derivesFrom defn.AnnotationClass) ||

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
            val thisMut = mutabilityOf(thisTpe, ctx.owner)
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
    case TypeRef(thereThis, MutabilityMemberName) if thereThis.isInstanceOf[ThisType] => here
    case _ => there
  }

  def viewpointAdapt(prefix: Type, target: Type, origSym: Symbol)(implicit ctx: Context): Type = {
    val preMut = mutabilityOf(prefix, origSym)
    val target1 = substThisMutability(target)   // translate this-mutabilities on the target
    val tpMut = mutabilityOf(target1, ctx.owner)
    val finalMut = preMut | tpMut

    // Refine the mutability if the final mutability is different than target mutability.
    val tp1 = if (finalMut ne tpMut)
      refineResultMember(target1, MutabilityMemberName, TypeAlias(finalMut, 1), otherMembersToDrop = List(PrefixMutabilityName))
    else
      target1

    // Set the assignability member if the prefix is non-mutable.
    if (preMut ne defn.MutableAnnotType)
      refineResultMember(tp1, PrefixMutabilityName, TypeAlias(preMut, 1))
    else
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

        if (denot.symbol.name eq termName("w"))
          false

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
      val preMut = mutabilityOf(prefix, tree.symbol)
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
      if (mut eq NoType)
        tp   // not a valid RI annotation - return type unchanged
      else
        refineResultMember(tp.underlying, MutabilityMemberName, TypeAlias(mut, 1))  // strip annotation, replace with refinement
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

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo) = {
        val cls = ctx.owner
        val opc = new OverridingPairs.Cursor(cls)
        while (opc.hasNext) {
          customOverrideCheck(opc.overriding, opc.overridden)
          checkFinals(opc.overriding, opc.overridden)
          checkTypeViews(opc.overriding, opc.overridden)
          checkPurityEffects(opc.overriding, opc.overridden)
          opc.next()
        }
        super.transformTemplate(tree)
      }
    }

  }
}
