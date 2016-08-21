package dotty.tools
package dotc

import ast.{tpd, untpd}
import core._
import core.Annotations._
import core.Contexts._
import core.Decorators.sourcePos
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
    /** Gets annotations on a symbol without forcing the symbol's completion. */
    def annotationsWithoutCompleting(implicit ctx: Context): List[Annotation] = sym.infoOrCompleter match {
      case inf: Typer#Completer =>
        // we've got a completer here, so look at the original untyped tree, and type the annotations directly
        untpd.modsDeco(inf.original.asInstanceOf[untpd.MemberDef]).mods.annotations.map { annotTree =>
          Annotation(ctx.typer.typedAnnotation(annotTree))
        }
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
  }

  implicit class TreeDecorator(val tree: tpd.Tree) extends AnyVal {
  }


  // CONSTANTS

  val MutabilityMemberName = typeName("__MUTABILITY__")
  val PrefixMutabilityName = typeName("__PREFIX_MUTABILITY__")
  def ReadonlyType(implicit ctx: Context) = TypeAlias(defn.ReadonlyAnnotType, 1)  // info for a readonly mutability member
  def MutableType(implicit ctx: Context) = TypeAlias(defn.MutableAnnotType, 1)    // info for a mutable mutability member

  lazy val defaultPolyreadArrayMethods: List[TermName] = List (
    "length", "apply", "clone"
  ).map(termName).map(NameTransformer.encode)

  val methodNamesDefaultingToPolyreadReceiver: List[TermName] = List (
    "==", "!=", "asInstanceOf", "isInstanceOf",
    "clone", "eq", "equals", "finalize", "getClass", "hashCode", "ne", "toString"
  ).map(termName).map(NameTransformer.encode)

  //def newMutabilityOf(tp: Type): Type = tp match {

  //}

  /******************
    * TYPE COMPARER *
    ******************
    * A TypeComparer that also compares mutability.
    * @param initCtx the context the comparer operates within
    */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    override def isSubType(tp1: Type, tp2: Type): Boolean = {

      // Pass basic cases. We assume that Any is a readonly type.
      if ((tp1 eq tp2) || (tp2 eq defn.AnyType) || (tp2 eq WildcardType))
        return true

      // Since we do custom logic for term types only, pass the prototypes, class infos, bounds, wilds, and errors to the default subtype logic.
      if ((tp2 eq NoType) || tp2.isInstanceOf[ProtoType] || tp2.isInstanceOf[TypeType] || tp2.isInstanceOf[WildcardType] || tp2.isError)
        return super.isSubType(tp1, tp2)

      // Special case logic any time tp1 (or its widening) refines the mutability member.
      // We need to check this here because otherwise the ordinary subtyping logic may
      // assume that tp2 does not contain this member, and discard it from tp1.
      tp1.widen match {
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

  /**
    * If tpe is a refinement of a type parameter, returns the refinedInfo's alias type.
    * Note: We shouldn't be getting a type bounds here. (I'm assuming an instantiated type argument is always a TypeAlias because it should be fully defined.)
    * Otherwise, returns NoType.
    */
  def getLastTypeArgOf(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case RefinedType(_, _, TypeAlias(tp)) =>
      tp
    case RefinedType(_, _, TypeBounds(_, _)) =>
      assert(false, d"Unexpected TypeBounds in refinedInfo of $tpe")
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
    case tp: ExprType => isViewpointAdaptable(tp.resultType)
    case tp: PolyType => isViewpointAdaptable(tp.resultType)
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
    case _ =>
      tp
  }

  /** Returns the mutability of the given type. */
  def mutabilityOf(tp: Type)(implicit ctx: Context): Type = {
    tp.widenIfUnstable.stripAnnots match {

      // tp is C.this for some C - look for a declared receiver mutability
      case tp: ThisType =>
        mutabilityOfThis(tp.cls)
      case tp: SuperType =>
        // The mutability of super is the same as the mutability of this. See <DISCUSSION: SUPER_THIS_MUT> in the notes.
        mutabilityOf(tp.thistpe)

      // Basically, we want to avoid doing a member query if tp depends on an abstract/uninstantiated type member.
      //   In such cases, we want tp's mutability to depend directly on that type member,
      //     not default to @mutable. See <DISCUSSION: POLYMORPHIC_2> in notes.
      case tpw =>
        tpw.widenDealias.stripAnnots match {

          // tp widens to an abstract type member - return tp#__MUTABILITY__
          case tp: TypeRef if !tp.symbol.isClass =>
            TypeRef(tp, MutabilityMemberName)
          case tp: TypeVar =>
            TypeRef(tp, MutabilityMemberName)
          case tp: PolyParam =>
            TypeRef(tp, MutabilityMemberName)

          // RefinedType - check if we've got a mutability right here, otherwise check parent
          case tp: RefinedType if tp.refinedName eq MutabilityMemberName => // got a mutability right here
            tp.refinedInfo.bounds.hi
          case tp: RefinedType =>
            mutabilityOf(tp.parent)

          // tp refers to a specific class for which we have an unusual default
          case tp: TypeRef if tp.symbol eq defn.AnyClass =>  // Any is assumed to be readonly
            defn.ReadonlyAnnotType

          case tp1 =>  // got something else - look for mutability member - default to mutable
            val mutDenot = tp1.member(MutabilityMemberName)
            if (mutDenot.exists)
              mutDenot.info.bounds.hi
            else
              defn.MutableAnnotType
        }
    }
  }

  /** Finds the declared mutability of C.this for class C in the current context */
  def mutabilityOfThis(cls: Symbol)(implicit ctx: Context): Type = {
    def rec(sym: Symbol): Type = {
      val currentOwner = sym.effectiveOwner

      // If sym is non-weak, and its immediate non-weak owner is the class we're looking for,
      // then sym may contain annotations describing the receiver type.
      if (currentOwner eq cls)
        declaredReceiverType(sym)

      // It is entirely possible that the current context owner is not inside the class
      // symbol we're looking for. In such cases, we will just return mutable.
      // See test neg/i1050a for an example where mutabilityOfThis is called inside a
      // viewpoint adaptation, which results from a symbol completion that is triggered during a type comparison.
      else if (!currentOwner.exists) {
        // I'm still not convinced that this should be happening, or that returning mutable is the
        // right thing to do. So I'm showing a message so I can see when it happens.
        //System.err.println(s"WEIRD WARNING: Mutability of ${tpe.cls.name}.this is being requested from outside of ${tpe.cls}. Assuming @mutable.")
        defn.MutableAnnotType
      }

      else
        rec(currentOwner)
    }

    if (ctx.owner.isPackageObject || ctx.owner.isEffectiveRoot || (ctx.owner.skipWeakOwner eq cls))
      defn.MutableAnnotType  // we're inside a static module or the class constructor
    else
      rec(ctx.owner.skipWeakOwner)
  }

  /**
    * Returns the mutability of a symbol's declared receiver-mutability type.
    * If no RI annotations are present on the symbol, defaults to @mutable.
    */
  def declaredReceiverType(sym: Symbol)(implicit ctx: Context): Type = {
    // Return the first annotation declared on the symbol
    sym.annotationsWithoutCompleting.foreach { annot =>
      val mut = receiverAnnotationToMutabilityType(annot, sym)
      if (mut ne NoType)
        return mut
    }
    // No RI annotations found.
    defn.MutableAnnotType
  }

  def receiverAnnotationToMutabilityType(annot: Annotation, methodSym: Symbol)(implicit ctx: Context): Type = {
    //assert(methodSym.effectiveOwner.isClass, d"Expected the effective owner of $methodSym to be a class")
    val symbol = annot.symbol

    // @readonly and @mutable
    if (symbol eq defn.ReadonlyAnnot) defn.ReadonlyAnnotType
    else if (symbol eq defn.MutableAnnot) defn.MutableAnnotType

    // @polyread - same as @mutabilityOfRef(this)
    else if (annot.symbol eq defn.PolyreadAnnot) {
      if (!methodSym.effectiveOwner.isClass)
        errorType(d"Only class/trait members may be annotated @polyread (owner ${methodSym.effectiveOwner} is not a class/trait)", symbol.pos)
      else
        TypeRef(methodSym.effectiveOwner.thisType, MutabilityMemberName)
    }

    // @mutabilityOfRef(r) - generates type r.__MUTABILITY__
    else if (annot.symbol eq defn.MutabilityOfRefAnnot)
      TypeRef(ctx.typer.typed(annot.arguments.head).tpe.widenIfUnstable, MutabilityMemberName)

    // @mutabilityOf[T] - generates type T#__MUTABILITY__
    else if (annot.symbol eq defn.MutabilityOfAnnot)
      TypeRef(getLastTypeArgOf(annot.tree.tpe), MutabilityMemberName)

    else
      NoType
  }

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
        case TypeRef(thisTpe, MutabilityMemberName) if thisTpe.isInstanceOf[ThisType] =>
          // Also, we don't just replace the ThisType of the nearest enclosing class. We replace any ThisType.
          val thisMut1 = mutabilityOf(thisTpe)
          refineResultMember(tp, MutabilityMemberName, TypeAlias(thisMut1, 1), otherMembersToDrop = List(PrefixMutabilityName))
          // We don't bother keeping the __PREFIX_MUTABILITY__ (or assignability) member.
          // It only matters during assignment, and it should be put back if this method is followed by a viewpoint adaptation.
        case _ =>
          tp  // mutability member is not a ThisType. No substitution necessary. (Unless we allow more complicated mutability types that contain this-types!)
      }
  }

  def viewpointAdapt(prefix: Type, target: Type)(implicit ctx: Context): Type = {
    val preMut = mutabilityOf(prefix)
    val tpMut = mutabilityOf(target)
    val finalMut = preMut | tpMut  // upper bound of prefix and target mutabilities

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

  def isAssignable(tp: Type)(implicit ctx: Context): Boolean = {
    val prefixMutDenot = tp.member(PrefixMutabilityName)
    !prefixMutDenot.exists || (prefixMutDenot.info <:< MutableType)
  }

  class DotModTypeOpHooks(initCtx: Context) extends TypeOpHooks(initCtx) {

    /** The info of the given denotation, as viewed from the given prefix. */
    override def denotInfoAsSeenFrom(pre: Type, denot: Denotation): Type = {
      val target = substThisMutability(denot.info)

      // do viewpoint adaption for terms only, and not during erasure phase
      if (!ctx.erasedTypes && denot.isTerm && !(denot.symbol is Module) && isViewpointAdaptable(target))
        viewpointAdapt(pre, target)
      else
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
      val (preMut, declaredMut) = (mutabilityOf(prefix), declaredReceiverType(tree.symbol))
      // Make sure to do a this-substitution on the declared receiver type.
      // Before substitution, @polyread methods have a declared mutability like C.this.__MUTABILITY__,
      // which would not otherwise compare equal to the receiver mutability.
      val declared2 = if (prefix ne NoPrefix) declaredMut.substThis(tree.symbol.effectiveOwner.asClass, prefix) else declaredMut
      if (!(preMut <:< declared2)) {
        errorTree(tree, d"Incompatible receiver mutability in call to method ${tree.symbol.name}:\n" +
          d"  Expected: $declaredMut\n" +
          d"  Got: $preMut")
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
            System.err.println(d"WEIRD WARNING: Selection of ${tree.symbol} does not have a TermRef type. Type is: ${tree.tpe}")
            tree
        }
      case _ => tree
    }

    /**
      * customChecks: Checks the following:
      *  - Assignability.
      */
    def customChecks(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: tpd.Assign =>
        if (isAssignable(tree.lhs.tpe))
          tree
        else
          errorTree(tree, d"Cannot assign to unassignable type ${tree.lhs.tpe}")
      case _ => tree
    }

    /**
      * TODO: deprecate this method in favour of mutabilityOf method.
      */
    def convertAnnotationToRefinement(tp: Type)(implicit ctx: Context): Type = tp match {
      case tp: AnnotatedType =>
        if (tp.annot.symbol eq defn.ReadonlyAnnot)
          refineResultMember(tp.underlying, MutabilityMemberName, ReadonlyType)
        else if (tp.annot.symbol eq defn.MutableAnnot)
          refineResultMember(tp.underlying, MutabilityMemberName, MutableType)
        else if (tp.annot.symbol eq defn.MutabilityOfRefAnnot) {
          val argMutability = mutabilityOf(typed(tp.annot.arguments.head).tpe)
          refineResultMember(tp.underlying, MutabilityMemberName, TypeAlias(argMutability, 1))
        } else
          tp
      case _ =>
        tp
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

      customChecks(tree.withType(tpe1))
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
          ctx.error(d"Cannot override $overridden due to receiver mutability.\n" +
            d"   Overridden mutability: $riddenMut\n" +
            d"   Overriding mutability: $ridingMut", overriding.pos)
      }

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo) = {
        val cls = ctx.owner
        val opc = new OverridingPairs.Cursor(cls)
        while (opc.hasNext) {
          customOverrideCheck(opc.overriding, opc.overridden)
          opc.next()
        }
        super.transformTemplate(tree)
      }
    }

  }
}
