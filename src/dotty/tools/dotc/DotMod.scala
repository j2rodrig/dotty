package dotty.tools
package dotc

import ast.{tpd, untpd}
import core._
import core.Annotations._
import core.Contexts._
import core.Denotations._
import core.Flags._
import core.Names._
import core.Symbols._
import core.TypeOpHooks
import core.Types._
import typer._
import typer.ErrorReporting._


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
  }

  implicit class TypeDecorator(val tp: Type) extends AnyVal {
  }


  // CONSTANTS

  val MutabilityMemberName = typeName("__MUTABILITY__")
  val PrefixMutabilityName = typeName("__PREFIX_MUTABILITY__")
  def ReadonlyType(implicit ctx: Context) = TypeAlias(defn.ReadonlyAnnotType, 1)  // info for a readonly mutability member
  def MutableType(implicit ctx: Context) = TypeAlias(defn.MutableAnnotType, 1)    // info for a mutable mutability member



  /******************
    * TYPE COMPARER *
    ******************
    * A TypeComparer that also compares mutability.
    * @param initCtx the context the comparer operates within
    */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    override def isSubType(tp1: Type, tp2: Type): Boolean = {

      // The (tp2 eq WildcardType) check below carries the implicit assumption that
      // the upper bound is readonly.
      // TODO: What do we assume about upper type bounds in the general case? Do we augment type bounds <: Any with a readonly refinement?

      // Handle cases where a mutability member has not been set.
      if (!(tp2.isInstanceOf[ProtoType] || tp2.isError || (tp2 eq WildcardType))) {
        // Special case logic any time tp1 (or its widening) refines the mutability member.
        // We need to check this here because otherwise the ordinary subtyping logic may
        // assume that tp2 does not contain this member, and discard it from tp1.
        tp1.widen match {
          case tp1w: RefinedType =>
            val denot1 = tp1w.member(MutabilityMemberName)
            if (denot1.exists) {
              val denot2 = tp2.member(MutabilityMemberName)
              val info2 = if (denot2.exists) denot2.info else MutableType // default to mutable
              val dropped1 = dropRefinementsNamed(tp1w, List(MutabilityMemberName, PrefixMutabilityName))
              val dropped2 = dropRefinementsNamed(tp2.widen, List(MutabilityMemberName, PrefixMutabilityName))
              val resultInfo = isSubType(denot1.info, info2) // check refined member
              val resultParent = super.isSubType(dropped1, dropped2) // check parents
              return resultInfo && resultParent
            }
          case _ =>
        }

        // Special case logic any time tp2 refines the mutability member, but tp1 does not.
        // If tp1 does not have the mutability member, we default it to mutable.
        // Practically, defaulting tp1's member to mutable just means we ignore the
        // member on tp2 (and allow the type comparison to proceed on tp2's parent).
        tp2 match {
          case tp2: RefinedType if tp2.refinedName eq MutabilityMemberName =>
            val denot1 = tp1.member(MutabilityMemberName)
            if (!denot1.exists)
              return isSubType(tp1, tp2.parent)  // member's OK, but still have to check the parent.

          // Also make sure the presence of a prefix mutability member doesn't affect subtyping
          case tp2: RefinedType if tp2.refinedName eq PrefixMutabilityName =>
            return isSubType(tp1, tp2.parent)    // member's OK, but still have to check the parent.

          case _ =>
        }
      }

      val tp1defaulted = tp1 match {
        // If tp1 refers to the mutability member of another type, but that member doesn't exist, find its default mutability.
        case tp1: TypeRef if (tp1.name eq MutabilityMemberName) && !tp1.denot.exists =>
          tp1.prefix match {
            case _: ThisType =>  // we've got a this-type with polymorphic mutability: don't reduce
              if (tp2 eq defn.ReadonlyAnnotType)  // all this-type mutabilities are <: readonly
                return true
              tp1
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

  def canHaveAnnotations(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case _: TypeRef => true
    case _: RefinedOrRecType => true
    case _: AndOrType => true
    case _: PolyParam => true
    case _: HKApply => true
    case _: TypeVar => true
    case tp: AnnotatedType => canHaveAnnotations(tp.underlying)
    case _ => false
  }

  def isViewpointAdaptable(tp: Type)(implicit ctx: Context): Boolean = canHaveAnnotations(tp) || (tp match {
    case tp: ExprType => isViewpointAdaptable(tp.resultType)
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
    val tpw = tp.widenIfUnstable  // get a stable type (to correctly reference members of the type)
    tpw match {
      case tpw: ThisType =>
        mutabilityOfThis(tpw)
      case tpw: SuperType =>
        mutabilityOfThis(tpw.thistpe.asInstanceOf[ThisType])
      case NoPrefix =>
        defn.MutableAnnotType
      case _ =>
        val mutDenot = tpw.member(MutabilityMemberName)
        if (mutDenot.exists)
          mutDenot.info match {
            case TypeAlias(mut) =>  // tp has a specific mutability alias
              mut
            case _ =>
              TypeRef(tpw, MutabilityMemberName)  // type bounds: not using type bounds right now, but just in case
          }
        else
          defn.MutableAnnotType
    }
  }

  /** Finds the declared mutability of C.this for class C in the current context */
  def mutabilityOfThis(tpe: ThisType)(implicit ctx: Context): TypeRef = {
    def rec(sym: Symbol): TypeRef = {
      val currentOwner = sym.effectiveOwner

      // If sym is non-weak, and its immediate non-weak owner is the class we're looking for,
      // then sym may contain annotations describing the receiver type.
      if (currentOwner eq tpe.cls)
        declaredReceiverType(sym, tpe)

      // It is entirely possible that the current context owner is not inside the class
      // symbol we're looking for. In such cases, we will just return mutable.
      // See test neg/i1050a for an example where mutabilityOfThis is called inside a
      // viewpoint adaptation, which results from a symbol completion that is triggered during a type comparison.
      else if (!currentOwner.exists) {
        // I'm still not convinced that this should be happening, or that returning mutable is the
        // right thing to do. So I'm showing a message so I can see when it happens.
        System.err.println(s"WEIRD WARNING: Mutability of ${tpe.cls.name}.this is being requested from outside of ${tpe.cls}. Assuming @mutable.")
        defn.MutableAnnotType
      }

      else
        rec(currentOwner)
    }

    if (ctx.owner.isPackageObject || ctx.owner.isEffectiveRoot || (ctx.owner.skipWeakOwner eq tpe.cls))
      defn.MutableAnnotType  // we're inside a static module or the class constructor
    else
      rec(ctx.owner.skipWeakOwner)
  }

  /** Looks at the annotations on a symbol.
    * Returns the mutability of the symbol's declared receiver type C.this,
    * where C is expected to be the owning class of the symbol.
    */
  def declaredReceiverType(sym: Symbol, thisTpe: ThisType)(implicit ctx: Context): TypeRef = {
    assert(sym.effectiveOwner eq thisTpe.cls, s"The owner of $sym is expected to be ${thisTpe.cls}")
    // Take the first annotation declared on the symbol
    sym.annotationsWithoutCompleting.foreach { annot =>
      if (annot.symbol eq defn.ReadonlyAnnot)
        return defn.ReadonlyAnnotType
      else if (annot.symbol eq defn.PolyreadAnnot)
        return TypeRef(thisTpe, MutabilityMemberName)
    }
    defn.MutableAnnotType
  }

  def isAssignable(tp: Type)(implicit ctx: Context): Boolean = {
    val prefixMutDenot = tp.member(PrefixMutabilityName)
    !prefixMutDenot.exists || (prefixMutDenot.info <:< MutableType)
  }

  class DotModTypeOpHooks(initCtx: Context) extends TypeOpHooks(initCtx) {

    /** The info of the given denotation, as viewed from the given prefix. */
    override def denotationAsSeenFrom(pre: Type, denot: Denotation): Type = {
      // TODO: check receiver mutability/adaptation info for ExprTypes?

      var target = denot.info
      if (!ctx.erasedTypes && denot.isTerm && !(denot.symbol is Module) && isViewpointAdaptable(target)) {   // do viewpoint adaption for terms only, and not during erasure phase

        // Find prefix mutability.
        val preMut = mutabilityOf(pre)

        // Do a viewpoint adaptation only if the prefix is non-mutable.
        if (preMut ne defn.MutableAnnotType) {
          // Find mutability underlying target
          val underMutDenot = target.member(MutabilityMemberName)
          val underMutBounds = if (underMutDenot.exists) underMutDenot.info.bounds else MutableType

          // Do viewpoint adaptation
          val targetMutBounds = underMutBounds match {
            case tp: TypeAlias =>
              tp.derivedTypeAlias(preMut | tp.alias)
            case tp: TypeBounds =>
              tp.derivedTypeBounds(preMut | tp.lo, preMut | tp.hi)
          }
          target = refineResultMember(target, MutabilityMemberName, targetMutBounds, otherMembersToDrop = List(PrefixMutabilityName))

          // Assignability
          target = refineResultMember(target, PrefixMutabilityName, TypeAlias(preMut, 1))
        }
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

    def customChecks(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: tpd.Assign =>
        if (isAssignable(tree.lhs.tpe))
          tree
        else
          errorTree(tree, d"Cannot assign to unassignable type ${tree.lhs.tpe}")
      case _ => tree
    }

    def convertAnnotationToRefinement(tp: Type)(implicit ctx: Context): Type = tp match {
      case tp: AnnotatedType =>
        if (tp.annot.symbol eq defn.ReadonlyAnnot)
          refineResultMember(tp.underlying, MutabilityMemberName, ReadonlyType)
        else if (tp.annot.symbol eq defn.MutableAnnot)
          refineResultMember(tp.underlying, MutabilityMemberName, MutableType)
        else if (tp.annot.symbol eq defn.MutabilityOfAnnot) {
          val argMutability = mutabilityOf(typed(tp.annot.arguments.head).tpe)
          refineResultMember(tp.underlying, MutabilityMemberName, TypeAlias(argMutability, 1))
        } else
          tp
      case _ =>
        tp
    }

    override def adaptApplyResult(funRef: TermRef, res: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
      val receiverMut = mutabilityOf(funRef.prefix)
      if (receiverMut eq defn.MutableAnnotType)  // if the receiver is mutable, then no checks are needed
        res
      else if (funRef.symbol eq NoSymbol) {
        System.err.println(s"WEIRD WARNING: Method application doesn't have a method symbol. funRef type is: $funRef")
        res
      } else {
        funRef.symbol.effectiveOwner.thisType match {

          // If the owner is not a class, then we're calling a no-prefix method.
          case NoPrefix =>
            res

          // The owner is a class, find the method's declared receiver mutability, and compare with the given receiver mutability
          case thisTpe: ThisType =>
            // Make sure to do a this-substitution on the returned receiver type.
            // Before substitution, @polyread methods have a declared mutability like C.this.__MUTABILITY__,
            // which would not otherwise compare equal to the receiver mutability.
            val declaredMut = declaredReceiverType(funRef.symbol, thisTpe).substThis(thisTpe.cls, funRef.prefix)
            if (receiverMut <:< declaredMut)
              res
            else
              errorTree(res, d"Incompatible receiver mutability in call to method ${funRef.name}:\n" +
                d"  Expected: $declaredMut\n" +
                d"  Got: $receiverMut")
        }
      }
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {
      // Find out what type the default typer thinks this tree has.
      val tree = super.adapt(tree0, pt, original)

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
        case tp: RealTypeBounds if tp.hi eq defn.AnyType =>
          tp.derivedTypeBounds(tp.lo, refineResultMember(tp.hi, MutabilityMemberName, ReadonlyType))

        case tp =>
          tp
      }

      /*
      // Check tpe1 against the prototype with our custom type comparer.
      val dontCheck = ctx.mode.is(Mode.Pattern) || !tpe1.exists || pt.isInstanceOf[ProtoType] || (pt eq WildcardType)
      if (!dontCheck) {
        if (new DotModTypeComparer(ctx).isSubType(tpe1, pt))
          tree1
        else
          err.typeMismatch(tree1, pt)
      }
      else
        tree1
      */
      customChecks(tree.withType(tpe1))
    }

    override def newLikeThis: Typer = new DotModTyper
  }


  /**************
    * REFCHECKS *
    **************
    * This phase runs the regular Scala RefChecks with the DotMod type comparer to enforce necessary
    * subtyping relationships among symbols.
    */
  class DotModRefChecks extends RefChecks {
    //override def run(implicit ctx: Context): Unit = {
    //  super.run(ctx.fresh.setTypeComparerFn(new DotModTypeComparer(_)))
    //}
    override def phaseName: String = "dotmodrefchecks"
  }
}
