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
import transform.OverridingPairs
import transform.TreeTransforms.TransformerInfo
import typer._
import typer.ErrorReporting._
import util.NameTransformer
import util.Positions.Position


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

  val methodNamesDefaultingToPolyreadReceiver: List[TermName] = List (
    "==", "!=", "asInstanceOf", "isInstanceOf",
    "clone", "eq", "equals", "finalize", "getClass", "hashCode", "ne", "toString"
  ).map(termName).map(NameTransformer.encode)


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
            val denot2 = tp2.member(MutabilityMemberName)
            val info2 = if (denot2.exists) denot2.info else MutableType // default to mutable
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
          val denot1 = tp1.member(MutabilityMemberName)
          if (!denot1.exists) {
            if ((tp1 eq defn.AnyType) && !isSubType(ReadonlyType, tp2.refinedInfo))  // special case: assume Any is readonly
              return false
            else
              return isSubType(tp1, tp2.parent) // member's OK, but still have to check the parent.
          }

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
    case tp: ExprType => isViewpointAdaptable(tp.resultType)
    case tp: MethodType => isViewpointAdaptable(tp.resultType)
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
    val tpw = tp.widenIfUnstable  // get a stable type (to correctly reference members of the type)
    tpw match {
      case tpw: ThisType =>
        mutabilityOfThis(tpw)
      case tpw: SuperType =>
        // The mutability of super is the same as the mutability of this. See <DISCUSSION: SUPER_THIS_MUT> in the notes.
        mutabilityOf(tpw.thistpe)
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
        //System.err.println(s"WEIRD WARNING: Mutability of ${tpe.cls.name}.this is being requested from outside of ${tpe.cls}. Assuming @mutable.")
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
  def declaredReceiverType(sym: Symbol, thisTpe: Type)(implicit ctx: Context): TypeRef = {
    if (thisTpe eq NoPrefix) return defn.MutableAnnotType
    assert(sym.effectiveOwner eq thisTpe.asInstanceOf[ThisType].cls, s"The owner of $sym is expected to be ${thisTpe.asInstanceOf[ThisType].cls}")
    // Take the first annotation declared on the symbol
    sym.annotationsWithoutCompleting.foreach { annot =>
      if (annot.symbol eq defn.ReadonlyAnnot)
        return defn.ReadonlyAnnotType
      else if (annot.symbol eq defn.MutableAnnot)
        return defn.MutableAnnotType
      else if ((annot.symbol eq defn.PolyreadAnnot) || (annot.symbol eq defn.FreshAnnot))
        return TypeRef(thisTpe, MutabilityMemberName)
      else if (annot.symbol eq defn.MutabilityOfAnnot)
        return TypeRef(ctx.typer.typed(annot.arguments.head).tpe.widenIfUnstable, MutabilityMemberName)
    }
    // No annotations found on the symbol, so choose a default
    //defaultedReceiverType(sym, thisTpe)
    defn.MutableAnnotType
  }

  /*
  def defaultedReceiverType(sym: Symbol, thisTpe: ThisType)(implicit ctx: Context): TypeRef = {
    if (sym.isParameterless /*|| methodNamesDefaultingToPolyreadReceiver.contains(sym.name)*/)
      TypeRef(thisTpe, MutabilityMemberName)

    // All methods on AnyVal are also considered polymorphic in receiver mutability
    //else if (thisTpe.cls eq defn.AnyValClass)
    //  TypeRef(thisTpe, MutabilityMemberName)

    else
      defn.MutableAnnotType
  }*/

  def ignoreReceiverMutabilityWhenCalled(sym: Symbol)(implicit ctx: Context): Boolean = {
    val ownerClassIfExists = sym.effectiveOwner
    //methodNamesDefaultingToPolyreadReceiver.contains(sym.name) ||
      (ownerClassIfExists eq defn.AnyClass) ||
      (ownerClassIfExists eq defn.AnyValClass) ||
      (ownerClassIfExists eq defn.ObjectClass)
  }

  /**
    * Replaces a recursive this-mutability with its equivalent in the current context.
    * For example, when calling a polyread method d from a non-polyread method e:
    *   class C {
    *     @polyread def d: Any @mutabiliyOf(this) = ???   // here, @mutabilityOf(this) means { __MUTABILITY__ = C.this.__MUTABILITY__ }
    *     def e: Any = d   // here, @mutabilityOf(this) should mean { __MUTABILITY__ = mutable }
    *   }
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
        case _ =>
          tp  // mutability member is not a ThisType. No substitution necessary. (Unless we allow more complicated mutability types that contain this-types!)
      }
  }

  def viewpointAdapt(prefix: Type, target: Type)(implicit ctx: Context): Type = {
    val target1 = substThisMutability(target)
    val preMut = mutabilityOf(prefix)   // get prefix mutability
    val tpMut = target1.finalResultType.member(MutabilityMemberName) match {  // get target mutability
      case NoDenotation =>
        defn.MutableAnnotType
      case d =>
        // Find the mutability of the denotation. (We take the upper bound here. See <DISCUSSION: MUT_BOUNDS> in notes.)
        d.info.bounds.hi
    }
    val finalMut = preMut | tpMut  // upper bound of prefix and target mutabilities

    // refine the mutability if the final mutability is different than target mutability
    val tp1 = if (finalMut ne tpMut)
      refineResultMember(target1, MutabilityMemberName, TypeAlias(finalMut, 1), otherMembersToDrop = List(PrefixMutabilityName))
    else
      target1

    // set the assignability member if the prefix is non-mutable
    if (preMut ne defn.MutableAnnotType)
      refineResultMember(tp1, PrefixMutabilityName, TypeAlias(preMut, 1))
    else
      tp1
  }

  def checkReceiverMutability(prefix: Type, sym: Symbol, pos: Position)(implicit ctx: Context): Unit = {
    val declaredMut = declaredReceiverType(sym, sym.effectiveOwner.thisType.asInstanceOf[ThisType])
    val prefixMut = mutabilityOf(prefix)
    if (!(prefixMut <:< declaredMut))
      ctx.error(d"Incompatible receiver mutability in call to method ${sym.name}:\n" +
        d"  Expected: $declaredMut\n" +
        d"  Got: $prefixMut",
        pos)
  }

  def isAssignable(tp: Type)(implicit ctx: Context): Boolean = {
    val prefixMutDenot = tp.member(PrefixMutabilityName)
    !prefixMutDenot.exists || (prefixMutDenot.info <:< MutableType)
  }

  class DotModTypeOpHooks(initCtx: Context) extends TypeOpHooks(initCtx) {

    /** The info of the given denotation, as viewed from the given prefix. */
    override def denotInfoAsSeenFrom(pre: Type, denot: Denotation): Type = {
      var target = denot.info
      if (!ctx.erasedTypes && denot.isTerm && !(denot.symbol is Module) && isViewpointAdaptable(target)) {   // do viewpoint adaption for terms only, and not during erasure phase

        //checkReceiverMutability(pre, denot.symbol)

        if (denot.symbol.name eq termName("asInstanceOf"))
          false  // br

        //System.err.println(d"Target ${denot.symbol.name} result is: $target, prefix is: $pre")
        //System.err.println(d"Target mutability is: ${mutabilityOf(target)}, prefix mut is: ${mutabilityOf(pre)}")
        target = viewpointAdapt(pre, target)
        //System.err.println(d"Target ${denot.symbol.name} adapted to: $target")
        /*
        //def substThisIfNeeded(tp: Type): Type =
        //  if ((pre ne NoPrefix)
        val ownerClass = denot.symbol.effectiveOwner.asClass

        // Find prefix mutability.
        val preMut = mutabilityOf(pre)

        // Do a viewpoint adaptation only if the prefix is non-mutable.
        //if (preMut ne defn.MutableAnnotType) {
          // Find mutability underlying target
          val underDenot = target.member(MutabilityMemberName)
          val underMut =
            if (underDenot.exists) underDenot.info.asInstanceOf[TypeAlias].alias
            else declaredReceiverType(denot.symbol, ownerClass.thisType.asInstanceOf[ThisType])

          // Do viewpoint adaptation
          val targetMut = preMut.substThis(ownerClass, pre) | underMut.substThis(ownerClass, pre)
          /*underMutBounds match {
            case tp: TypeAlias =>
              tp.derivedTypeAlias((preMut | tp.alias).substThis(denot.symbol.effectiveOwner.asClass, pre))
            //case tp: TypeBounds =>
            //  tp.derivedTypeBounds(preMut | tp.lo, preMut | tp.hi)
          }*/
          target = refineResultMember(target, MutabilityMemberName, TypeAlias(targetMut, 1), otherMembersToDrop = List(PrefixMutabilityName))

          // Assignability
          target = refineResultMember(target, PrefixMutabilityName, TypeAlias(preMut, 1))
          */
        //}
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
      if ((tree.symbol is Method) && !ignoreReceiverMutabilityWhenCalled(tree.symbol)) {
        val (preMut, declaredMut) = (mutabilityOf(prefix), declaredReceiverType(tree.symbol, tree.symbol.effectiveOwner.thisType))
        // Make sure to do a this-substitution on the declared receiver type.
        // Before substitution, @polyread methods have a declared mutability like C.this.__MUTABILITY__,
        // which would not otherwise compare equal to the receiver mutability.
        val declared2 = if (prefix ne NoPrefix) declaredMut.substThis(tree.symbol.effectiveOwner.asClass, prefix) else declaredMut
        if (!(preMut <:< declared2))
          errorTree(tree, d"Incompatible receiver mutability in call to method ${tree.symbol.name}:\n" +
            d"  Expected: $declaredMut\n" +
            d"  Got: $preMut")
        else
          tree
      } else
        tree
    }

    /*
    def viewpointAdaptTree(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case _: tpd.Ident | _: tpd.Select if !(tree.symbol is Module) =>
        if (tree.asInstanceOf[tpd.NameTree].name eq termName("im"))
          false
        tree.tpe match {
          case tpe: TermRef =>
            val adapted = viewpointAdapt(tpe.prefix, tpe.underlying)
            tree.withType(adapted)
          case _ => tree
        }
      case _ => tree
    }
    */

    def preChecks(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = tree match {
      case tree: tpd.Ident if (tree.symbol ne NoSymbol) && !tree.symbol.isStatic =>
        tree.symbol.effectiveOwner.thisType match {
          case NoPrefix =>
            tree
          case thisTpe: ThisType =>
            checkedReceiver(mutabilityOfThis(thisTpe), tree)
        }
      case tree: tpd.Select =>
        checkedReceiver(tree.qualifier.tpe, tree)
      case _ => tree
    }

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

    /*override def adaptApplyResult(funRef: TermRef, res0: tpd.Tree)(implicit ctx: Context): tpd.Tree = {

      // get a version of the result where recursive this-type mutabilities are substituted out for non-recursive mutabilities where possible
      val res = res0.withType(substThisMutability(res0.tpe))

      System.err.println(d"In adaptApplyResult($funRef)")
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
            //if (!ignoreReceiverMutabilityWhenCalled(funRef.symbol, thisTpe))
            //  res
            //else
            {
              // Make sure to do a this-substitution on the returned receiver type.
              // Before substitution, @polyread methods have a declared mutability like C.this.__MUTABILITY__,
              // which would not otherwise compare equal to the receiver mutability.
              val declaredMut = declaredReceiverType(funRef.symbol, thisTpe)  //.substThis(thisTpe.cls, funRef.prefix)
              if (receiverMut <:< declaredMut) {
                //if (methodNamesDefaultingToPolyreadResult.contains(funRef.symbol.name) && !funRef.symbol.info.finalResultType.member(MutabilityMemberName).exists)
                //  res.withType(refineResultMember(res.tpe, MutabilityMemberName, TypeAlias(mutabilityOf(funRef.prefix), 1)))
                //else
                res //.defaultedReceiverAdaptation(funRef.symbol, funRef.prefix)
              }
              else
                errorTree(res, d"Incompatible receiver mutability in call to method ${funRef.name}:\n" +
                  d"  Expected: $declaredMut\n" +
                  d"  Got: $receiverMut")
            }
        }
      }
    }*/

    /*  // an attempt to viewpoint-adapt match cases
    override def typedBind(tree: untpd.Bind, pt: Type)(implicit ctx: Context): tpd.Tree = {
      val tpdBind = super.typedBind(tree, pt)
      tpdBind.withType(tpdBind.tpe.adaptWith(pt))
    }*/

    /*
    // an attempt to set default mutabilities for parameterless-method results
    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
      val tpt1 =
        if (sym.isParameterless)
          ddef.tpt match {
            case _: untpd.TypeTree =>
              ddef.tpt
            case _: untpd.Annotated =>
              ddef.tpt
            case tpt =>
              //Annotated(Apply(Select(New(Ident(mutabilityOf)),<init>),List(This())),Ident(C))
              //untpd.Annotated(untpd.Apply(untpd.Select(untpd.New(untpd.Ident(typeName("mutabilityOf"))), termName("<init>")), List(untpd.This(sym.effectiveOwner.name.asTypeName))), original)
              var tpe = typedType(tpt).tpe
              if (canHaveAnnotations(tpe) && !tpe.member(MutabilityMemberName).exists)
                sym.effectiveOwner.thisType match {
                  case thisTpe: ThisType =>
                    tpe = refineResultMember(tpe, MutabilityMemberName, TypeAlias(declaredReceiverType(sym, thisTpe), 1))
                  case _ =>
                }
              untpd.TypeTree(tpt).withType(tpe)
          }
        else
          ddef.tpt
      super.typedDefDef(untpd.cpy.DefDef(ddef)(tpt = tpt1), sym)
    }*/

    override def adaptInterpolated(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      val tree01 = preChecks(tree0)
      if (tree01.tpe.isError)
        return tree01

      /*val tpe01 = tree01.tpe match {
        case tpe: TermRef if !tpe.symbol.isStatic && isViewpointAdaptable(tpe.underlying) =>
          val vptu = viewpointAdapt(tpe.prefix, tpe.underlying)
          TermRef(tpe, vptu)
        case tp =>
          tp
      }*/

      // Find out what type the default typer thinks this tree has.
      val tree = super.adaptInterpolated(tree01, pt, original)
      if (tree.tpe.isError)
        return tree

      // Add refinements to certain types.
      var tpe1 = tree.tpe match {

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

      /*// For parameterless defs: default result mutability is receiver mutability
      tree match {
        case tree: tpd.DefDef if tree.symbol.isParameterless && !tpe1.finalResultType.member(MutabilityMemberName).exists =>
          tree.symbol.effectiveOwner.thisType match {
            case thisType: ThisType =>
              tpe1 = refineResultMember(tpe1, MutabilityMemberName, TypeAlias(declaredReceiverType(tree.symbol, thisType), 1))
            case _ =>
          }
        case _ =>
      }*/

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
        var riddenMut = declaredReceiverType(overridden, overridden.effectiveOwner.thisType)
            .substThis(overridden.effectiveOwner.asClass, overriding.effectiveOwner.thisType)
        var ridingMut = declaredReceiverType(overriding, overriding.effectiveOwner.thisType)

        // Where a field is overriding a method, readjust its receiver to @mutabilityOf(C.this) where C is the overriding symbol's class.
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
