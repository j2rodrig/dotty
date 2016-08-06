package dotty.tools
package dotc

import ast.{tpd, untpd}
import core._
import core.Contexts._
import core.Decorators._
import core.Denotations._
import core.Flags._
import core.Names._
import core.Symbols._
import core.TypeOpHooks
import core.Types._
import typer._
import typer.ErrorReporting._


object DotMod {

  def customInit(ctx: FreshContext): FreshContext = {
    ctx.setTypeComparerFn(new DotModTypeComparer(_))
    ctx.setTypeOpHooks(new DotModTypeOpHooks(ctx))
    ctx.setTyper(new DotModTyper)
    ctx
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
  }

  implicit class CompletedSymbolDecorator(val symbol: Symbol) extends AnyVal {
    /** Gets annotations as a list of typed trees (for consistency with UntypedTreeDecorator) */
    def getAnnotations(implicit ctx: Context): List[tpd.Tree] = symbol.annotations.map(_.tree)
  }

  implicit class TypeDecorator(val tp: Type) extends AnyVal {

    /** Creates a new Type equivalent to tp */
    /*
    def duplicate(implicit ctx: Context): Type = tp match {
      case tp: TypeRef =>
        TypeRef.createWithoutCaching(tp.prefix, tp.name)
      case tp: TermRef =>
        TermRef.createWithoutCaching(tp.prefix, tp.name)
      case _ => tp  // TODO: duplicate other types of types
    }
    */
  }


  val MutabilityMemberName = typeName("__MUTABILITY__")
  def ReadonlyType(implicit ctx: Context) = TypeAlias(defn.ReadonlyAnnotType, 1)
  def MutableType(implicit ctx: Context) = TypeAlias(defn.MutableAnnotType, 1)



  /**
    * A TypeComparer that also compares mutability.
    *
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
          case tp1w: RefinedType if tp1w.refinedName eq MutabilityMemberName =>
            val denot2 = tp2.member(MutabilityMemberName)
            val info2 = if (denot2.exists) denot2.info else MutableType  // default to mutable
            val skipped2 = tp2 match {
              case tp2: RefinedType => skipMatching(tp1w, tp2)  // if tp2 has matching refinements, get rid of them
              case _ => tp2
            }
            val resultInfo = super.isSubType(tp1w.refinedInfo, info2)  // check refined member
            val resultParent = super.isSubType(tp1w.parent, skipped2)  // check parents
            return resultInfo && resultParent
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
          case _ =>
        }
      }

      super.isSubType(tp1, tp2)
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

  def refineMutability(tp: Type, mutInfo: TypeBounds)(implicit ctx: Context): Type = {
    if (canHaveAnnotations(tp))
      RefinedType(dropRefinementsNamed(tp, MutabilityMemberName), MutabilityMemberName, mutInfo)
    else tp match {
      case tp: ExprType =>
        tp.derivedExprType(refineMutability(tp.resultType, mutInfo))
      case tp: AnnotatedType =>
        tp.derivedAnnotatedType(refineMutability(tp.underlying, mutInfo), tp.annot)
      case tp: TypeAlias =>
        tp.derivedTypeAlias(refineMutability(tp.alias, mutInfo))
      case tp: TypeBounds =>
        tp.derivedTypeBounds(tp.lo, refineMutability(tp.hi, mutInfo))
      case tp: TypeLambda =>
        tp.derivedTypeLambda(tp.paramNames, tp.paramBounds, refineMutability(tp.resultType, mutInfo))
      case tp: WildcardType =>
        if (tp.optBounds.exists)
          tp.derivedWildcardType(refineMutability(tp.optBounds, mutInfo))
        else
          tp.derivedWildcardType(TypeBounds(defn.NothingType, RefinedType(defn.AnyType, MutabilityMemberName, mutInfo)))
      case _ =>
        tp
    }
  }

  def dropRefinementsNamed(tp: Type, name: Name)(implicit ctx: Context): Type = tp match {
    case tp: RefinedType if tp.refinedName eq name =>
      dropRefinementsNamed(tp.underlying, name)
    case _ =>
      tp
  }

  class DotModTypeOpHooks(initCtx: Context) extends TypeOpHooks(initCtx) {

    /** The info of the given denotation, as viewed from the given prefix. */
    override def denotationAsSeenFrom(pre: Type, denot: Denotation): Type = {
      var target = denot.info
      if (denot.isTerm && isViewpointAdaptable(target) && !ctx.erasedTypes) {   // do viewpoint adaption for terms only, and not during erasure phase
        val prefixMutabilityDenot = pre.member(MutabilityMemberName)
        val prefixMutability = if (prefixMutabilityDenot.exists) prefixMutabilityDenot.info.asInstanceOf[TypeBounds].hi else defn.MutableAnnotType
        val underlyingMutabilityDenot = target.member(MutabilityMemberName)
        val underlyingMutabilityBounds = if (underlyingMutabilityDenot.exists) underlyingMutabilityDenot.info else MutableType
        val targetMutabilityBounds = underlyingMutabilityBounds match {
          case tp: TypeAlias =>
            tp.derivedTypeAlias(simplifiedOrType(prefixMutability, tp.alias))
          case tp: TypeBounds =>
            tp.derivedTypeBounds(tp.lo, simplifiedOrType(prefixMutability, tp.hi))
        }
        if (targetMutabilityBounds ne underlyingMutabilityBounds)
          target = refineMutability(target, targetMutabilityBounds)
      }
      target
    }

    def simplifiedOrType(tp1: Type, tp2: Type): Type = {
      if (tp1 eq tp2) tp1
      else if (tp1 eq defn.ReadonlyAnnotType) tp1
      else if (tp2 eq defn.ReadonlyAnnotType) tp2
      else if (tp1 eq defn.MutableAnnotType) tp2
      else if (tp2 eq defn.MutableAnnotType) tp1
      else OrType(tp1, tp2)
    }

    /** A new object of the same type as this one, using the given context. */
    override def copyIn(ctx: Context) = new DotModTypeOpHooks(ctx)
  }

  class DotModTyper extends Typer {

    def convertAnnotationToRefinement(tp: Type)(implicit ctx: Context): Type = tp match {
      case tp: AnnotatedType =>
        if (tp.annot.symbol eq defn.ReadonlyAnnot)
          refineMutability(tp.underlying, ReadonlyType)
        else if (tp.annot.symbol eq defn.MutableAnnot)
          refineMutability(tp.underlying, MutableType)
        else if (tp.annot.symbol eq defn.MutabilityOfAnnot) {
          val argTpe = typed(tp.annot.arguments.head).tpe
          refineMutability(tp.underlying, TypeAlias(TypeRef(argTpe, MutabilityMemberName), 1))
        } else
          tp
      case _ => tp
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {
      // Find out what type the default typer thinks this tree has.
      val tree = super.adapt(tree0, pt, original)

      // Turn RI annotations into type refinements.
      val tpe1 = convertAnnotationToRefinement(tree.tpe)

      // Return a tree with the new type.
      val tree1 =
        if (tree.tpe ne tpe1) {
          if (!canHaveAnnotations(tree.tpe))
            errorTree(tree, "Reference immutability annotations are not allowed here")
          else
            tree.withType(tpe1)
        } else
          tree

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
      tree1
    }

    override def newLikeThis: Typer = new DotModTyper
  }

  /** This phase runs the regular Scala RefChecks with the DotMod type comparer to enforce necessary
    * subtyping relationships among symbols.
    */
  class DotModRefChecks extends RefChecks {
    //override def run(implicit ctx: Context): Unit = {
    //  super.run(ctx.fresh.setTypeComparerFn(new DotModTypeComparer(_)))
    //}
    override def phaseName: String = "dotmodrefchecks"
  }
}
