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
import dotty.tools.dotc.core.Denotations.SingleDenotation
import transform.TreeTransforms._
import typer._
import typer.ErrorReporting._


object DotMod {


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
    def duplicate(implicit ctx: Context): Type = tp match {
      case tp: TypeRef =>    //tp.newLikeThis(tp.prefix)
        TypeRef.createWithoutCaching(tp.prefix, tp.name)
      case tp: TermRef =>
        TermRef.createWithoutCaching(tp.prefix, tp.name)
      case _ => tp  // TODO: duplicate other types of types
    }
  }


  val MutabilityMember = typeName("$$$$_mutability!$$$$")

  /**
    * A TypeComparer that also compares mutability.
    *
    * @param initCtx the context the comparer operates within
    */
  class DotModTypeComparer(initCtx: Context) extends TypeComparer(initCtx) {

    /*
    def subMutability(info1: Type, info2: Type): Boolean = {
      // if no shadow member exists, assume MutabilityMember is Nothing (mutable)
      if (info1 eq NoType) true
      else if (info2 eq NoType) false
      else isSubType(info1, info2)
    }

    override def isSubType(tp1: Type, tp2: Type): Boolean = {
      super.isSubType(tp1, tp2) && {
        val info1 = tp1.findShadowMember(MutabilityMember, NoPrefix) //, tp1.widenIfUnstable)
        val info2 = tp1.findShadowMember(MutabilityMember, NoPrefix) //, tp2.widenIfUnstable)
        subMutability(info1, info2)
      }
    }
    */

    override def copyIn(ctx: Context): TypeComparer = new DotModTypeComparer(ctx)
  }


  class DotModTyper extends Typer {

    // TODO: Trying not to repeat code
    // alt1: report shadow members directly from findMember. but then, how should we do type comparison?
    // alt2: instead of shadow members, use shadow base types. Treat bases as intersected types.
    //    but how to express lack of mutability?--perhaps a type member in a refinement of the shadow base?
    // Again here we run into the problem of expressing a lack of mutability given an underlying
    // type that has mutability. This seems like a situation where we would want to do an override
    // if the underlying type already contains that type member. Overrides are OK--I think I can
    // live with overrides.

    /*
      ATTEMPT 3b partial failure - Passing the assignment "val e: C = d" where d is C @readonly.

      It is possible that I got the shadow-member check in the type comparer wrong...

      For tp1 <: tp2, if the shadow member exists only in tp2, then t1 is assumed to have the member == Nothing, which should always succeed.
      This is implemented.
      If the shadow member exists only in tp1, then t2 is assumed to have the member == Nothing, which should fail unless t1's member is also Nothing.
      This is not implemented.

      New information: The type comparer is updated to fix this problem, but there is no change in output.

      There is possibly something wrong with the representation of readonly:
      The current implementation uses isSubType to compare RefinedType bases, but maybe this won't give the correct
      result if we want two readonly types to compare equal... possibly, shadow member types should be manipulated
      directly rather than encased inside RefinedTypes.

      What we're after here must have the following relations:
        Readonly <: Readonly
        Untyped <: Readonly
        Untyped <: Untyped
      This is really only a single bit.

      As for polymorphism, we want to compare with an arbitrary type, but only wrt to readonlyness.
      Thus, type members.
      So we're back to comparing shadow types directly, rather than as refined members.
      (The problem with using RefinedTypes to hold these members is that I am skeptical that:
        Object { M <: Any }  <:  Object { M <: Any }
        ).
      So, findMember should still return the requested member early-exit (to get the shadowing effect),
      but I should do the following:
        - return the refinedInfo directly, rather than doing a findMember on the shadow base.
        - make the refinedInfo for Readonly have the property Readonly <: Readonly.
        - choose Nothing as the default refinedInfo. (to get Untyped <: Readonly and Untyped <: Untyped)

      New information: In trying to compare defn.NothingTyoe <: TypeBounds(...), isSubType returns false.
        defn.NothingType is a TypeRef rather than a ClassInfo, which doesn't seem to compare correctly.

      A possibility is to make both halves into covariant TypeBounds.

      SUCCESS: now Untyped <: Readonly returns true as expected.
      New FAILURE: There's now a failure in ReTyper#typedSelect where "new C" is reported
        incompatible with its prototype.
        The specific phase reporting the problem is "Ycheck".
      One option here is to temporarily disable phases after the typer, and deal with
        this issue later (if necessary).

      FAILURE: Still not getting an error on "val e: C = d" after disabling Ycheck options.
        It looks like there is an issue with the fact that I'm not duplicating types before
        setting their shadow members. Since I have "C @readonly" at a prior line, the TypeRef
        to C gets a shadow member, so the use of this TypeRef on the next line is interpreted
        as Readonly. The duplication is probably unnecessary for singleton types (since the
        singleton type means the same type everywhere), but duplication couldn't hurt.

      (Note: shadow members are now copied in Type#newLikeThis.)

      Still not getting the expected failure. The TypeRef to C is still getting a shadow member added.

      New theory: the call to ctx.uniqueNamedTypes.enterIfNew (which is invoked when I try to duplicate
      named types) is not returning a new Type object, but rather is returning the old object because
      the name and prefix are equivalent to the old Type object.
      One option: Attempt to add a flag that adds the new type regardless of prior cached objects.
      A second option: Create and add the object directly in duplicate method, using ctx.uniqueNamedTypes.enterIfNew as an example.

      FAILURE: Still no effect. The TermRef referring to d isn't reporting any shadow members.
      Possibly, changing tree types is not enough. We need to get the shadow types into the symbol info.
      Possibly: Change types in the symbol completer?
     */

    /*
      ATTEMPT 3 partial failure - Putting shadow member checking directly inside the type comparer
       is causing the statement "val c: C @readonly = new C" to interpret the RHS type as Object.
      This is probably due to the constraint solver being unable to find a type T where
       C <: T <: C @readonly. (But this doesn't make sense to me--it should be able to select
       either C or C @readonly and be OK.)

      One possible course of action here is to see where the constraint solver checks subtypes
       and try to figure out why it does this.

      A second course of action is to create a secondary type comparer in the DotMod extension
       and invoke it only when checking certain kinds of trees (e.g., assignment).

      Let's try both.

      New information: The statement above fails even if the custom addition to the type comparer
      is disabled. Perhaps the AnnotatedType itself is causing a problem?
      New information: Stripping the annotations doesn't change the result. Perhaps there is
      something wrong with findMember? (which is the only other place where significant changes have been made?)
      New information: Disabling the shadow-member early-exit in findMember makes the error disappear.
      New information: The shadow-member early exit is taken if the requested member is from Object
        (e.g., method <init>), since Object is the refinement parent of the shadow bases.
        Now trying to filter these requests so we don't get members of Object.

      SUCCESS: Filtering out members of Object in findMember's shadow-member logic seems to work.
        Now making annotation stripping optional.
    */

    def ReadonlyType(implicit ctx: Context) = TypeAlias(defn.ReadonlyAnnotType, 1)


    /**
      * If tpe is an annotated type, finds the meaning of RI annotations.
      * If there is a meaningful RI annotation, sets the first non-annotation underlying type's
      * shadow member.
      *
      * Returns a version of the type that has shadow members, but is stripped of all RI annotations.
      */
    def toShadows(tpe: Type, shadowInfo: Type)(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType =>
        if (tpe.annot.symbol eq defn.ReadonlyAnnot)
          tpe.derivedAnnotatedType(toShadows(tpe.underlying, ReadonlyType), tpe.annot)  // leave RI annotations in place (for now)
        else
          tpe.derivedAnnotatedType(toShadows(tpe.underlying, shadowInfo), tpe.annot)  // leave non-RI annotations in place
      case _ =>
        if (shadowInfo.exists)
          tpe.duplicate.addUniqueShadowMember(MutabilityMember, shadowInfo, visibleInMemberNames = true)
        else
          tpe
    }

    def adaptType(tpe: Type)(implicit ctx: Context): Type = tpe match {

      case tpe: AnnotatedType =>
        toShadows(tpe, NoType)

      case tpe: TermRef =>
        if (tpe.name eq termName("d"))
          true  // breakpoint
        val infoPrefix = tpe.prefix.member(MutabilityMember).info
        val infoTpe = tpe.member(MutabilityMember).info
        val infoCombined = {
          if (infoPrefix.exists && infoTpe.exists)  // if prefix and tpe both have the shadow, combine their types with a union
            OrType(infoPrefix, infoTpe)
          else if (infoPrefix.exists)
            infoPrefix
          else
            infoTpe
        }
        if (infoCombined ne infoTpe)
          tpe.duplicate.addUniqueShadowMember(MutabilityMember, infoCombined, visibleInMemberNames = true)
        else
          tpe

        /*
        val pre = tpe.widenIfUnstable
        val origMemberDenot = tpe.findMember(MutabilityMember, pre, EmptyFlags)
        val newMemberDenot = origMemberDenot & (tpe.prefix.findMember(MutabilityMember, pre, EmptyFlags), pre)
        if (newMemberDenot.exists && (newMemberDenot ne origMemberDenot)) {
          tpe.addUniqueShadowMember(MutabilityMember, newMemberDenot.info, visibleInMemberNames = true)
        }
        */

        // TODO: What about TypeRefs? Should shadow members be transferred, or is automatic prefix substitution sufficient?

      case _ =>
        tpe
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {
      // Find out what type the default typer thinks this tree has.
      val tree = super.adapt(tree0, pt, original)
      val tpe = tree.tpe

      //if (tree.isInstanceOf[tpd.Apply]) println(s"Apply type = $tpe")

      // Do our stuff to the type.
      val tpe1 = adaptType(tpe)

      // Return a tree with the new type.
      val tree1 =
        if (tpe ne tpe1)
          tree.withType(tpe1)
        else
          tree

      // Do we really need a subtype check here?
      val dontCheck = (tpe eq tpe1) || !tpe1.exists || pt.isInstanceOf[ProtoType] || (pt eq WildcardType)
      if (!dontCheck) {
        if (tpe1 <:< pt)
          tree1
        else
          err.typeMismatch(tree1, pt)
      }
      else
        tree1
    }


    /*
    ATTEMPT 2: FAILED.
    Using "shadow members" as separate things from normal members is obviously complicated,
    as it would involve replicating (at a minimum) the implementations of findMember and isSubType.

    However, using adaptApplyResult as a hook into realApply seems like a very good way to extensibly
    handle viewpoint adaptation. In realApply, we have access to the receiver, the result, the
    method denotation, and all arguments.


    def mergeShadowMutabilities(pre: Type, inf: Type)(implicit ctx: Context): Type = OrType(pre, inf)

    // For TermRefs: viewpoint-adapt the prefix.
    def adaptTermRef(tpe: TermRef)(implicit ctx: Context): Type = {
      val prefix = tpe.prefix
      val prefixMember = prefix.findShadowMember(MutabilityMember, NoPrefix)  //, tpe.widenIfUnstable)
      if (prefixMember ne NoType) {
        val unadaptedMember = tpe.findShadowMember(MutabilityMember, NoPrefix)  //, tpe.widenIfUnstable)
        val adaptedMember =
          if (unadaptedMember eq NoType)
            prefixMember
          else
            mergeShadowMutabilities(prefixMember, unadaptedMember)
        tpe.setShadowMember(MutabilityMember, adaptedMember)
      }
      tpe
    }

    // For ThisTypes: viewpoint-adapt based on enclosing-scope annotations.
    def adaptThisType(tpe: ThisType): Type = {
      tpe
    }

    def adaptAnnotatedType(tpe: AnnotatedType)(implicit ctx: Context): Type = {
      if (tpe.annot.symbol eq defn.ReadonlyAnnot) {
        tpe.setShadowMember(MutabilityMember, defn.AnyType)
        //RefinedType(tpe.underlying, MutabilityMember, TypeBounds(defn.NothingType, defn.AnyType))
        tpe
      }
      else
        tpe.derivedAnnotatedType(adaptType(tpe), tpe.annot)
    }

    def adaptType(tpe: Type)(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType => adaptAnnotatedType(tpe)
      case tpe: TermRef if tpe.isStable => adaptTermRef(tpe)
      case tpe: ThisType => tpe
      case _ => tpe
    }

    override def adaptApplyResult(funRef: TermRef, res: Tree)(implicit ctx: Context): Tree = {
      println(s"Apply type = ${res.tpe}")
      println(s"Fun type = ${funRef}")
      val receiver = funRef.prefix
      val methodDefs = funRef.alternatives
      if (funRef.findShadowMember(MutabilityMember, NoPrefix) ne NoType)
        println(funRef.findShadowMember(MutabilityMember, NoPrefix))
      res.tpe.copyShadowMembers(funRef)
      res
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {
      // Find out what type the default typer thinks this tree has.
      val tree = super.adapt(tree0, pt, original)
      val tpe = tree.tpe

      //if (tree.isInstanceOf[tpd.Apply]) println(s"Apply type = $tpe")

      // Do our stuff to the type.
      val tpe1 = adaptType(tpe)

      // Return a tree with the new type.
      if (tpe ne tpe1) tree.withType(tpe1) else tree
    }
   */


    /*
    ATTEMPT 1: FAILED: A RefinedType is not always accepted where a TermRef is accepted.
      One failure instance: in realApply in Applications: methPart(fun1).tpe is expected to be a TermRef.
        The problem here seems to be that the underlying type is a method rather than a reference,
        so one way to fix this is to check that we've actually got a reference type underlying.
      Update: as it turns out, just checking for a non-methodic type is not enough. Also, checking for a stable TermRef is not enough.
      An alternative is to do adaptations only on certain kinds of trees, but this could get ugly.

    val MutabilityMember = typeName("$$$$_mutability!$$$$")

    // For TermRefs: viewpoint-adapt the prefix.
    def adaptTermRef(tpe: TermRef)(implicit ctx: Context): Type = {
      val prefix = tpe.prefix
      prefix.findMember(MutabilityMember, prefix.widenIfUnstable, EmptyFlags) match {
        case NoDenotation => RefinedType(tpe, MutabilityMember, defn.NothingType)
        case denot: SingleDenotation => RefinedType(tpe, MutabilityMember, denot.info)
      }
    }

    // For ThisTypes: viewpoint-adapt based on enclosing-scope annotations.
    def adaptThisType(tpe: ThisType): Type = {
      tpe
    }

    def adaptAnnotatedType(tpe: AnnotatedType)(implicit ctx: Context): Type = {
      if (tpe.annot.symbol eq defn.ReadonlyAnnot)
        RefinedType(tpe.underlying, MutabilityMember, TypeBounds(defn.NothingType, defn.AnyType))
      else
        tpe.derivedAnnotatedType(adaptType(tpe), tpe.annot)
    }

    def adaptType(tpe: Type)(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType => adaptAnnotatedType(tpe)
      case tpe: TermRef => adaptTermRef(tpe)
      case tpe: ThisType => tpe
      case _ => tpe
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      // Find out what type the default typer thinks this tree has.
      val tree = super.adapt(tree0, pt, original)
      val tpe = tree.tpe

      // Do our stuff to the type.
      val tpe1 = adaptType(tpe)

      // TODO: extra type check?

      //if (tpe1 eq tree.tpe) tree
      //else {
      //  val tree1 = tree.withType(tpe1)
      //  if ((ctx.mode is Mode.Pattern) || tpe1 <:< pt) tree1
      //  else err.typeMismatch(tree1, pt)
      //}

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

      if (tpe ne tpe1) tree.withType(tpe1) else tree
    }*/

    override def newLikeThis: Typer = new DotModTyper
  }

  /** This phase runs the regular Scala RefChecks with the DotMod type comparer to enforce necessary
    * subtyping relationships between symbols.
    */
  class DotModRefChecks extends RefChecks {
    //override def run(implicit ctx: Context): Unit = {
    //  super.run(ctx.fresh.setTypeComparerFn(new DotModTypeComparer(_)))
    //}
    override def phaseName: String = "dotmodrefchecks"
  }
}
