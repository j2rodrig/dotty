package dotty.tools
package dotc

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

    override def isSubType(tp1: Type, tp2: Type): Boolean = {
      tp1.isError || tp2.isError || (super.isSubType(tp1, tp2) && {
        val name = MutabilityMember
        val denot1 = tp1.member(name)
        val denot2 = tp2.member(name)
        val info1 = if (denot1.exists) denot1.info else TypeAlias(NothingType, 1)  // default to Nothing
        val info2 = if (denot2.exists) denot2.info else TypeAlias(NothingType, 1)
        if (info1.isError || info2.isError)
          false // breakpoint
        if (denot1.exists || denot2.exists)
          false  // breakpoint
        val result = super.isSubType(info1, info2)
        if (!result)
          false  // breakpoint
        result
      })
    }

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
    Next up: Merge latest Dotty.
    Next up: Do standard tests still pass?
    Next up: What about viewpoint adaptation on ExprTypes?
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
        if (tpe.prefix.isError)
          tpe
        else {
          val denotPrefix = tpe.prefix.member(MutabilityMember)
          val denotTpe = tpe.member(MutabilityMember)
          if (denotPrefix.exists) {
            // we need to change the mutability only if the prefix has a mutability member
            val infoCombined =
            if (denotTpe.exists) // if prefix and tpe both have the shadow, combine their types with a union
              OrType(denotPrefix.info, denotTpe.info)
            else
              denotPrefix.info // only the prefix has a shadow
            tpe.duplicate.addUniqueShadowMember(MutabilityMember, infoCombined, visibleInMemberNames = true)
          } else
            tpe
        }

      case _ =>
        tpe
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {
      // Find out what type the default typer thinks this tree has.
      val tree = super.adapt(tree0, pt, original)
      val tpe = tree.tpe

      // Do our stuff to the type.
      val tpe1 = adaptType(tpe)

      // Return a tree with the new type.
      val tree1 =
        if (tpe ne tpe1)
          tree.withType(tpe1)
        else
          tree

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
    }

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
