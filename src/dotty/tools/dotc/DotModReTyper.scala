package dotty.tools.dotc

import ast.tpd
import ast.untpd
import ast.tpd._
import core._
import core.Annotations._
import core.Contexts._
import core.Decorators._
import core.DenotTransformers._
import core.Phases._
import core.Symbols._
import core.Types._
import dotty.tools.dotc.typer.NoChecking
import transform.TreeTransforms._

// TODO: build the first phase as a Phase with a ReTyper? See Erasure phase:
// val eraser = new Erasure.Typer
// def run(implicit ctx: Context): Unit = {
//   val unit = ctx.compilationUnit
//   unit.tpdTree = eraser.typedExpr(unit.tpdTree)(ctx.fresh.setPhase(this.next))
// }


/**
 * A compiler extension for reference immutability.
 *
 * There is a difference between
 *
 * LAYER0:
 *   Mapping of (possibly annotated) types to viewpoint-adapted types. (TermRefs)
 * LAYER1:
 *   Parsing outer-accessor annotations on methods and classes.
 *   Determination of mutability type of NoPrefix symbols. Symbols that reach into enclosing environments must take outer accessors into account.
 *   Determination of mutability of ThisTypes?
 * LAYER2:
 *   Type comparer that understands mutability annotations.
 * LAYER3:
 *   Viewpoint adaptation involving getter-like methods.
 *   - Getter-like methods include Scala getter style (def x: T) and JavaBeans style (def getX(): t).
 * LAYER4:
 *   Checking that the creation of a new object passes a compatible outer reference.
 *   Checking that "extends" types are valid for construction.
 *
 * ORDERS OF BUSINESS:
 * 1. Type Comparer. The type comparer here needs only compare mutabilities.
 *
 */
object DotModReTyper {

  private var _mutableAnnotation: TypeRef = null
  def mutableAnnot(implicit ctx: Context) = {
    if (_mutableAnnotation eq null) {
      _mutableAnnotation = ctx.requiredClassRef("scala.annotation.mutable")
    }
    _mutableAnnotation.symbol.asClass
  }

  private var _polyreadAnnotation: TypeRef = null
  def polyreadAnnot(implicit ctx: Context) = {
    if (_polyreadAnnotation eq null) {
      _polyreadAnnotation = ctx.requiredClassRef("scala.annotation.polyread")
    }
    _polyreadAnnotation.symbol.asClass
  }

  private var _readonlyAnnotation: TypeRef = null
  def readonlyAnnot(implicit ctx: Context) = {
    if (_readonlyAnnotation eq null) {
      _readonlyAnnotation = ctx.requiredClassRef("scala.annotation.readonly")
    }
    _readonlyAnnotation.symbol.asClass
  }

  def canAnnotateType(annot: Annotation)(implicit ctx: Context) = {
    annot.symbol == mutableAnnot || annot.symbol == readonlyAnnot || annot.symbol == polyreadAnnot
  }

  def canAnnotateMethod(annot: Annotation)(implicit ctx: Context) = {
    annot.symbol == mutableAnnot || annot.symbol == readonlyAnnot || annot.symbol == polyreadAnnot
  }

  def canAnnotateClass(annot: Annotation)(implicit ctx: Context) = {
    annot.symbol == mutableAnnot || annot.symbol == readonlyAnnot
  }


  class ModPhase extends Phase {

    override def phaseName: String = "DotMod"

    val typer = new DotModReTyper.Typer

    def run(implicit ctx: Context): Unit = {
      val unit = ctx.compilationUnit
      unit.tpdTree = typer.typedExpr(unit.tpdTree)(ctx.fresh.setPhase(this.next))
    }

  }

  class Typer extends typer.ReTyper {  // with NoChecking {
    //override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
    //  println("typedSelect: " + tree)
    //  super.typedSelect(tree, pt)
    //}

    override def typedUnadapted(initTree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
      assert(initTree.isInstanceOf[Tree], "Expected a typed tree")
      initTree match {
        case tree: UnApply => tree
        case _ =>
          super.typedUnadapted(initTree, pt)
      }
    }

    override def adaptInterpolated(tree: Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): Tree = {
      // Don't do anything with the tree here.
      // The tree should have been adapted already in the front end typer.
      tree
    }
  }


  implicit class TypeDecorator(val tpe: Type) extends AnyVal {
    def withAnnotation(cls: ClassSymbol)(implicit ctx: Context) = {
      val tree = tpd.New(cls.typeRef, Nil)
      AnnotatedType(tpe, new ConcreteAnnotation(tree))
    }
  }


  //sealed trait BoundSpec
  //case object Hi extends BoundSpec
  //case object Lo extends BoundSpec

  /**
   * Returns the mutability portion of the given type.
   * The returned type, if monomorphic, has the form Any@mutable or Any@readonly.
   * The returned type may include polymorphic components P, provided Any@mutable <: P.
   */
  def getMutability(tp: Type)(implicit ctx: Context): Type = {
    def gotAnnotation(annot: Annotation) = AnnotatedType(ctx.definitions.AnyType, annot)

    tp match {
      case AnnotatedType(tpe, annot) =>
        if (canAnnotateType(annot))
          gotAnnotation(annot)
        else
          getMutability(tpe)
      case tp: TermRef =>
        // field reads as getter calls


        // TODO: viewpoint adaptation on actual getter calls
        tp

      case tp: ThisType => tp  // TODO: outer accessors to enclosing classes

      case _ => tp
    }
  }

  //def getMutability(tp: Type, bound: BoundSpec)(implicit ctx: Context):  // doesn't work for polymorphic types

  /**
   * A class for comparing the mutabilities of types.
   *
   * This class is a drop-in replacement for TypeComparer.
   *
   */
  class MutabilityComparer(initctx: Context) extends TypeComparer(initctx) {

    def isSubMutability(tp1: Type, tp2: Type): Boolean = {
      if (tp1 eq tp2) true
      else if (tp2 eq NoType) false
      else tp2 match {
        case tp2: AnnotatedType =>
          if (canAnnotateType(tp2.annot))
            isSubMutability(tp1, tp2.underlying)  // skip non-reference-immutability annotations
          else
            true //isSubMutability(tp1) // TODO

        case tp2: TermRef =>
          // TermRefs are viewpoint adapted:
          // Check that tp1 <:< (tp2.prefix & tp2.underlying).
          //  - tp2.underlying calls tp2.info, which should have been transformed by ModPhase.
          isSubMutability(tp1, tp2.prefix) && isSubMutability(tp1, tp2.underlying)

      }
    }

    override def isSubType(tp1: Type, tp2: Type): Boolean = {
      if (tp1 eq tp2) true
      else if (!super.isSubType(tp1, tp2)) false
      else isSubMutability(tp1, tp2)
    }

    override def copyIn(ctx: Context): TypeComparer = new MutabilityComparer(ctx)
  }

  //def checkConstructibility(tp: Type)(implicit ctx: Context) = {
  //TODO: make sure the TypeRef prefix path is compatible with the outer accessor on the named symbol class
  //}

  /**
   * This phase performs:
   * 1. viewpoint adaptation of symbols and identifiers to reflect access to outer environments.
   * 2. viewpoint adaptation of method calls.
   * 3. viewpoint adaptation of field selections (equivalent to synthetic getter method calls).
   *
   * N. conversion of annotated types to type intersections.
   *
   *
   */

  // TODO: The first phase as a ReTyper?

  class ModPhaseDep extends MiniPhaseTransform with AnnotationTransformer with InfoTransformer {
    override def phaseName = "dotmod"

    override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
      // TODO: Viewpoint adaption of tp:
      println("info for " + sym.name + ": " + tp)
      //   - using any outer accessors traversed.
      //   - method selections
      /*val ref = sym.denot
      val annotTrees: List[tpd.Tree] = ref.annotations.map(_.tree)
      val annotTrees1 = annotTrees.mapConserve(annotationTransformer.macroTransform)
      val annots1 = if (annotTrees eq annotTrees1) ref.annotations else annotTrees1.map(new ConcreteAnnotation(_))*/
      tp
    }

    /*override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = {
      val tpe = tree.tpe
      //println("Ident " + tree.name + ": " + tpe)
      tree.withTypeUnchecked(tpe)
    }*/

    /*override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
      // TODO: Viewpoint adaption.
      val tpe = tree.tpe.asInstanceOf[TermRef].info // TODO: what if tpe[TermRef] info were accessed here?
      println("Select " + tree.name) // + ": " + tpe)
      tree.withTypeUnchecked(tpe)
      tree
    }*/

    override def transformNew(tree: New)(implicit ctx: Context, info: TransformerInfo): Tree = {
      // Give the new object a @mutable type.
      tree.withTypeUnchecked(tree.tpe.withAnnotation(mutableAnnot))
    }

    override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
      // If the applied method is receiver-polymorphic, then give it a new result type.
      println("Apply " + tree.tpe)
      //TODO
      tree
    }

  }

}
