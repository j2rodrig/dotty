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
import core.Phases._
import core.Symbols._
import core.Types._
import transform.TreeTransforms._
import typer.Typer

// Philosophical approach:
//  - Add @readonly to every type that I don't want to be mutable.
//
// Perhaps symbol info transformation can handle the adaptation of the symbol wrt. outer references.
//
// Viewpoint adapation of field reads can be done through the type comparer.
//
// Applications are a sticky issue right now.
// Can viewpoint adaptation on applications be dealt with through typedApply or similar interface?
// (Note: remember that getters/setters can also have JavaBeans naming, e.g.: def getX() for field x.)
//
// A RefChecks phase can check constructibility of parent classes.
// And overriding, etc.

object DotMod {

  def canAnnotateType(annot: Annotation)(implicit ctx: Context) = {
    (annot.symbol == defn.MutableAnnotType
      || annot.symbol == defn.ReadonlyAnnotType
      || annot.symbol == defn.PolyReadAnnotType)
  }

  def canAnnotateMethod(annot: Annotation)(implicit ctx: Context) = {
    (annot.symbol == defn.MutableAnnotType
      || annot.symbol == defn.ReadonlyAnnotType
      || annot.symbol == defn.PolyReadAnnotType)
  }

  def canAnnotateClass(annot: Annotation)(implicit ctx: Context) = {
    (annot.symbol == defn.MutableAnnotType
      || annot.symbol == defn.ReadonlyAnnotType)
  }


  //def getAnnotatedMutabilityBound(mtp: Type): ClassSymbol


  class DotModTyper extends Typer {

    def isGetter(sym: Symbol)(implicit ctx: Context): Boolean = {
      // A symbol refers to a getter method if:
      //  - the symbol has no parameters (ExprType)
      //  - or the symbol has one empty parameter list and the name starts with "get" + S where S does not start with a lower-case letter
      //  - or the symbol is annotated with @polyread
      if (!(sym is Method)) false
      else if (sym.annotations.exists(_.matches(defn.PolyReadAnnotType))) true
      else sym.info match {
        case _: ExprType => true
        case tpe: MethodType =>
          tpe.paramNames.isEmpty && sym.name.startsWith("get") && (sym.name.length == 3 || !sym.name.toString.charAt(3).isLower)
        // TODO: PolyType?
      }
    }

    //def intersectWithMutability(tp: Type, mtp: Type): Type = {

    //}

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      def viewpointAdapt(tree: Tree): Tree = {
        if (!tree.hasType || tree.tpe.isError || !tree.isInstanceOf[Select] || !isGetter(tree.symbol))
          tree
        else {
          println(tree + " => " + fun(tree).tpe)

          def fun(tree: Tree): Tree = tree match {
            case _: Ident | _: Select => tree
            case tree: Apply => fun(tree.fun)
            case tree: TypeApply => fun(tree.fun)
            case tree: Typed => fun(tree.expr)
          }

          val functionType = fun(tree).tpe
          if (functionType.isError) tree
          else {
            val receiverType = functionType.stripAnnots.asInstanceOf[TermRef].prefix
            if (receiverType.isError) tree
            else {
              //println("       (apply getter)  " + receiverType + " . " + tree.symbol)
              tree
            }
          }
        }
      }

      val tree = viewpointAdapt(super.adapt(tree0, pt, original))

      val dontCheck = (
        pt == WildcardType
          || !tree.tpe.exists
          || pt.isInstanceOf[ProtoType]
          || tree.tpe <:< defn.AnyValType
        )

      if(dontCheck) tree else {
        // TODO check that tree.tpe <:< pt wrt. mutability
        tree
      }
    }
  }

  class DotModInfoPhase extends MiniPhaseTransform with InfoTransformer {
    override def phaseName = "dotmodinfo"

    override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
      // Do we really need anything in here?
      tp
    }
  }
}
