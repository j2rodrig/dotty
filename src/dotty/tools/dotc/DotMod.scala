package dotty.tools
package dotc

import ast.tpd._
import ast.{tpd, untpd}
import core._
import core.Annotations._
import core.Contexts._
import core.Decorators._
import core.DenotTransformers._
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


  class DotModTyper extends Typer {
    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      def viewpointAdapt(tree: Tree): Tree = {
        if (tree.isInstanceOf[Apply]) {
          // TODO viewpoint adaptation of getter method applications
          tree
        } else tree
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
