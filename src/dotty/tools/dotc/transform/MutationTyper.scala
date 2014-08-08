package dotty.tools.dotc
package transform

import ast.{tpd, untpd}
import ast.Trees._

import core.Annotations._
import core.Constants._
import core.Contexts._
import core.Decorators._
import core.Denotations._
import core.DenotTransformers._
import core.Names._
import core.NameOps._
import core.Phases._
import core.StdNames._
import core.Symbols._
import core.SymDenotations._
import core.Types._

import typer.Checking
import typer.NoChecking


class MutationTyper extends Phase {

	override def name = "mutation typer"

	val typerInstance = new TyperClass

	def run(implicit ctx: Context): Unit = {
		val unit = ctx.compilationUnit
		unit.tpdTree = typerInstance.typedExpr(unit.tpdTree)(ctx.fresh.setPhase(this.next))
	}
	
	class TyperClass extends typer.ReTyper {
		override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
			//println ("DefDef: " + ddef.symbol.owner.name + "." + ddef.symbol.name)
			super.typedDefDef(ddef, sym)
		}
	}
}
