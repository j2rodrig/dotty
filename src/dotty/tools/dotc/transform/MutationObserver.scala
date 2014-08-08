package dotty.tools.dotc
package transform

import ast.tpd
import ast.Trees._

import core.Annotations._
import core.Contexts._
import core.Denotations._
import core.Phases._
import core.Symbols._
import core.SymDenotations._
import core.Types._

import collection.mutable.{Set, Map}

/**

The objective of this phase is to find (conservatively) all symbols that may be mutated by each function.
If a function's mutation set is empty, then it is "pure".

The final set of mutations for a function involves removing mutations of local variables, since these
variables are not in scope outside of the function body.

Basic approach:
1. find out where all the denotations are defined, and store by ID
2. whenever a function call occurs, all externally-visible effects are accumulated into the current function's mutation set
3. we will need to build a graph of all functions called by each function. Cycles may be collapsed for greater efficiency.

*/

/*PackageDef(Ident(test),List(ValDef(Modifiers(final module <stable>,,List()),x,TypeTree[TypeRef(ThisType(module class
test),x$)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class test),x$)]),<init>),List())), TypeDef(Modifiers(final
module,,List()),x$,Template(DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class
lang),Object)]),<init>),List())),ValDef(Modifiers(private <selfname>,,List()),_,TypeTree[TermRef(ThisType(module class
test),x)],EmptyTree),List(Apply(Ident(f00),List()),
DefDef(Modifiers(<method>,,List()),f00,List(),List(List()),TypeTree[TypeRef(ThisType(module class
scala),Unit)],Block(List(),Literal(Constant(())))))))))*/


class MutationObserver extends Phase {
	override def name: String = "MutationObserver"
	
	def run (implicit ctx: Context): Unit = {
		(new WithContext).run ()
	}
	
	
	class WithContext (implicit ctx: Context) {
	
		def run () = {
			val unit = ctx.compilationUnit
			val tree = unit.tpdTree
	
			println ("TREE:\n" + tree.toString + "\n\n")
	
			traverse (tree, new FindEnclosed(null))
			//traverse (tree, findOwnersAssigned)
			//traverse (tree, printDenotations)
			//traverse (tree, new FindFunctionApplications (null))
	
		}

		val encloser = Map[Int,Int]()  // symbol id => enclosing symbol id
		val classes = Map[Int,TypeDef[Type]]()   // symbol id => TypeDef node
		
		class FindEnclosed (val enclosing: Tree[Type]) extends (Tree[Type] => Boolean) {
			def apply (tree: Tree[Type]) : Boolean = {
				tree match {
					case p: PackageDef[Type] =>
						p.stats.foreach { t: Tree[Type] => traverse (t, new FindEnclosed (p)) }
						return false
					case t: TypeDef[Type] =>
						if (t.isClassDef) {
							classes += ((t.denot.symbol.id,t))
							encloser += ((t.denot.symbol.id,enclosing.denot.symbol.id))
							traverse (t.rhs, new FindEnclosed(t))
						}
						return false
					case d: DefDef[Type] =>
						encloser += ((d.denot.symbol.id,enclosing.denot.symbol.id))
						d.vparamss.foreach { vs: List[Tree[Type]] => vs.foreach { v: Tree[Type] => traverse (v, new FindEnclosed(d)) } }
						traverse (d.rhs, new FindEnclosed(d))
						return false
					case _ => return true
				// A valid symbol can refer to: local variable, enclosing variable, this, some super.
				}
			}
		}
		
		// TODO: identify parameters
		
		def inEnclosingScope (symbol: Symbol, currentScope: Tree[Type]): Boolean = {
			var encloser_id = currentScope.denot.symbol.id
			while (true) {
				if (symbol.owner.id == encloser_id) {
					return true
				}
				if (!encloser.contains (encloser_id))  // went past the top scope?
					return false;
				encloser_id = encloser(encloser_id)
			}
			return false
		}
		
		def isParameter (symbol: Symbol, currentScope: Tree[Type]): Boolean = {
			return false // TODO: return true if parameter
		}
		
		def isLocalVariable (symbol: Symbol, currentScope: Tree[Type]): Boolean = {
			return symbol.owner.id == currentScope.denot.symbol.id
		}
		
		def printIdentifiers (tree: Tree[Type]) : Boolean = tree match {
			case This(qual) =>
				println ("This")
				return false
			case _ =>
				return true
		}
		
		// NOTE: We're going to need to track:
		// - the enclosing class(es), to determine what denotations are in this/super
		// - the enclosing method(s), to determine what denotations are in outer closures
	
		/*type Readonly = Boolean
	
		trait FunctionInfo () {
			val defdef: DefDef[Type]
			val params: Map[SingleDenotation,Readonly]
			val locals: Map[SingleDenotation,Readonly]
		}*/
		trait OwnershipAssigned {
			def union (other: OwnershipAssigned) : OwnershipAssigned
		}
		
		case class OwnershipAssignedAny() extends OwnershipAssigned {
			def union (other: OwnershipAssigned) : OwnershipAssigned = this
			override def toString () : String = "anyowner"
		}
		case class OwnershipSymbolSet() extends OwnershipAssigned {
			val ids = Set[Int]()
			def addSymbolOwner (id: Int) { ids += id }
			def union (other: OwnershipAssigned) : OwnershipAssigned = other match {
				case o: OwnershipSymbolSet =>
					val r = OwnershipSymbolSet ()
					ids.foreach { id => r.addSymbolOwner (id) }
					o.ids.foreach { id => r.addSymbolOwner (id) }
					r
				case o: OwnershipAssignedAny => o
			}
			override def toString () : String = {
				var s = "("
				ids.foreach { id => s = s + id + "," }
				s = s + ")"
				return s
			}
		}
		
		val aliasMap = Map[Int,OwnershipAssigned]()
		
		def alias (assigned: OwnershipAssigned, rhs: OwnershipAssigned) {
			if (assigned.isInstanceOf[OwnershipSymbolSet]) {
				assigned.asInstanceOf[OwnershipSymbolSet].ids.foreach { id =>
					if (!aliasMap.contains (id))
						aliasMap += ((id, rhs))
					else
						aliasMap += ((id, aliasMap(id).union (rhs)))
				}
			} else
				println ("Can't assign to " + assigned)
		}
		
		def OwnershipOf (denot: Denotation) : OwnershipAssigned = {
			var r = OwnershipSymbolSet ()
			denot.alternatives.foreach { de => de match {
				case s: SymDenotation => r.addSymbolOwner (s.symbol.owner.id)
				case s: SingleDenotation => return OwnershipAssignedAny()
				}
			}
			return r
		}
		
		/* getOwnerAssigned returns the ownership reference of the given tree.
		*/
		def getOwnerAssigned (tree: Tree[Type]) : OwnershipAssigned = tree match {
			case Ident(name) => OwnershipOf (tree.denot)
			case Select(qualifier, name) => getOwnerAssigned (qualifier)
			case New(tpt) => OwnershipSymbolSet()
			case _ => OwnershipAssignedAny()
		}
		

		val parameterSet = Set[Int]()
		def parameterDefinition (ownerTree: Tree[Type], paramTree: Tree[Type]) : Boolean = {
			paramTree match {
				case ValDef(mods,name,tpt,rhs) =>
					parameterSet += paramTree.denot.symbol.id
					return false
				case _ =>
					def visitor (t: Tree[Type]) : Boolean = {
						if (t == paramTree) return true else return parameterDefinition (ownerTree, t)
					}
					traverse (paramTree, visitor)
			}
			return true
		}
		
		def findOwnersAssigned (tree: Tree[Type]) : Boolean = {
			tree match {
				case Assign(lhs,rhs) =>
					val assignedOwner = getOwnerAssigned (lhs)
					println ("var " + assignedOwner +  " = " + getOwnerAssigned (rhs))
					alias (assignedOwner, getOwnerAssigned (rhs))
				case ValDef(mods,name,tpt,rhs) =>
					if (!parameterSet(tree.denot.symbol.id)) {
						val assignedOwner = OwnershipOf (tree.denot)
						println ("val " + name + "," + assignedOwner +  " = " + getOwnerAssigned (rhs))
						alias (assignedOwner, getOwnerAssigned (rhs))
					}
				case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
					vparamss.foreach { vs:List[Tree[Type]] => vs.foreach { v:Tree[Type] => parameterDefinition (tree, v) } }
				case _ =>
			}
			return true
		}
		
		
		//--------------
		
		def printDenotations (tree: Tree[Type]) : Boolean = {
			if (tree.denot.exists) {
				if (tree.denot.alternatives.length > 1) println ("MULTIDENOTATION:")
				tree.denot.alternatives.foreach { de =>
					de match {
						case s: SymDenotation =>
							if (s.privateWithin != NoSymbol)
								println (s.symbol.owner.name + "." + s.symbol.name + ", private within " +
									s.privateWithin.owner.name + "." + s.privateWithin.name)
							else
								println (s.symbol.owner.name + "." + s.symbol.name)
						case s: SingleDenotation =>
							println (s.symbol.owner.name + "." + s.symbol.name + " (SingleDenotation)")
					}
				}
				if (tree.denot.alternatives.length > 1) println ()
			} else {
				println ("\nEmpty denotation for tree: " + tree.toString + "\n")
			}
			return true
		}
	
		/*val functionInfo = Map[SingleDenotation,FunctionInfo]()
	
		trait FunctionInfo (val denot: SingleDenotation) {
		}
	
		class FunctionInfo () {
			val applications = List[SingleDenotation] ()
			val 
			//println ("TESTING: " + tree.symbol.owner.name + "." + tree.symbol.name + " has readonly annotation: " + hasAnnotation(tree.denot, "readonly"))
		}
	
		def getWriteConstraints (tree: Tree[Type]) : Constraint*/
	
		/**
		Maps SingleDenotations to function information objects.
		Assumes that distinct SingleDenotation objects always represent distinct defintions - 
		i.e., that given two SingleDenotations d and e, d==e if and only if they refer to the same definition.
		*/
		//val mapDenotationToFunction = Map[SingleDenotation,FunctionInfo]()
	
		
		/* // TODO: find all application denotations, not just the ones defined in this file.
	
		def registerDefDefDenotations (tree: Tree[Type]) : Boolean = {
			tree match {
				case d: DefDef[Type] =>
					val fi = new FunctionInfo (d)
					d.denot.alternatives.foreach { de =>
						if (mapDenotationToFunction.contains (de))
							println ("WARNING: Was not expecting multiple DefDef trees to have the same Denotation!")
						//println ("Symbol ID: " + de.symbol.id)
						mapDenotationToFunction += ((de, fi))
					}
					if (!d.denot.exists)
						println ("WARNING: Was not expecting a DefDef tree without a valid Denotation!")
				case _ =>
			}
			return true
		}*/
		
		/*class MutationType (val readonly: Boolean) {
			def merge (EffectType other) = new MutationType (readonly || other.readonly)
		}
		
		object MutationType {
		
			def fromDenotation (denot: Denotation): MutationType = {
				var readonly = false
				// If any alternative is readonly, then the mutation type is readonly
				denot.alternatives.foreach { s => if (hasAnnotation (s, "readonly")) readonly = true }
				return new MutationType (readonly)
			}
			
			def fromExpression (val tree: Tree[Type]) : MutationType = {
				tree match {
					case Ident(name) =>
						MutationType.fromDenotation(tree.denot)
					case Select(qualifier, name) =>
						MutationType.fromExpression(qualifier).union(MutationType.fromDenotation(tree.denot))
					case This(qual) =>
						MutationType.fromExpression(qualifier).union(MutationType.fromDenotation(tree.denot))
					case Super(qual, mix) =>
						traverse (qual, visitor)
					case Apply(fun, args) =>
						traverse (fun, visitor)
						args.foreach { x:Tree[Type] => traverse (x, visitor) }
					case TypeApply(fun, args) =>
						traverse (fun, visitor)
						args.foreach { x:Tree[Type] => traverse (x, visitor) }
					case Literal(const) =>
					case New(tpt) =>
						traverse (tpt, visitor)
					case Pair(left, right) =>
						traverse (left, visitor)
						traverse (right, visitor)
					case Typed(expr, tpt) =>
						traverse (expr, visitor)
						traverse (tpt, visitor)
					case NamedArg(name, arg) =>
						traverse (arg, visitor)
					case Assign(lhs, rhs) =>
						traverse (lhs, visitor)
						traverse (rhs, visitor)
					case Block(stats, expr) =>
						stats.foreach { x:Tree[Type] => traverse (x, visitor) }
						traverse (expr, visitor)
					case If(cond, thenp, elsep) =>
						traverse (cond, visitor)
						traverse (thenp, visitor)
						traverse (elsep, visitor)
					case Closure(env, meth, tpt) =>
						env.foreach { x:Tree[Type] => traverse (x, visitor) }
						traverse (meth, visitor)
						traverse (tpt, visitor)
					case Match(selector, cases) =>
						traverse (selector, visitor)
						cases.foreach { x:Tree[Type] => traverse (x, visitor) }
					case CaseDef(pat, guard, body) =>
						traverse (pat, visitor)
						traverse (guard, visitor)
						traverse (body, visitor)
					case Return(expr, from) =>
						traverse (expr, visitor)
						traverse (from, visitor)
					case Try(block, handler, finalizer) =>
						traverse (block, visitor)
						traverse (handler, visitor)
						traverse (finalizer, visitor)
					case Throw(expr) =>
						traverse (expr, visitor)
					case SeqLiteral(elems) =>
						elems.foreach { x:Tree[Type] => traverse (x, visitor) }
					case TypeTree(original) =>
					case SingletonTypeTree(ref) =>
						traverse (ref, visitor)
					case SelectFromTypeTree(qualifier, name) =>
						traverse (qualifier, visitor)
					case AndTypeTree(left, right) =>
						traverse (left, visitor)
						traverse (right, visitor)
					case OrTypeTree(left, right) =>
						traverse (left, visitor)
						traverse (right, visitor)
					case RefinedTypeTree(tpt, refinements) =>
						traverse (tpt, visitor)
						refinements.foreach { x:Tree[Type] => traverse (x, visitor) }
					case AppliedTypeTree(tpt, args) =>
						traverse (tpt, visitor)
						args.foreach { x:Tree[Type] => traverse (x, visitor) }
					case ByNameTypeTree(result) =>
						traverse (result, visitor)
					case TypeBoundsTree(lo, hi) =>
						traverse (lo, visitor)
						traverse (hi, visitor)
					case Bind(name, body) =>
						traverse (body, visitor)
					case Alternative(trees) =>
						trees.foreach { x:Tree[Type] => traverse (x, visitor) }
					case UnApply(fun, implicits, patterns) =>
						traverse (fun, visitor)
						implicits.foreach { x:Tree[Type] => traverse (x, visitor) }
						patterns.foreach { x:Tree[Type] => traverse (x, visitor) }
					case ValDef(mods, name, tpt, rhs) =>
						traverse (tpt, visitor)
						traverse (rhs, visitor)
					case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
						tparams.foreach { x:Tree[Type] => traverse (x, visitor) }
						vparamss.foreach { v:List[Tree[Type]] => v.foreach { x:Tree[Type] => traverse (x, visitor) } }
						traverse (tpt, visitor)
						traverse (rhs, visitor)
					case TypeDef(mods, name, rhs) =>
						traverse (rhs, visitor)
					case Template(constr, parents, self, body) =>
						traverse (constr, visitor)
						parents.foreach { x:Tree[Type] => traverse (x, visitor) }
						traverse (self, visitor)
						body.foreach { x:Tree[Type] => traverse (x, visitor) }
					case Import(expr, selectors) =>
						traverse (expr, visitor)
					case PackageDef(pid, stats) =>
						traverse (pid, visitor)
						stats.foreach { x:Tree[Type] => traverse (x, visitor) }
					case Annotated(annot, arg) =>
						traverse (annot, visitor)
						traverse (arg, visitor)
					case Thicket(ts) =>
						ts.foreach { x:Tree[Type] => traverse (x, visitor) }
					case _ =>
						println ("ERROR: Unexpected node type in expression evaluation.\nTree: " + tree.toString + "\n")
						return EffectType (false)
				}
			}
		}*/
	
		class FindFunctionApplications (val enclosing: Tree[Type]) extends (Tree[Type] => Boolean) {
			def apply (tree: Tree[Type]) : Boolean = {
				/* A PackageDef or Template or DefDef can enclose function applications.
				If a PackageDef or Template or DefDef is encountered, a new FindFunctionApplications is created
				for subsequent traversals. false is returned to stop the default traversal in these cases.
				*/
				tree match {
				
					/* PackageDef may contain executable statements (stats). */
					case PackageDef(pid, stats) =>
						val visitor = new FindFunctionApplications (tree)
						traverse (pid, visitor)
						stats.foreach { t: Tree[Type] => traverse (t, visitor) }
						return false
						
					/* Template is a class/trait definition. constr, parents, and body may all contain
					applications, presumably all of which are called during object initialization.
					*/
					case Template(constr, parents, self, body) =>
						val visitor = new FindFunctionApplications (tree)
						traverse (constr, visitor)
						parents.foreach { t: Tree[Type] => traverse (t, visitor) }
						traverse (self, visitor)
						body.foreach { t: Tree[Type] => traverse (t, visitor) }
						return false
					
					/* DefDef is a function definition. */
					case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
						val visitor = new FindFunctionApplications (tree)
						tparams.foreach { t: Tree[Type] => traverse (t, visitor) }
						vparamss.foreach { v: List[Tree[Type]] => v.foreach { t: Tree[Type] => traverse (t, visitor) } }
						traverse (tpt, visitor)
						traverse (rhs, visitor)
						return false
					
					case Ident (name) => println ("Ident " + name + " = " + getOwnerAssigned (tree))
					
					/* Apply is a function call.
					 denotation (or possible meaning - overloaded names may have multiple denotations),
					take a union of
					*/
					case a: Apply[Type] =>
						a.denot.alternatives.foreach { de =>
							//println (enclosing.symbol.owner.name + "." + enclosing.symbol.name +
							//	" calls " + de.symbol.owner.name + "." + de.symbol.name +
							//	" (readonly = " + hasAnnotation(de, "readonly") + ") ")
						}
					
					/* */
					case Select(qualifier, name) => println ("Select " + name + " = " + getOwnerAssigned (tree))
					
					// No action needed for other node types.
					case _ =>
				}
				return true
			}
		}
	
		/// Returns true if the given denotation has an annotation of the given name
		def hasAnnotation (d: SingleDenotation, a: String) : Boolean = {
			d match {
				case sd: SymDenotation =>
					sd.annotations.foreach { ann: Annotation =>
						if (ann.symbol.name.toString == a) return true
					}
				case s: SingleDenotation =>
			}
			return false
		}
	
		/**
		Traverses the given tree, calling visitor(tree) on every subtree.
		If visitor returns false, traverse() will not traverse any subtrees.
		If visitor returns true, automatic traversal of subtrees will continue.
	
		The reason for using a custom tree traversal function is so that we have more control
		over how the traversal proceeds.
		If an unexpected node type is encountered, we expect a MatchError to occur.
		A MatchError gives us an opportunity to consider whether anything in the plugin needs to change,
		given that a new node type exists.
		 */
 		def traverse (tree: Tree[Type], visitor: (Tree[Type] => Boolean)) : Unit = {
			val continueTraversing = visitor (tree)
			if (continueTraversing) tree match {
				case Ident(name) =>
				case Select(qualifier, name) =>
					traverse (qualifier, visitor)
				case This(qual) =>
				case Super(qual, mix) =>
					traverse (qual, visitor)
				case Apply(fun, args) =>
					traverse (fun, visitor)
					args.foreach { x:Tree[Type] => traverse (x, visitor) }
				case TypeApply(fun, args) =>
					traverse (fun, visitor)
					args.foreach { x:Tree[Type] => traverse (x, visitor) }
				case Literal(const) =>
				case New(tpt) =>
					traverse (tpt, visitor)
				case Pair(left, right) =>
					traverse (left, visitor)
					traverse (right, visitor)
				case Typed(expr, tpt) =>
					traverse (expr, visitor)
					traverse (tpt, visitor)
				case NamedArg(name, arg) =>
					traverse (arg, visitor)
				case Assign(lhs, rhs) =>
					traverse (lhs, visitor)
					traverse (rhs, visitor)
				case Block(stats, expr) =>
					stats.foreach { x:Tree[Type] => traverse (x, visitor) }
					traverse (expr, visitor)
				case If(cond, thenp, elsep) =>
					traverse (cond, visitor)
					traverse (thenp, visitor)
					traverse (elsep, visitor)
				case Closure(env, meth, tpt) =>
					env.foreach { x:Tree[Type] => traverse (x, visitor) }
					traverse (meth, visitor)
					traverse (tpt, visitor)
				case Match(selector, cases) =>
					traverse (selector, visitor)
					cases.foreach { x:Tree[Type] => traverse (x, visitor) }
				case CaseDef(pat, guard, body) =>
					traverse (pat, visitor)
					traverse (guard, visitor)
					traverse (body, visitor)
				case Return(expr, from) =>
					traverse (expr, visitor)
					traverse (from, visitor)
				case Try(block, handler, finalizer) =>
					traverse (block, visitor)
					traverse (handler, visitor)
					traverse (finalizer, visitor)
				case Throw(expr) =>
					traverse (expr, visitor)
				case SeqLiteral(elems) =>
					elems.foreach { x:Tree[Type] => traverse (x, visitor) }
				case TypeTree(original) =>
				case SingletonTypeTree(ref) =>
					traverse (ref, visitor)
				case SelectFromTypeTree(qualifier, name) =>
					traverse (qualifier, visitor)
				case AndTypeTree(left, right) =>
					traverse (left, visitor)
					traverse (right, visitor)
				case OrTypeTree(left, right) =>
					traverse (left, visitor)
					traverse (right, visitor)
				case RefinedTypeTree(tpt, refinements) =>
					traverse (tpt, visitor)
					refinements.foreach { x:Tree[Type] => traverse (x, visitor) }
				case AppliedTypeTree(tpt, args) =>
					traverse (tpt, visitor)
					args.foreach { x:Tree[Type] => traverse (x, visitor) }
				case ByNameTypeTree(result) =>
					traverse (result, visitor)
				case TypeBoundsTree(lo, hi) =>
					traverse (lo, visitor)
					traverse (hi, visitor)
				case Bind(name, body) =>
					traverse (body, visitor)
				case Alternative(trees) =>
					trees.foreach { x:Tree[Type] => traverse (x, visitor) }
				case UnApply(fun, implicits, patterns) =>
					traverse (fun, visitor)
					implicits.foreach { x:Tree[Type] => traverse (x, visitor) }
					patterns.foreach { x:Tree[Type] => traverse (x, visitor) }
				case ValDef(mods, name, tpt, rhs) =>
					traverse (tpt, visitor)
					traverse (rhs, visitor)
				case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
					tparams.foreach { x:Tree[Type] => traverse (x, visitor) }
					vparamss.foreach { v:List[Tree[Type]] => v.foreach { x:Tree[Type] => traverse (x, visitor) } }
					traverse (tpt, visitor)
					traverse (rhs, visitor)
				case TypeDef(mods, name, rhs) =>
					traverse (rhs, visitor)
				case Template(constr, parents, self, body) =>
					traverse (constr, visitor)
					parents.foreach { x:Tree[Type] => traverse (x, visitor) }
					traverse (self, visitor)
					body.foreach { x:Tree[Type] => traverse (x, visitor) }
				case Import(expr, selectors) =>
					traverse (expr, visitor)
				case PackageDef(pid, stats) =>
					traverse (pid, visitor)
					stats.foreach { x:Tree[Type] => traverse (x, visitor) }
				case Annotated(annot, arg) =>
					traverse (annot, visitor)
					traverse (arg, visitor)
				case Thicket(ts) =>
					ts.foreach { x:Tree[Type] => traverse (x, visitor) }
			}
		}
	}
}
