package dotty.tools
package dotc
package core

import util.common._
import ast._
import tpd._
import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import Scopes._
import Constants._
import Contexts._
import Annotations._
import SymDenotations._
import Decorators._
import Denotations._
import Periods._
import Types._

object Mutability {

// Type System 2:
// - All types start out as Unadapted.
// - When a concrete reference is created, Unadapted types drop to Mutable.
// - Annotations can restrict Unadapted to Mutable, Readonly, or Polyread.

/*
The top type (unadapted) is the most constrained.
The bottom type (mutable) is the least constrained.
Polyread <: readonly <- readonly <: readonly AND mutable <: readonly.
Mutable <: polyread because mutable <: mutable AND mutable <: readonly.

Bounds adjustment rules:

Type bounds always have a single mutability type.
Ground types are: Mutable <: Readonly <: Unannotated.

type T
type U: @mutable
type V: @polyread
type W: @readonly

Equality:

type T = X @readonly
type U = T @mutable   // OK: @mutable can override @readonly at the type level
var u: U = new T   // should fail: X @readonly is not compatible with X @mutable

Variables of type T are mutable.

Polytypes are: Polyread (Mutable | Readonly), 



Preliminary summary 1:

* Any mutability type annotation overrides any underlying annotation.
* The mutability of any type is its underlying mutability.
* If no mutability annotation is available, the mutability is assumed "mutable".

AND TYPES:
* The mutability is the GLB of the underlying types.

OR TYPES:
* The mutability is the LUB of the underlying types.

BOUNDED TYPES:
* ...

*/

/**
Approach 5 to viewpoint adaptation of function/method calls:

Principle 1: The result type of a method depends on the types of the applied arguments.

Fact 1: In the case of partial application (closure), some arguments may have been applied
prior to the application node under current consideration.

Therefore: The type of a function includes the effects of previous partial applications.
This extra information means that there are many function types where the Scala
compiler only recognizes one type...

Solution alternative 1: Each apply site is aware of previous apply sites.
	Problem: Having first-class functions means that it is not easily known
	which apply sites were previous. However, an approximation of this
	information is encoded into the type, alleviating this problem.

Solution alternative 2: Extend the function class to include information
	about previously-applied arguments. Odersky may not like this, but
	I don't see an easier way forward right now. ... (do this!)

Solution alternative 3: 1) Create mutability type for methods.
	2) Add an "as-viewed-from" parameter.
	3) Include "as-viewed-from" on the closure.
	4) ...

*/


// TODO: use stripTypeVar?

	trait Mutability {
	}

	trait SimpleMutability extends Mutability {
		def exists: Boolean
		def isAnnotated: Boolean
		def isPolyread: Boolean = false
		def canBeMutable: Boolean = false
		def canBeReadonly: Boolean = false
	}
	case class readonly() extends SimpleMutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def canBeReadonly: Boolean = true
		override def toString = "@readonly"
	}
	case class polyread() extends SimpleMutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def isPolyread: Boolean = true
		override def canBeMutable: Boolean = true
		override def canBeReadonly: Boolean = true
		override def toString = "@polyread"
	}
	case class minpolyread() extends SimpleMutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def isPolyread: Boolean = true
		override def canBeMutable: Boolean = false   //true   // this is a lie, but some functions need it
		override def canBeReadonly: Boolean = true
		override def toString = "@minpolyread"
	}
	case class mutable() extends SimpleMutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def canBeMutable: Boolean = true
		override def toString = "@mutable"
	}
	/** The mutability of a type where no mutability has been specified. */
	case class unannotated() extends SimpleMutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = false
		override def toString = "(unannotated)"
	}
	/** The mutability resulting from a failed search. */
	case class notfound() extends SimpleMutability {
		override def exists: Boolean = false
		override def isAnnotated: Boolean = false
		override def toString = "(notfound)"
	}
	/** The mutability of an ErrorType. */
	case class errortype() extends SimpleMutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = false
		override def toString = "(errortype)"
	}
	
	/** Represents a function object or closure. */
	/*class FunctionMutability(paramTypes: List[Type], returnMut: SimpleMutability) extends Mutability {
		def apply(argType: Type)(implicit ctx: Context): Mutability = {
			val newRet = viewpointAdaptPartial(getSimpleMutability(paramTypes.head), returnMut)
			if (paramTypes.tail.isEmpty) viewpointAdaptFinal(newRet)
			else new FunctionMutability(paramTypes.tail, newRet)
		}
	}*/
	//case class NotAFunction() extends FunctionMutability(List(), errortype()) {}
	
	/** Viewpoint adapation for simple field selection. */
	def viewpointAdapt(view: SimpleMutability, underlying: SimpleMutability): SimpleMutability = underlying match {
		case minpolyread() => if (view.isInstanceOf[unannotated]) underlying else view
		case polyread()    => if (view.isInstanceOf[unannotated]) underlying else view
		case _             => lub(view, underlying)
	}
/*	def viewpointAdapt(view: SimpleMutability, underlying: SimpleMutability): SimpleMutability = underlying match {
		case readonly()    => underlying
		case minpolyread() => if (view.isInstanceOf[unannotated]) underlying else view
		case polyread()    => if (view.isInstanceOf[unannotated]) underlying else view
		case mutable()     => if (view.isInstanceOf[unannotated]) underlying else view
		case unannotated() => view
		case errortype()   => underlying
	}*/
	
	/** Viewpoint adapation for simple field selection. */
	def viewpointAdapt(qual: Type, actual: Type)(implicit ctx: Context): Type = {
		val qualMut = getSimpleMutability(qual)
		val actualMut = getSimpleMutability(actual)
		withSimpleMutability(actual, viewpointAdapt(qualMut, actualMut))
	}
	
	/** Partial viewpoint adapation of return types, for arguments to polyread parameters. */
	/*def viewpointAdaptPartial(arg: SimpleMutability, ret: SimpleMutability): SimpleMutability = ret match {
		case readonly()    => readonly()
		case mutable()     => mutable()
		case unannotated() => polyread()
		case polyread()    => arg match {
				case readonly()    => readonly()
				case polyread()    => minpolyread()
				case mutable()     => polyread()
				case unannotated() => polyread()
				case errortype()   => errortype()
			}
		case minpolyread() => arg match {
				case readonly()    => readonly()
				case polyread()    => minpolyread()
				case mutable()     => minpolyread()
				case unannotated() => minpolyread()
				case errortype()   => errortype()
			}
		case errortype()   => errortype()
	}*/
	
	/** Final viewpoint adapation of return types, after all arguments have been applied. */
	/*def viewpointAdaptFinal(ret: SimpleMutability): SimpleMutability = ret match {
		case readonly()    => readonly()
		case minpolyread() => polyread()
		case polyread()    => mutable()
		case mutable()     => mutable()
		case unannotated() => unannotated()
		case errortype()   => errortype()
	}*/
	
	def tmtMismatch(tree: Tree, pt: Type)(implicit ctx: Context): Tree = {
		typer.ErrorReporting.errorTree(tree, tmtMismatchStr(tree.tpe, pt))
	}

	def tmtMismatchStr(found: Type, expected: Type)(implicit ctx: Context) = {
		s"mutability mismatch:\n"+
		s" found   : ${found.show}\n"+
		s" required: ${expected.show}"
		//s" found   : ${getSimpleMutability(found)}\n"+
		//s" required: ${getSimpleMutability(expected)}"
	}
	
	case class Adaptations(polyresult: SimpleMutability) {
	
	}
	
	case class MutabilityContext(tree: Tree) {}
	
	/** Applies a function of the given type t with arguments.
	Type t can be a MethodType, a FunctionClass, or an alias thereof.
	Returns the result type, adapted.
	*/
	def applyFunction(t: Type)(implicit ctx: Context): Type = {
		// 1. Find mutabilities of enclosing variables
		// 2. If t is a method type, wrap final result type in modifier
		// 2b. If result type is a FunctionClass, 
		// 3. Check args to polyread params
		//var 
		t  // temp
	}
	
	/*t.dealias match {
		case t @ MethodType(paramNames, paramTypes) =>
		case t: Refined
	}*/
	
	def checkEnclosingVarMutabilities(applyNode: Tree) = {}


	/** If t is a method type, annotates the result with the mutability tmt. */
	/** If t is a ground type, returns that type with the annotation applied. */
	/** If t is an annotated type, skips the annotation if it is a mutability annotation. */
	/*def copyTypeWithResult(t: Type, tmt: SimpleMutability): Type = t.dealias match {
		case t @ MethodType(paramNames, paramTypes) =>
			t.derivedMethodType(paramNames, paramTypes, copyTypeWithResult(mt.resultType, tmt))
			
		//case t: NamedType =>
		//	t.newLikeThis(t.prefix) withDenot copyTypeWithResult(t.d)
			
		case t: AnnotatedType =>
			copyTypeWithResult(
				(if (getAnnotationMutability(t).isAnnotated) t.underlying else t),
				tmt
			)
		case _ =>
			new AnnotatedType(toAnnotation(tmt), t)
	}*/
	
	//def skipAliasesAndAnnotations(t: Type): Type = t match {
	//}
	
	/** Gets the mutability of the given type, provided the type is a MethodType or Function class. */
	/*def getFunctionMutability(t: Type)(implicit ctx: Context): FunctionMutability = t.dealias match {
		case t @ MethodType(paramNames, paramTypes) =>
			val resultFn = getFunctionMutability(t.resultType)
			if (resultFn.isInstanceOf[NotAFunction])
				new FunctionMutability(paramTypes, getSimpleMutability(t.resultType))
			else
				new FunctionMutability(paramTypes ::: resultFn.paramTypes, resultFn.returnMut)
		case _ => NotAFunction()
	}*/

	
	//case class MethodMutability(paramMut: List[Mutability], resultMut: Mutability) extends Mutability {}

	/** Finds a mutability that bounds both m1 and m2.
	If either mutability can be adapted to @mutable, then the result can be adapted to @mutable.
	If either mutability can be adapted to @readonly, then the result can be adapted to @readonly.
	E.g., for types P and Q, the bounds expression "<:P @mutable >:Q @readonly" yields @polyread.
	The expression "<:P >:Q @polyread" is equivalent.
	*/
	def narrowestBound(m1: SimpleMutability, m2: SimpleMutability): SimpleMutability = {
		if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
		else m2 match {
			case readonly() => if (m1.canBeMutable) polyread() else if (m1.isPolyread) minpolyread() else readonly()
			case minpolyread() => if (m1.canBeMutable) polyread() else minpolyread()
			case polyread() => polyread()
			case _ => m2   // anything else - just return the upper bound
		}
/*		val mut = (m1.canBeMutable || m2.canBeMutable)
		val poly = (m1.isPolyread || m2.isPolyread)
		val ro = (m1.canBeReadonly || m2.canBeReadonly)
		if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
		else if (mut && ro) polyread()
		else if (mut) mutable()
		else if (ro) readonly()
		else unannotated()*/
	}

	/// The greatest lower bound / intersection of two mutabilities.
	def glb(m1: SimpleMutability, m2: SimpleMutability): SimpleMutability = {
		if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
		else if (!m1.isAnnotated || !m2.isAnnotated) unannotated()
		else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
		else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
		else if (m1.isInstanceOf[minpolyread] || m2.isInstanceOf[minpolyread]) minpolyread()
		else readonly()
	}
	def lower = glb _   // alias

	/// The least upper bound / union of two mutabilities.
	def lub(m1: SimpleMutability, m2: SimpleMutability): SimpleMutability = {
		if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
		else if (m1.isInstanceOf[readonly] || m2.isInstanceOf[readonly]) readonly()
		else if (m1.isInstanceOf[minpolyread] || m2.isInstanceOf[minpolyread]) minpolyread()
		else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
		else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
		else unannotated()
	}
	def upper = lub _   // alias

	def simpleSubtype(m1: SimpleMutability, m2: SimpleMutability): Boolean =
		m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype] ||   // don't generate extra errors for error types
			(m2 match {
				//case m2: MethodMutability => m1.isInstanceOf
				case m2: readonly => true
				case m2: minpolyread => !m1.isInstanceOf[readonly]
				case m2: polyread => !m1.isInstanceOf[readonly] && !m1.isInstanceOf[minpolyread]
				case _ => !m1.isInstanceOf[readonly] && !m1.isInstanceOf[minpolyread] && !m1.isInstanceOf[polyread]
			})
	
	def getAnnotationMutability(annot: Annotation)(implicit ctx: Context): SimpleMutability =
		annot.symbol.name.toString match {
			case "mutable" => mutable()
			case "polyread" => polyread()
			case "minpolyread" => minpolyread()
			case "readonly" => readonly()
			case _ => unannotated()
		}
	
	def toAnnotation(mut: SimpleMutability)(implicit ctx: Context): Annotation = {
		//Ident(tpnme.readonly)
	val ann = mut match {
		/*case mutable() => ConcreteAnnotation(New(ctx.definitions.MutableClass.typeRef) withType ctx.definitions.MutableClass.typeRef)
		case polyread() => ConcreteAnnotation(New(ctx.definitions.PolyreadClass.typeRef) withType ctx.definitions.PolyreadClass.typeRef)
		case minpolyread() => ConcreteAnnotation(New(ctx.definitions.MinpolyreadClass.typeRef) withType ctx.definitions.MinpolyreadClass.typeRef)
		case readonly() => ConcreteAnnotation(New(ctx.definitions.ReadonlyClass.typeRef) withType ctx.definitions.ReadonlyClass.typeRef)
		*/
		case mutable() => Annotation(ctx.definitions.MutableClass, List())
		case polyread() => Annotation(ctx.definitions.PolyreadClass, List())
		case minpolyread() => Annotation(ctx.definitions.MinpolyreadClass, List())
		case readonly() => Annotation(ctx.definitions.ReadonlyClass, List())
	}
	//println (s"ANNOTATION symbol for $mut = ${ann.symbol.name.toString}")
	ann
	}
	
	
	/** If a top-level automatically-added type annotation is incompatible with
	 *  the underlying mutability type, then return it.
	 *  Returns a pair: (found_annotation, expected_mutability)
	 */
	def incompatibleAutoMutabilityTypes(t: Type)(implicit ctx: Context): Option[(Annotation,SimpleMutability)] = t match {
		case t @ AnnotatedType(annot, tpe) if (t.isInstanceOf[AutoType]) =>
			// If t is an automatic mutability type annotation, then it must be a supertype of the underlying type.
			val found = getAnnotationMutability(annot)
			val expected = getSimpleMutability(t.underlying)
			if (found.isAnnotated && !simpleSubtype(expected, found)) Some((annot, expected))
			else incompatibleAutoMutabilityTypes(t.underlying)
		case t: MethodType => incompatibleAutoMutabilityTypes(t.resultType)  // annotations on methods go on the result type
		case t: TypeProxy => incompatibleAutoMutabilityTypes(t.underlying)
		case _ => None
	}
	
	/// Convenience method for adding a list of mutability annotations.
	def addAnnotations(t: Type, annots: List[Annotation])(implicit ctx: Context): Type = {
		var newT = t
		annots.foreach { annot =>
			if (getAnnotationMutability(annot).isAnnotated)  // skip non-mutability annotations
				newT = addAnnotation(newT, annot)
		}
		newT
	}
	
	/** Returns t without top-level mutability annotations. */
	def stripMutabilityAnnots(t: Type)(implicit ctx: Context): Type = t match {
		// TODO: this method stops as soon as a non-mutability annotation is found.
		case AnnotatedType(ann, underlying) if (getAnnotationMutability(ann).isAnnotated) =>
			stripMutabilityAnnots(underlying)
		case _ => t
	}
	
	/** Returns the given type with the given mutability applied.
	Do not use this method for MethodTypes/PolyTypes/ExprTypes! (Modify the result types/modifiers instead.)
	*/
	def withMutability(t: Type, mut: SimpleMutability)(implicit ctx: Context): Type =
		if (mut.isAnnotated)
			new AnnotatedType(toAnnotation(mut), stripMutabilityAnnots(t))
		else {
			// No annotation supplied - try to return an unannotated result.
			// If stripping annotations does not remove t's mutability, then apply a @mutable annotation.
			val stripped = stripMutabilityAnnots(t)
			if (getSimpleMutability(stripped).isAnnotated) new AnnotatedType(toAnnotation(mutable()), stripped)
			else stripped
		}
	
	/** Returns type t wrapped in the given annotations.
	If t is a method type, returns a copy of t with the annotation applied to the result type.
	*/
	def addAnnotation(t: Type, annot: Annotation)(implicit ctx: Context): Type = t match {
		case mt @ MethodType(paramNames, paramTypes) =>
			mt.derivedMethodType(paramNames, paramTypes, addAnnotation(mt.resultType, annot))
			/*val newResultType = addAnnotation(mt.resultType, annot)
			if (newResultType != mt.resultType) {  // if result type has changed, make a copy of the method type
				if (mt.isJava) JavaMethodType(paramNames, paramTypes)(_ => newResultType)
				if (mt.isImplicit) ImplicitMethodType(paramNames, paramTypes)(_ => newResultType)
				else MethodType(paramNames, paramTypes)(_ => newResultType)
			}
			else mt  // no copy needed*/
		case _ =>
			if (getAnnotationMutability(annot).isAnnotated)   // only strip annotations if annot will override them
				new AnnotatedType(annot, stripMutabilityAnnots(t)) //with AutoType
			else
				new AnnotatedType(annot, t) //with AutoType
	}
	def addAnnotation(t: Type, mut: SimpleMutability)(implicit ctx: Context): Type = {
		addAnnotation(t, toAnnotation(mut))
	}
	
	/** Returns true if the given mutability can override the given annotation in a type expression.
	I.e., where the given mutability is applied, can the given annotation be ignored
	without changing the meaning of the program?
	The rule implemented here is: If both mut and annot have specified mutabilities,
	    then annot can be ignored (mut can override annot).
	*/
	def overridesAnnotation(mut: SimpleMutability, annot: Annotation)(implicit ctx: Context): Boolean =
		mut.isAnnotated && getAnnotationMutability(annot).isAnnotated
	
	def overridesAnnotation(annot1: Annotation, annot2: Annotation)(implicit ctx: Context): Boolean =
		overridesAnnotation(getAnnotationMutability(annot1), annot2)

	/** Finds the top-level (simple) mutability of a given type.
	Returns one of: mutable, polyread, readonly, unannotated, errortype.
	*/
	def getSimpleMutability(tp: Type)(implicit ctx: Context): SimpleMutability = tp match {
		case tp @ AnnotatedType(annot, tpe) => annot.symbol.name.toString match {
			case "mutable" => mutable()
			case "polyread" => polyread()
			case "minpolyread" => minpolyread()
			case "readonly" => readonly()
			case _ => getSimpleMutability(tpe)  // unrecognized annotation: check underlying type
		}
		/*case tp: NamedType => {
			var mut: SimpleMutability = unannotated()  // include mutability annotations on symbols
			tp.denot.alternatives.foreach { d => d match {
				case d: SymDenotation =>
					d.annotations.foreach { annot => mut = lub(mut, getAnnotationMutability(annot)) }
				case _ =>
				}
			}
			lub(mut, getSimpleMutability(tp.underlying))
		}*/
		//case tp @ TermRef(prefix, name) => viewpointAdapt(getSimpleMutability(prefix), getSimpleMutability(tp.underlying))
		//case tp @ TypeRef(prefix, name) => getSimpleMutability(tp.underlying)
		case tp @ TypeBounds(lo, hi) => simpleTypeBoundsMutability(lo, hi)
		case tp @ AndType(tp1, tp2) => glb(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp @ OrType(tp1, tp2) => lub(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp: TypeProxy => getSimpleMutability(tp.underlying)
		//case tp @ MethodType(paramNames, paramTypes) =>
		//	MethodMutability(paramTypes map { t => getSimpleMutability(t) }, getSimpleMutability(tp.resultType))
		case tp: ErrorType => errortype()
		case _ => unannotated()
	}

	/** Finds the mutability bounds for the given type bounds.
	If any annotations are explicitly present, bounds are determined entirely from explicit annotations.
	Else-wise, bounds are determined from the default mutabilities of the bounding types.
	*/
	def simpleTypeBoundsMutability(lo: Type, hi: Type)(implicit ctx: Context): SimpleMutability = {
		val mLo = explicitSimpleMutability(lo)
		val mHi = explicitSimpleMutability(hi)
		if (mLo.isInstanceOf[errortype] || mHi.isInstanceOf[errortype]) errortype()
		else if (mLo.isAnnotated && mHi.isAnnotated) narrowestBound(mLo, mHi)
		else if (mLo.isAnnotated) mLo
		else if (mHi.isAnnotated) mHi
		else narrowestBound(getSimpleMutability(lo), getSimpleMutability(hi))
	}
	
	/** If t refers a sequence of annotations, returns the mutability of the outermost mutability annotation. */
	def explicitSimpleMutability(t: Type)(implicit ctx: Context): SimpleMutability = t match {
		case t @ AnnotatedType(annot, underlying) =>
			val mut = getAnnotationMutability(annot)
			if (mut.isAnnotated) mut
			else explicitSimpleMutability(underlying)
		case t: ErrorType => errortype()
		case _ => unannotated()
	}

	def simpleMemberMutability(tp: Type, name: Name)(implicit ctx: Context): SimpleMutability = tp match {
		// Search for the member in a ClassInfo.
		// If more than one member matches, then take the upper bound of all matched members.
		case tp @ ClassInfo(prefix, cls, classParents, decls, selfInfo) =>
			var mut: SimpleMutability = notfound()
			cls.denot.findMember(name, prefix, EmptyFlags).alternatives.foreach { d =>
				if (mut.isAnnotated) mut = lub(mut, getSimpleMutability(d.info))
				else mut = getSimpleMutability(d.info)
			}
			mut
			
		case tp @ RefinedType(parent, refinedName) =>
			if (name eq refinedName)
				getSimpleMutability(tp.refinedInfo)
			else
				simpleMemberMutability(parent, name)
				
		case tp @ AndType(tp1, tp2) =>
			glb(simpleMemberMutability(tp1, name), simpleMemberMutability(tp2, name))
			
		case tp @ OrType(tp1, tp2) =>
			lub(simpleMemberMutability(tp1, name), simpleMemberMutability(tp2, name))
			
		case tp: TypeProxy =>
			simpleMemberMutability(tp.underlying, name)
			
		//case tp: MethodOrPoly =>
		//	simpleMemberMutability(tp.resultType, name)
			
		case _ => notfound()
	}
	
	def typeRefinementMutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = t2 match {
		case t2 @ RefinedType(parent, refinedName) => {
			val mutability1 = simpleMemberMutability(t1, refinedName)
			val mutability2 = simpleMemberMutability(t2, refinedName)
			// Possibilities:
			// * mutability1 is notfound -- should not happen if t1 is really a subtype of t2,
			//   but I am ignoring PolyTypes and a few other things, so we'll just return true in this case.
			// * mutability1 is something else -- simple mutability subtyping applies.
			// Also makes sure refinements in the parent type are OK.
			simpleSubtype(mutability1, mutability2) && typeRefinementMutabilitySubtype(t1, parent)
		}
		
		// TODO: And/Or/Bounds/etc.?
		
		case t2: TypeProxy =>
			typeRefinementMutabilitySubtype(t1, t2.underlying)
		
		case _ => true   // can't reject the subtype relation based on type refinements
	}
	
	def mutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		// Simple test: if same type, then same mutability
		if (t1 eq t2) return true
	
		// Check simple (top-level) mutability.
		if (!simpleSubtype(getSimpleMutability(t1), getSimpleMutability(t2))) return false
	
		// Check mutability of method signatures and type refinements.
		typeRefinementMutabilitySubtype(t1, t2)
		// TODO: (unapplied) method mutability
	}
	
	// TODO: copy all enclosing types if underlying method result type changes? (Yes.)
	private[this] def copyMethodWithResult(mt: MethodType, mut: SimpleMutability)(implicit ctx: Context): Type =
		addAnnotation(mt, toAnnotation(mut))	


	/** If t refers to a function type, then t can have: a mutability on its apply method,
	and a mutability on the closure object itself. When a closure is created, that closure's apply
	should have the same result mutability as the method it is closing over - a copying of the
	method's signature is involved here. Therefore, signtaures should also have result mutabilities.
	
	When a multi-parameterset method is called, its result is either a raw method type
	or a closure object. If a raw method type, then it suffices to place the result mutability
	on the returned method type (or on the returned method's final result type).
	If a closure object, then the closure's apply signature/type must take the resulting mutability. */
	def copyWithResult(t: Type, mut: SimpleMutability)(implicit ctx: Context): Type = {
		t match {
			case mt @ MethodType(paramNames, paramTypes) =>
				mt.derivedMethodType(paramNames, paramTypes, copyWithResult(mt.resultType, mut))
			case _ =>
				if (mut.isAnnotated)
					new AnnotatedType(toAnnotation(mut), t)    // TODO: strip off old annotations.
				else
					new AnnotatedType(toAnnotation(mutable()), t)  // TODO: strip off old annotations.
		}
	}
	
	def argMutToResult(mut: SimpleMutability): SimpleMutability = mut match {
		case mutable() => polyread()
		case polyread() => minpolyread()
		case _ => mut
	}
	
	/** Upgrade the return type of a polyread method. Assumes t is a MethodType, and argMut
	is the LUB of arguments that have been applied to polyread parameters.
	*/
	def copyMethodWithModifiedResult(t: Type, argMut: SimpleMutability)(implicit ctx: Context): Type = t match {
		case mt @ MethodType(paramNames, paramTypes) =>
			val prevResultMut = getSimpleMutability(mt.finalResultType)
			if (prevResultMut.isPolyread && simpleSubtype(prevResultMut, argMutToResult(argMut)))
				copyWithResult(t, argMutToResult(argMut))
			else
				t
	}
	
	def isParameter(sym: Symbol)(implicit ctx: Context): Boolean = {
		if (sym.denot.flags.is(Param)) true
		else false  // TODO: more?
	}
	


	private[dotc] def withSimpleMutability(t: Type, tmt: SimpleMutability)(implicit ctx: Context): Type = t.dealias match {
		case AnnotatedType(annot, underlying) =>
			if (getAnnotationMutability(annot).isAnnotated)
				withSimpleMutability(underlying, tmt)  // strip existing simple TMT annotations
			else
				new AnnotatedType(annot, withSimpleMutability(underlying, tmt))  // leave non-TMT annotations in place
		case _ =>
			if (tmt.isAnnotated)
				new AnnotatedType(toAnnotation(tmt), t)  // add new TMT annotation
			else if (getSimpleMutability(t).isAnnotated)
				new AnnotatedType(toAnnotation(mutable()), t)  // tmt is unannotated, but t is - so apply @mutable
			else
				t   // tmt and t are both unannotated - leave as unannotated
	}

	private[this] def modifyFinalResultType(t: Type, tmt: SimpleMutability)(implicit ctx: Context): Type = t match {
		case mt @ MethodType(paramNames, paramTypes) =>
			mt.derivedMethodType(paramNames, paramTypes, modifyFinalResultType(mt.resultType, tmt))
		case _ => withSimpleMutability(t, tmt)
	}

	/*def modifiedResultType(res: Type, modifier: SimpleMutability)(implicit ctx: Context): Type = res match {
		case mt: MethodType =>
			modifyFinalResultType(mt, lub(modifier, getSimpleMutability(mt.finalResultType)))
		case _ =>
			withSimpleMutability(res, lub(modifier, getSimpleMutability(res)))
	}*/
	
	def modifiedResultType(res: Type, modifier: SimpleMutability)(implicit ctx: Context): Type = { //res match {
		def mutabilityOnPartialApplication(tmt: SimpleMutability) = tmt match {
			case mutable()  => mutable()
			case polyread() => modifier match {
				case mutable()     => polyread()
				case polyread()    => minpolyread()
				case readonly()    => readonly()
				case unannotated() => polyread()
				case errortype()   => errortype()
			}
			case minpolyread() => modifier match {
				case mutable()     => minpolyread()
				case polyread()    => minpolyread()
				case readonly()    => readonly()
				case unannotated() => minpolyread()
				case errortype()   => errortype()
			}
			case readonly()    => readonly()
			case unannotated() => unannotated()
			case errortype()   => errortype()
		}
		def mutabilityOnTotalApplication(tmt: SimpleMutability) = tmt match {
			case mutable()  => mutable()
			case polyread() => modifier match {
				case mutable()     => mutable()
				case polyread()    => polyread()
				case readonly()    => readonly()
				case unannotated() => unannotated()
				case errortype()   => errortype()
			}
			case minpolyread() => modifier match {
				case mutable()     => polyread()
				case polyread()    => polyread()
				case readonly()    => readonly()
				case unannotated() => polyread()
				case errortype()   => errortype()
			}
			case readonly()    => readonly()
			case unannotated() => unannotated()
			case errortype()   => errortype()
		}
		res match {
			case mt: MethodType =>
				modifyFinalResultType(mt, mutabilityOnPartialApplication(getSimpleMutability(mt.finalResultType)))
			case _ =>
				// if the result is a Function class, then use the partial application mutability
				if (ctx.definitions.isFunctionType(res))
					withSimpleMutability(res, mutabilityOnPartialApplication(getSimpleMutability(res)))
				else
					withSimpleMutability(res, mutabilityOnTotalApplication(getSimpleMutability(res)))
		}
	}


	/***
	DISCOURSE
	On Viewpoint Adaptation of Method Return Types
	---
	THe problem with default viewpoint adaptation on method return types is that it doesn't "scale"
	in the presence of closures. e.g.:
	
	// Listing 1:
	import annotation._
	trait J {
		var p: AnyRef @polyread
		def d1(a: AnyRef @polyread)(b: AnyRef @polyread): AnyRef @polyread
		val f1 = d1(p) _
	}
	
	The statement "val f1 = d1(p) _" desugars to (approximately):
	
	val f1: Function1[AnyRef @polyread, AnyRef @minpolyread] = {
		val $1$: AnyRef @polyread = p
		{ (b: AnyRef @polyread) => d1($1$)(b) }   // enclosing parameters don't modify the return type, but enclosing variables do.
			//  is it unnecessarily restrictive to assume that a non-mutable, non-parameter argument forces the result to readonly?
			//  (it is not unnecessarily restrictive.) Let's try it.
	}

	Which desugars to (approximately):
	
	val f1: Function1[AnyRef @polyread, AnyRef @minpolyread] = {
		val $1$: AnyRef @polyread = p
		{
			def $anonfun(b: AnyRef @polyread): AnyRef @minpolyread = d1($1$)(b)
			new Function1[AnyRef @polyread, AnyRef @minpolyread] {
				def apply(v1: AnyRef @polyread): AnyRef @minpolyread = $anonfun(v1)
			}
		}
	}
	
	Possible counterexample:
	
	def f1(x: @polyread) = {
		var m: @mutable
		d1(x)(x)   // OK: result is polyread
		d1(p)(x)   // result is @readonly, b/c we don't necessarily know where p came from
		d1(x)(p)   // also readonly
		d1(x)(m)   // polyread, b/c x could be used in a readonly context  -- intermediate result is @minpolyread
		
		d1(x)(x)   // results: @minpolyread / @polyread
		d1(m)(x)   // results: @polyread / @polyread
		d1(m)(m)   // results: @polyread / @mutable
		d1(m)(p)   // results: @polyread / @readonly
		d1(p)(m)   // results: @readonly / @readonly
	}
	
	Underlying issue:
	
	It is possible that b in the following code should be forced to @readonly:
	def outer(a: @polyread) = {
		val b = a
	}
	The idea of @polyread is that @polyread will be adapted to either @mutable or @readonly (or unannotated),
	depending upon whether any @readonly arguments are supplied at a call site context.
	An unadapted method can call another unadapted method, in which case the viewing context of
	the callee is the same as the caller. In this case, the callee is allowed to return @polyread.
	Path-dependent typing can also preserve @polyread - a function is permitted to return mutable
	fields of @polyread parameters.
	
	If, however, a @polyread parameter is stored in a field, then that field may potentially be accessed
	from a different call site context, so the field's type should be @readonly.
	The same applies for local variables: Every local variable is either @mutable or @readonly.

	Forcing fields and local variables to be either @mutable or @readonly means that @polyread
	can only appear on parameters and localities of those parameters, so I don't have to try to
	figure out which arguments actually correspond to parameters.
	The side effect is that hidden @polyread arguments are forced to @readonly upon closure creation,
	allowing the closure to be passed around freely.

	***/


	// denoting types: reset denotation
	// on symbol creation:
	//  annotate result type with prefixed annotations; set symbol denotation to: prev. denotation with denot.info wrapped in annotatedtypes
	//  - MethodTypes: copy type with annotated result type
	//  - others: just overwrite with new denotation
	// on method application:
	//  - TermRef types: just copy & reset denotation, before application occurs.
	//    i.e.: find underlying method type; copy method type;
	//    annotate result if result is not a method type,
	//    else repeat on method's result type;
	//    return denotation on method type; caller calls withDenot(method type's revised denotation) on termRef.

	/*def copyWithResultType(t: Type, mut: Mutability): Type = t match {
		case t: MethodType(paramNames, paramTypes) =>
			val newMt =
				if (t.isJava) JavaMethodType(paramNames, paramTypes)(t.resultType)
				else if(t.isImplicit) ImplicitMethodType(paramNames, paramTypes)(t.resultType)
				else MethodType(paramNames, paramTypes)(t.resultType)
			newMt.overrideResultMutability = mut
			newMt
		case TermRef(prefix, name) => 
	}*/
	
	/* ATTEMPT 2
	
	// not working -- next attempt is to call getSimpleMutability from appropriate
	// places in TypeComparer.isSubtype. Questions:
	// - Which places need a mutability subtyping?
	// If mutability subtyping should only be done in certain places:
	//   copy TypeComparer.isSubtype and add mutability checks where needed.
	def mutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		if (t1 eq t2) return true
		
		if (!simpleSubtype(getSimpleMutability(t1), getSimpleMutability(t2))) return false
		
		println("Secondary mutability check on types " + t1 + " and " + t2)
		
		t2 match {
			case t2 @ RefinedType(parent, refinedName) =>
				println ("  * Checking name: " + refinedName)
				t1.member(refinedName).alternatives.foreach({ denot_t1_name =>
					println("  * Found in type t1, with type " + denot_t1_name.info)
					if (!mutabilitySubtype(denot_t1_name.info, t2.refinedInfo))
						return false
				})
			//case TypeBounds(lo2, hi2) => mutabilitySubtype(t1, lo)
			case AndType(t21, t22) => return mutabilitySubtype(t1, t21) && mutabilitySubtype(t1, t22)
			case OrType(t21, t22) => return mutabilitySubtype(t1, t21) || mutabilitySubtype(t1, t22)
			case t2: TypeProxy => return mutabilitySubtype(t1, t2.underlying)
			case _ => t1 match {
				//case TypeBounds(lo1, hi1) => mutabilitySubtype(hi, t2)
				case AndType(t11, t12) => return mutabilitySubtype(t11, t2) || mutabilitySubtype(t12, t2)
				case OrType(t11, t12) => return mutabilitySubtype(t11, t2) && mutabilitySubtype(t11, t2)
				case t1: TypeProxy => return mutabilitySubtype(t1.underlying, t2)
				case _ =>
			}
		}
		
		return true
	}
	*/
	
	
	/* ATTEMPT 1
	
	def mutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		if (t1 == t2) return true
		
		if (!simpleSubtype(getSimpleMutability(t1), getSimpleMutability(t2))) return false
		// TODO: method subtypes
		
		// For each named member of m2, make sure that it is a supertype of that member in m1
		t2.memberNames(takeAllFilter).foreach({ name =>
			t2.member(name).alternatives.foreach({ denot_t2 =>
				t1.member(name).alternatives.foreach({ denot_t1 =>
					if (!mutabilitySubtype(denot_t1.info, denot_t2.info)) return false
				})
			})
		})
		// TODO: stop infinite recursions?
		
		return true
	}
	*/
	
}