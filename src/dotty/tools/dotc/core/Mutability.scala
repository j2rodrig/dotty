package dotty.tools
package dotc
package core

import util.common._
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

	object SimpleMutabilityTypes {

		abstract class SimpleMutability {
			def exists: Boolean
			def isAnnotated: Boolean
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
			override def canBeMutable: Boolean = true
			override def canBeReadonly: Boolean = true
			override def toString = "@polyread"
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
			override def toString = "@unannotated"
		}
		/** The mutability resulting from a failed search. */
		case class notfound() extends SimpleMutability {
			override def exists: Boolean = false
			override def isAnnotated: Boolean = false
			override def toString = "@notfound"
		}
		/** The mutability of an ErrorType. */
		case class errortype() extends SimpleMutability {
			override def exists: Boolean = true
			override def isAnnotated: Boolean = false
			override def toString = "@errortype"
		}

		/** Finds a mutability that bounds both m1 and m2.
		If either mutability can be adapted to @mutable, then the result can be adapted to @mutable.
		If either mutability can be adapted to @readonly, then the result can be adapted to @readonly.
		E.g., for types P and Q, the bounds expression "<:P @mutable >:Q @readonly" yields @polyread.
		The expression "<:P >:Q @polyread" is equivalent.
		*/
		def narrowestBound(m1: SimpleMutability, m2: SimpleMutability): SimpleMutability = {
			val mut = (m1.canBeMutable || m2.canBeMutable)
			val ro = (m1.canBeReadonly || m2.canBeReadonly)
			if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
			else if (mut && ro) polyread()
			else if (mut) mutable()
			else if (ro) readonly()
			else unannotated()
		}

		/// The greatest lower bound / intersection of two mutabilities.
		def glb(m1: SimpleMutability, m2: SimpleMutability): SimpleMutability = {
			if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
			else if (!m1.isAnnotated || !m2.isAnnotated) unannotated()
			else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
			else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
			else readonly()
		}
		def lower = glb _   // alias

		/// The least upper bound / union of two mutabilities.
		def lub(m1: SimpleMutability, m2: SimpleMutability): SimpleMutability = {
			if (m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype]) errortype()
			else if (m1.isInstanceOf[readonly] || m2.isInstanceOf[readonly]) readonly()
			else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
			else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
			else unannotated()
		}
		def upper = lub _   // alias
	
	}
	
	import SimpleMutabilityTypes._
	
	def simpleSubtype(m1: SimpleMutability, m2: SimpleMutability): Boolean =
		m1.isInstanceOf[errortype] || m2.isInstanceOf[errortype] ||   // don't generate extra errors for error types
			(m2 match {
				case m2: readonly => true
				case m2: polyread => !m1.isInstanceOf[readonly]
				case _ => !m1.isInstanceOf[readonly] && !m1.isInstanceOf[polyread]
			})
	
	def getAnnotationMutability(annot: Annotation)(implicit ctx: Context): SimpleMutability =
		annot.symbol.name.toString match {
			case "mutable" => mutable()
			case "polyread" => polyread()
			case "readonly" => readonly()
			case _ => unannotated()
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
		case tp @ TypeBounds(lo, hi) => simpleTypeBoundsMutability(lo, hi)
		case tp @ AndType(tp1, tp2) => glb(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp @ OrType(tp1, tp2) => lub(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp: TypeProxy => getSimpleMutability(tp.underlying)
		case tp: MethodOrPoly => getSimpleMutability(tp.resultType)
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
			
		case tp: MethodOrPoly =>
			simpleMemberMutability(tp.resultType, name)
			
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