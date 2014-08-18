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
import util.Positions.Position
import util.Stats._
import util.{Stats, DotClass, SimpleMap}
import ast.tpd._, printing.Texts._
import ast.untpd
import transform.Erasure
import printing.Printer
import Hashable._
import Uniques._
import collection.{mutable, Seq, breakOut}
import config.Config
import config.Printers._
import annotation.tailrec
import language.implicitConversions

import typer.Mode
import collection.{mutable => collection_mutable }
import printing.Disambiguation.disambiguated

import TypeComparer._


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
	abstract class Mutability {
		def exists: Boolean
		def isAnnotated: Boolean
		def canBeMutable: Boolean = false
		def canBeReadonly: Boolean = false
	}
	case class readonly() extends Mutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def canBeReadonly: Boolean = true
		override def toString = "@readonly"
	}
	case class polyread() extends Mutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def canBeMutable: Boolean = true
		override def canBeReadonly: Boolean = true
		override def toString = "@polyread"
	}
	case class mutable() extends Mutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = true
		override def canBeMutable: Boolean = true
		override def toString = "@mutable"
	}
	case class unannotated() extends Mutability {
		override def exists: Boolean = true
		override def isAnnotated: Boolean = false
		override def toString = "@unannotated"
	}
	case class notfound() extends Mutability {
		override def exists: Boolean = false
		override def isAnnotated: Boolean = false
		override def toString = "@notfound"
	}
	
	def simpleMutabilitySubtype(m1: Mutability, m2: Mutability): Boolean = m2 match {
		case m2: readonly => true
		case m2: polyread => !m1.isInstanceOf[readonly]
		case _ => !m1.isInstanceOf[readonly] && !m1.isInstanceOf[polyread]
	}
	
	def getAnnotationMutability(annot: Annotation)(implicit ctx: Context): Mutability = annot.symbol.name.toString match {
		case "mutable" => mutable()
		case "polyread" => polyread()
		case "readonly" => readonly()
		case _ => unannotated()
	}
	
	/** If a top-level automatically-added type annotation is incompatible with
	 *  the underlying mutability type, then return it.
	 */
	def incompatibleAutoMutabilityTypes(t: Type)(implicit ctx: Context): Option[Annotation] = t match {
		case t @ AnnotatedType(annot, tpe) if (t.isInstanceOf[AutoType]) =>
			// If t is an automatic mutability type annotation, then it must be a supertype of the underlying type.
			val mut = getAnnotationMutability(annot)
			if (mut.isAnnotated && !simpleMutabilitySubtype(getSimpleMutability(t.underlying), mut)) Some(annot)
			else incompatibleAutoMutabilityTypes(t.underlying)
		case t: TypeProxy => incompatibleAutoMutabilityTypes(t.underlying)
		case _ => None
	}
	
	/*/// Returns the annotated mutability of a symbol. For multiple annotations, returns the most restrictive mutability.
	def getSymbolMutability(sym: Symbol)(implicit ctx: Context): Mutability = {
		var accumulated: Mutability = unannotated()
		sym.denot.annotations.foreach { annot =>
			accumulated = unionMutability(accumulated, getAnnotationMutability(annot))
		}
		accumulated
	}*/

	/** Finds a mutability that bounds both m1 and m2.
	
	*/
	def narrowestFittingBound(m1: Mutability, m2: Mutability): Mutability = {
		val mut = (m1.canBeMutable || m2.canBeMutable)
		val ro = (m1.canBeReadonly || m2.canBeReadonly)
		if (mut && ro) polyread()
		else if (mut) mutable()
		else if (ro) readonly()
		else unannotated()
	}

	/// The greatest lower bound / intersection of two mutabilities.
	def intersectMutability(m1: Mutability, m2: Mutability): Mutability = {
		if (!m1.isAnnotated || !m2.isAnnotated) unannotated()
		else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
		else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
		else readonly()
	}

	/// The least upper bound / union of two mutabilities.
	def unionMutability(m1: Mutability, m2: Mutability): Mutability = {
		if (m1.isInstanceOf[readonly] || m2.isInstanceOf[readonly]) readonly()
		else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
		else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
		else unannotated()
	}
	
	/** Finds the top-level mutability of the named member of type tp.
	Returns one of: mutable, polyread, readonly, unannotated, notfound.
	*/
	/*def getNameMutability(tp: Type, name: Name)(implicit ctx: Context): Mutability = tp match {
		case tp @ ClassInfo
		case tp @ RefinedType(parent, refinedName) =>
			if (name == refinedName) getSimpleMutability(tp.refinedInfo)
			else getNameMutability(parent, name)
		case _ => notfound()
	}*/
	
	//TODO for generic mutability subtyping: see member-querying methods on Type

	/** Finds the top-level mutability of a given type.
	Returns one of: mutable, polyread, readonly, unannotated.
	*/
	def getSimpleMutability(tp: Type)(implicit ctx: Context): Mutability = tp match {
		case tp @ AnnotatedType(annot, tpe) => annot.symbol.name.toString match {
			case "mutable" => mutable()
			case "polyread" => polyread()
			case "readonly" => readonly()
			case _ => getSimpleMutability(tpe)  // unrecognized annotation: check underlying type
		}
		case tp @ TypeBounds(lo, hi) => typeBoundsMutability(lo, hi)
		case tp @ AndType(tp1, tp2) => intersectMutability(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp @ OrType(tp1, tp2) => unionMutability(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp: TypeProxy => getSimpleMutability(tp.underlying)
		case _ => unannotated()
	}

	/** Finds the mutability bounds for the given type bounds.
	If any annotations are immediately present, bounds are determined entirely from annotations.
	Else-wise, bounds are determined from the default mutabilities of the bounding types.
	*/
	def typeBoundsMutability(lo: Type, hi: Type)(implicit ctx: Context): Mutability = {
		val immLo = getImmediateMutability(lo)
		val immHi = getImmediateMutability(hi)
		if (immLo.isAnnotated && immHi.isAnnotated) narrowestFittingBound(immLo, immHi)
		else if (immLo.isAnnotated) immLo
		else if (immHi.isAnnotated) immHi		
		else {
			val deepLo = getSimpleMutability(lo)
			val deepHi = getSimpleMutability(hi)
			narrowestFittingBound(deepLo, deepHi)
		}
	}

	/** If the given type is an AnnotationType, returns mutability type.
	Non-mutability annotations at the same level are filtered out.
	Can return mutable, polyread, readonly, or unannotated.
	*/
	def getImmediateMutability(tp: Type)(implicit ctx: Context): Mutability = tp match {
		case tp @ AnnotatedType(annot, tpe) => annot.symbol.name.toString match {
			case "mutable" => mutable()
			case "polyread" => polyread()
			case "readonly" => readonly()
			case _ => getImmediateMutability(tpe)  // check underlying annotation
		}
		case _ => unannotated()
	}
	
	def mutabilityOfMember(tp: Type, name: Name)(implicit ctx: Context): Mutability = tp match {
		case tp @ ClassInfo(prefix, cls, classParents, decls, selfInfo) =>
			getSimpleMutability(cls.denot.findMember(name, prefix, EmptyFlags).info)
			// TODO: does findMember ever return NoDenotation or MultiDenotation? Handle this.
			
		case tp @ RefinedType(parent, refinedName) =>
			if (name eq refinedName)
				getSimpleMutability(tp.refinedInfo)
			else
				mutabilityOfMember(parent, name)
				
		case tp @ AndType(tp1, tp2) =>
			intersectMutability(mutabilityOfMember(tp1, name), mutabilityOfMember(tp2, name))
			
		case tp @ OrType(tp1, tp2) =>
			unionMutability(mutabilityOfMember(tp1, name), mutabilityOfMember(tp2, name))
			
		case tp: TypeProxy =>
			mutabilityOfMember(tp.underlying, name)
			
		case _ => notfound()
	}
	
	def typeRefinementMutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = t2 match {
		case t2 @ RefinedType(parent, refinedName) => {
			val mutability1 = mutabilityOfMember(t1, refinedName)
			val mutability2 = mutabilityOfMember(t2, refinedName)
			// Possibilities:
			// * mutability1 is notfound -- should not happen if t1 is really a subtype of t2,
			//   but I am ignoring PolyTypes and a few other things, so we'll just return true in this case.
			// * mutability1 is something else -- simple mutability subtyping applies.
			// Also makes sure refinements in the parent type are OK.
			simpleMutabilitySubtype(mutability1, mutability2) && typeRefinementMutabilitySubtype(t1, parent)
		}
		
		// TODO: And/Or/Bounds/etc.?
		
		case t2: TypeProxy =>
			typeRefinementMutabilitySubtype(t1, t2.underlying)
		
		case _ => true   // can't reject the subtype relation based on type refinements
	}
	
	def mutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		// Simple test: if same type, has same mutability
		if (t1 eq t2) return true
	
		// Check simple (top-level) mutability.
		if (!simpleMutabilitySubtype(getSimpleMutability(t1), getSimpleMutability(t2))) return false
	
		// Check complex (deep) mutability.
		// t1 is a subtype of t2 if all members 
		typeRefinementMutabilitySubtype(t1, t2)
	}
	
	/*def deepMutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		// Check complex (deep) mutability.
		// t1 is a subtype of t2 if all members 
		typeRefinementMutabilitySubtype(t1, t2)
	}*/
	


	/* ATTEMPT 2
	
	// not working -- next attempt is to call getSimpleMutability from appropriate
	// places in TypeComparer.isSubtype. Questions:
	// - Which places need a mutability subtyping?
	// If mutability subtyping should only be done in certain places:
	//   copy TypeComparer.isSubtype and add mutability checks where needed.
	def mutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		if (t1 eq t2) return true
		
		if (!simpleMutabilitySubtype(getSimpleMutability(t1), getSimpleMutability(t2))) return false
		
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
		
		if (!simpleMutabilitySubtype(getSimpleMutability(t1), getSimpleMutability(t2))) return false
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