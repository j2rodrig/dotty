package dotty.tools.dotc
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
import util.{DotClass, SimpleMap}
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
	object Simple {

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
		}
		case class polyread() extends Mutability {
			override def exists: Boolean = true
			override def isAnnotated: Boolean = true
			override def canBeMutable: Boolean = true
			override def canBeReadonly: Boolean = true
		}
		case class mutable() extends Mutability {
			override def exists: Boolean = true
			override def isAnnotated: Boolean = true
			override def canBeMutable: Boolean = true
		}
		case class unannotated() extends Mutability {
			override def exists: Boolean = true
			override def isAnnotated: Boolean = false
		}
		case class notfound() extends Mutability {
			override def exists: Boolean = false
			override def isAnnotated: Boolean = false
		}
	
		// Finds
		def narrowestFittingBound(m1: Mutability, m2: Mutability): Mutability = {
			val mut = (m1.canBeMutable || m2.canBeMutable)
			val ro = (m1.canBeReadonly || m2.canBeReadonly)
			if (mut && ro) polyread()
			else if (mut) mutable()
			else if (ro) readonly()
			else unannotated()
		}
	
		def intersectMutability(m1: Mutability, m2: Mutability): Mutability = {
			if (!m1.isAnnotated || !m2.isAnnotated) unannotated()
			else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
			else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
			else readonly()
		}
	
		def unionMutability(m1: Mutability, m2: Mutability): Mutability = {
			if (m1.isInstanceOf[readonly] || m2.isInstanceOf[readonly]) readonly()
			else if (m1.isInstanceOf[polyread] || m2.isInstanceOf[polyread]) polyread()
			else if (m1.isInstanceOf[mutable] || m2.isInstanceOf[mutable]) mutable()
			else unannotated()
		}
		
		def isSubtypeOf(m1: Mutability, m2: Mutability): Boolean = m2 match {
			case m2: readonly => true
			case m2: polyread => !m1.isInstanceOf[readonly]
			case _ => !m1.isInstanceOf[readonly] && !m1.isInstanceOf[polyread]
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

	}
	
	
	import Simple._
	


	/** Returns the simple (top-level) mutability, except for */
	//def getSimpleMutability(tp: Type)(implicit ctx: Context): Mutability = {
	//}
	
	/** Finds the mutability of the given name. The name is assumed to be a member of the given type.
	Returns one of: mutable, polyread, readonly, unannotated, notfound.
	*/
	//def getNameMutability(tp: Type, name: Name)(implicit ctx: Context): Mutability = tp match {
	//	case tp @ RefinedType(parent, refinedName) =>
	//		if (name == refinedName) tp.refinedInfo
	//}

	/** Finds the type of the given name. */
	//def findName(tp: Type, name: Name)(implicit ctx: Context): Type = tp match {
	//}
	
	
	// Subtype relation for reference immutability
	/*def isSubtypeOf(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = {
		tp1 match {
		case tp1: TypeRef => isSubtypeOf(tp1.underlying, tp2)
		case tp1: TermRef => isSubtypeOf(tp1.underlying, tp2)
		case tp1 @ RefinedType(parent, refinedName) =>
		tp1.refinedInfo
		}
	}*/

	


/*	trait RiType {
		def canBeReadonly = true
		def canBeMutable = true
		val variance = 0
		// todo: type members?
		var members = Map[Name,RiType]()
	}*/
	
	/*trait Polyread extends RiType {}

	trait Mutable extends Polyread {
		override def canBeReadonly = false
	}
	
	trait Readonly extends Polyread {
		override def canBeMutable = false
	}*/

	/*
	For a to be a subtype of b, 

	*/
	//def subtypeOf(a: RiType, b: RiType): Boolean = {
	//if (a.canBeMutab)
	//}
	
	
	/*case class OrdinaryRiType(refinement: RiTypeRefinement) {
	
	}
	
	case class RiTypeRefinement(refinedName: Name, refinedInfo: RiType) {
	}
	
	class UnknownRiType {
	}*/

	// annotatedType
	// refinedType
	// typeRef, termRef, thisType, superType

	// type bounds?
	// methodType
	// classInfo / thisType
	
	// A RefinedType may tighten the bounds of a type member.
	// Q?: What happens with RI types? Am I bounding them the same way as ordinary type bounds? Options:
	//   - custom notation for RI type bounds
	//   - attach to existing bounds types
	//   - ignore bounds types (only enforce RI for type aliases (where bound lo == bound hi))
	// A: The RI type of a type bound is the least upper bound of lo an hi.
	// However: RI type may be overriden by explicit annotation.(but doesn't compromise integrity b/c assignments are always checked.)
	// Strategy: Don't do overrides (yet). Check that overrides (and refinements) conform to parents.
	
/*	def lub(a: RiType, b: RiType): RiType = a match {
		case Mutable => b
		case Polyread => b match {
			case Readonly => Readonly
			case _ => Polyread
		}
		case Readonly => a
	}
	
	def glb(a: RiType, b: RiType): RiType = a match {
		case Mutable => a
		case Polyread => b match {
			case Mutable => Mutable
			case _ => Polyread
		}
		case Readonly => b
	}
	
	def getRiType(tpe: Type)(implicit ctx: Context): RiType = tpe match {
		case TypeBounds(lo, hi) =>
	
		//case tpe: AndType =>
		//case tpe: OrType =>
		//case tpe @ RefinedType(parent, refinedName) =>
		//	tpe.refinedInfo
		//case tpe @ MethodType(paramNames, paramTypes) =>
		//case tpe: ProxyType => getRiType(tpe.underlying)
	}
	
	*/

	//object RiType {
		/** Default mapping from types to RI Types */
		/*def apply(tpe: Type)(implicit ctx: Context): RiType = {
			// 1. if derived type exists, return it
			// 2. query for top-level annotations
			// 3. query for type parameters...
			// 4. if a derived type exists at any level, return it
			
			//if (!tpe.ri.isInstance[NoRiType]) tpe.ri
			
			tpe match {
				case tpe: AnnotatedType =>
			}
		}*/
	//}

	//abstract class RiType {
	//	var params: List[RiType] = Nil
	//}
	
	//case class NoRiType() extends RiType {}
	
	//case class Mutable() extends RiType {}
	
	//case class PolyRead() extends RiType {}
	
	//case class Readonly() extends RiType {}
	
}