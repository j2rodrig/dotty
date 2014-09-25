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

/***
DOCUMENTATION
-------------

	SUBTYPE RELATIONS
	
		M <: P_o <: R
		
		where
		
		M is mutable
		R is readonly
		P_o is polymorphic, defined at origin o
		o is any method symbol

	POLYMETH

		Polyread types are associated with methods.

***/

object TransitiveMutabilityTypes {

	trait Tmt {
		def <:<(other: Tmt): Boolean = tmtSubtypeOf(this, other)
		//def toAnnotation: Annotation
		
		def isReadonly = this.isInstanceOf[Readonly]
		def isPolyread = this.isInstanceOf[Polyread]
		def isMutable = this.isInstanceOf[Mutable]
		def isUnannotated = this.isInstanceOf[UnannotatedTmt]
		def isError = this.isInstanceOf[ErrorTmt]
		
		val origin: Symbol = NoSymbol   // origin if Polyread, NoSymbol otherwise
		val adaptations: Tmt = UnannotatedTmt()  // For Polyread result types, LUB of adaptations
		
		def exists = isReadonly || isPolyread || isMutable
	}

	case class Readonly() extends Tmt { }
	case class Polyread(override val origin: Symbol) extends Tmt { }
	case class Mutable() extends Tmt { }
	case class UnannotatedTmt() extends Tmt { }
	case class ErrorTmt() extends Tmt { }

	def withOrigin(tmt: Tmt, origin: Symbol): Tmt =
		// convenience function: if tmt is Polyread, returns a Polyread with the given origin
		tmt match {
			case _: Polyread => Polyread(origin)
			case _ => tmt
		}

	/// Least upper bound.
	def lub(tmt1: Tmt, tmt2: Tmt): Tmt =
		if (tmt1.isError || tmt2.isError) ErrorTmt()
		else if (tmt1.isReadonly || tmt2.isReadonly) Readonly()
		else if (tmt1.isPolyread && tmt2.isPolyread) {
			if (tmt1.origin == tmt2.origin) tmt1      // polyread types are equal if same origin
			else Readonly()     // if different origin, conservatively return readonly
		}
		else if (tmt1.isPolyread) tmt1   // any polyread dominates a mutable
		else if (tmt2.isPolyread) tmt2
		else if (tmt1.isMutable || tmt2.isMutable) Mutable()
		else UnannotatedTmt()
		
	def tmtSubtypeOf(tmt1: Tmt, tmt2: Tmt): Boolean =
		// Don't fail subtypes if a type error already exists. Prevents generation of spurious error messages.
		if (tmt1.isError || tmt2.isError) true
		else tmt2 match {
			case Readonly() => true
			case Polyread(origin2) => tmt1 match {
				case Readonly() => false
				case Polyread(origin1) => origin1 == origin2  // Polyreads are only equal if their origins are equal
				case _ => true
			}
			case Mutable() => !tmt1.isReadonly && !tmt1.isPolyread
			case UnannotatedTmt() => !tmt1.isReadonly && !tmt1.isPolyread
		}

	/// Finds the resulting TMT of an argument applied to a method parameter.
	def adaptPartial(argument: Tmt, parameter: Tmt, result: Tmt): Tmt =
		if (!parameter.isPolyread || !result.isPolyread) result  // no viewpoint adaptation
		else if (argument.isError || parameter.isError || result.isError) ErrorTmt()   // error!
		else new Polyread(result.origin) { override val adaptations = lub(result.adaptations, argument) }
		
		//else {
			/*if (argument.isReadonly) Readonly()
			else if (argument.isPolyread) lub(argument, result.adaptations)
			else if (argument.isMutable) result
			else if (argument.isUnannotated) result*/
			//else Polyread(result.methods | argument.methods)
		//}
	
	def adaptFinal(result: Tmt): Tmt =
		if (result.isPolyread) result.adaptations else result
	
		/*val finalResult = Polyread(adaptedResult.methods -- originalResult.methods)
		if (finalResult.isPolyread && finalResult.methods.isEmpty) Mutable()
		else finalResult*/

	def adaptPath(qualifier: Tmt, qualified: Tmt): Tmt =
		lub(qualifier, qualified)


	//---- Interfacing with Scala types ----
	
	/// Returns a denotation from the point of view of the given type.
	def povDenotation(tp: Type, denot: Denotation): Denotation = tp match {
		case TermRef(prefix, name) => denot  // temp
	}

	class TmtAnnotation(tree: Tree, val tmt: Tmt) extends ConcreteAnnotation(tree) { }

	def toAnnotation(tmt: Tmt)(implicit ctx: Context): Annotation =
		new TmtAnnotation(
			New (
				tmt match {
					case tmt: Mutable => ctx.definitions.MutableClass.typeRef
					case tmt: Polyread => ctx.definitions.PolyreadClass.typeRef
					case tmt: Readonly => ctx.definitions.ReadonlyClass.typeRef
				},
				Nil
			),
			tmt)

	def tmt(tp: Type)(implicit ctx: Context): Tmt =
		tp match {
			case AnnotatedType(annot, underlying) =>
				annot match {
					case annot: TmtAnnotation => annot.tmt   // a TMT is already present on this type.
					case _ =>  // try to match the annotation to a known TMT class.
						if (annot matches ctx.definitions.MutableClass) Mutable()
						else if (annot matches ctx.definitions.ReadonlyClass) Readonly()
						else if (annot matches ctx.definitions.PolyreadClass) {
							// Origins should have already been set before attempting to call this method
							ctx.error(s"Unexpected @polyread", annot.tree.pos)
							tmt(underlying)
						}
						else tmt(underlying)
				}
			//case //todo:more cases
		}

	/** Wraps a TermRef such that the underlying denotation has the correct viewpoint-adapted TMT. **/
	/*class TmtAnnotationOverTermRef(forwardTo: TermRef)
		extends TermRef(forwardTo.prefix, forwardTo.name) {
		}
	def tmtWrapTermRef(tp: Type): Type =
		tp match {
			case tp: TermRef => tp  // temp
			case _ => tp  // temp
		}*/

	/* Example:
		def n(a: @pr(n), b: @pr(n)): @pr(n) = {
			def m(c: @pr(m,n), d: @pr(m,n)): @pr(m,n) = {
				m(c,d)   // result type @pr(m)
				m(a,b)   // result type @pr(n)
				n(c,d)   // result type @pr(m)
				n(a,b)   // result type @pr(n)
				m(a,c)   // result type @pr(m,n)
				n(a,c)   // result type @pr(m,n)  -- must type-check, so return type of m must be at least as restricted
			}
			n(a,b)   // result type @pr(n)
			m(a,b)   // result type @pr(n)
		}
	Example 2:
		def n(a: @pr(n), b: @pr(n)): @pr(n) = {   // @pr ORIGIN == n, where n is a method.
			var nf1: @pr(n) = a
			def m(c: @pr(m,n), d: @pr(m,n)): @pr(m,n) = {  // @pr ORIGIN == m. We know statically that m.owner == n.
											// Generally, local variables are allowed to be @polyread,
											// but fields must be conservatively typed @readonly.
				var mf1: @pr(m,n) = c
				nf1 = mf1   // error: @pr(m,n) is not a subtype of @pr(n)
				mf1 = nf1   // OK: we haven't left method n, so @pr(n) is valid here
				return nf1  // OK
			}
		}
	Example 3:
		def o (@polyread(o) op1, @polyread(o) @op2): @polyread(o) = {
			class C (@readonly cp1) {      // note: polyread is disallowed on class parameters
				var cf1: @readonly = op1   // fields are conservatively typed to @readonly
				def n (@polyread(n) np1, @polyread(n) np2): @polyread(n) = {
					var nv1: @polyread(o) = op1
					var nv2: @polyread(n) = np1
					nv2 = nv1   // OK: @polyread(o) is a subtype of @polyread(n)
					nv1 = nv2   // Error: @polyread(n) is not a subtype of @polyread(o)
					return nv1  // OK
				}
			}
		}
	*/
	
	
	/// Finds the resulting TMT of an access path selection.
	/*def select(qualifier: Tmt, qualified: Tmt): Tmt =
		if (qualifer.isError || qualified.isError) ErrorTmt()
		else if (!qualifier.exists) qualified
		else if (qualified.isPolyread) qualifier  //is this correct if qualified has many methods?
		else lub(qualifier, qualified)
		*/

	
}