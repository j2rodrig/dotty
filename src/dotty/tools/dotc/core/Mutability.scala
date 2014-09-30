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
import printing.Texts.Text

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

Solution alternative 4: ... work in progress ...

*/



	// NEW STUFF

	trait Tmt {
		def origin: Symbol = NoSymbol   // origin if Polyread, NoSymbol otherwise
		def adaptations: Tmt = UnannotatedTmt()  // For Polyread result types, LUB of adaptations
		
		def <:<(other: Tmt): Boolean = tmtSubtypeOf(this, other)
		
		def isReadonly = this.isInstanceOf[Readonly]
		def isPolyread = this.isInstanceOf[Polyread]
		def isMutable = this.isInstanceOf[Mutable]
		def isUnannotated = this.isInstanceOf[UnannotatedTmt]
		def isError = this.isInstanceOf[ErrorTmt]
	
		def exists = isReadonly || isPolyread || isMutable

		// Convenience methods
		def withOrigin(_origin: Symbol): Tmt = this  // for non-polyread types, does nothing.
		def withAdaptations(_adaptations: Tmt): Tmt = this  // for non-polyread types, does nothing.
	
		// OLD STUFF
	}
	case class Readonly() extends Tmt {
		override def toString = "@readonly"
	}
	case class Polyread() extends Tmt {
		override def withOrigin(_origin: Symbol): Tmt = PolyreadWithOriginAndAdaptations(_origin, adaptations)
		override def withAdaptations(_adaptations: Tmt): Tmt = PolyreadWithOriginAndAdaptations(origin, _adaptations)
		override def toString(): String = {
			val orig = if (origin eq NoSymbol) "NoSymbol" else s"${origin}"
			if (adaptations.exists) s"@polyread($orig,adapted with $adaptations)"
			else s"@polyread($orig)"
		}
	}
	case class minpolyread() extends Tmt {
		override def toString = "@minpolyread"
	}
	case class Mutable() extends Tmt {
		override def toString = "@mutable"
	}
	/** The mutability of a type where no mutability has been specified. */
	case class UnannotatedTmt() extends Tmt {
		override def toString = "<unannotated TMT>"
	}
	/** The mutability resulting from a failed search. */
	case class notfound() extends Tmt {
		override def toString = "<TMT Not Found>"
	}
	/** The mutability of an ErrorType. */
	case class ErrorTmt() extends Tmt {
		override def toString = "<TMT Error Type>"
	}
	
	def PolyreadWithOriginAndAdaptations(_origin: Symbol, _adaptations: Tmt): Polyread =
		new Polyread() {
			override def origin = _origin
			override def adaptations = _adaptations
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
	
	/// Greatest lower bound.
	def glb(tmt1: Tmt, tmt2: Tmt): Tmt =
		if (tmt1.isError || tmt2.isError) ErrorTmt()
		else if (!tmt1.exists || !tmt2.exists) UnannotatedTmt()  // preserve unannotated-ness
		else if (tmt1.isMutable || tmt2.isMutable) Mutable()
		else if (tmt1.isPolyread && tmt2.isPolyread) {
			if (tmt1.origin == tmt2.origin) tmt1      // polyread types are equal if same origin
			else Mutable()     // if different origin, conservatively return mutable
		}
		else if (tmt1.isPolyread) tmt1   // any polyread is below readonly
		else if (tmt2.isPolyread) tmt2
		else Readonly()
	
	// Subtype relation: is tmt1 <:< tmt2?
	def tmtSubtypeOf(tmt1: Tmt, tmt2: Tmt): Boolean =
		// Don't fail subtypes if a type error already exists. Prevents generation of spurious error messages.
		if (tmt1.isError || tmt2.isError) true
		else
			tmt2 match {
				case Readonly() => true
				case Polyread() => tmt1 match {
					case Readonly() => false
					case Polyread() => (tmt1.origin == tmt2.origin // Polyreads are only equal if their origins are equal.
									 || tmt2.origin == NoSymbol)   // Or if tmt2's origin has not been set --
									                               //  this can happen when Function types are created.
																   //  Should be OK provided we don't try to instantiate
																   //  an object with a non-origin polyread type.
					case _ => true
				}
				case Mutable() => !tmt1.isReadonly && !tmt1.isPolyread
				case UnannotatedTmt() => !tmt1.isReadonly && !tmt1.isPolyread
			}


	//--- Tree transformations ---//

	/*def setOrigin(mods: untpd.Modifiers, origin: Symbol)(implicit ctx: Context): untpd.Modifiers = {
		val annotations1 = mods.annotations mapconserve { annot =>
			val tm = tmt(annot)
			if (tm.isPolyread) toAnnot(tm withOrigin origin)
			else annot
		}
		Modifiers(mods.flags, mods.privateWithin, annotations1)
	}*/
	
	/*def typeTreeWithOrigin(tree: untpd.Tree, origin: Symbol)(implicit ctx: Context): untpd.Tree =
		tree match {
			case untpd.Annotated(annot, arg) =>
				val annot1 = typedExpr(annot, defn.AnnotationClass.typeRef)
				if (tmt(annot1.tpe).isPolyread)
					cpy.Annotated(tree, annot1 withType, arg)
			case _ =>
				tree
		}
	
	def polyParamsWithOrigin(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context): untpd.DefDef = {
		val DefDef(mods, name, tparams, vparamss, tpt, rhs) = ddef
		val vparamss1 = vparamss nestedMapconserve { vdef =>
			val ValDef(vmods, vname, vtpt, vrhs) = vdef
			
			val vtpt1 = typeTreeWithOrigin(vtpt, sym)
			
			val Modifiers(flags, privateWithin, annotations) = vmods
			val annotations1 = annotations mapconserve { annotation =>
				val tm = tmt(annotation)
				if (tm.isPolyread) toAnnot(tm withOrigin sym)
				else annotation
			}
			
			cpy.ValDef(cpy.Modifiers(flags, privateWithin, annotations1), vname, vtpt1, vrhs)
		}
		cpy.DefDef(ddef, mods, name, tparams, vparamss1, tpt, rhs)
	}*/


	//--- Conversions to/from annotations ---//

	/// Special annotation that carries extra TMT information.
	class TmtAnnotation(tree: Tree, val tmt: Tmt) extends ConcreteAnnotation(tree) {
		override def toText(implicit ctx: Context): Text = tmt.toString
	}
	
	/// Finds the TMT of the given annotation.
	def tmt(annot: Annotation)(implicit ctx: Context): Tmt =
		annot match {
			case annot: TmtAnnotation => annot.tmt   // This annotation carries TMT information.
			case _ =>  // get TMT based on annotation class.
				if (annot matches ctx.definitions.ReadonlyClass) Readonly()
				else if (annot matches ctx.definitions.PolyreadClass) Polyread()
				else if (annot matches ctx.definitions.MutableClass) Mutable()
				else UnannotatedTmt()
		}
	
	/// Builds a TmtAnnotation from the given TMT.
	def toAnnot(tm: Tmt)(implicit ctx: Context): Annotation =
		new TmtAnnotation(
			New (
				tm match {
					case tm: Mutable  => ctx.definitions.MutableClass.typeRef
					case tm: Polyread => ctx.definitions.PolyreadClass.typeRef
					case tm: Readonly => ctx.definitions.ReadonlyClass.typeRef
				},
				Nil
			),
			tm)

	//--- Interfacing with the Scala type system ---//
	
	/// Finds the TMT of the given type.
	def tmt(tp: Type)(implicit ctx: Context): Tmt =
		tp dealias match {  // type aliases are removed.
			case AnnotatedType(annot, underlying) =>
				val tm = tmt(annot)
				if (tm.exists) tm else tmt(underlying)
			case tp @ TypeBounds(lo, hi) => tmtFromTypeBounds(lo, hi)  // TODO: deal with type bounds in typer?
			case tp @ AndType(tp1, tp2) => glb(tmt(tp1), tmt(tp2))
			case tp @ OrType(tp1, tp2) => lub(tmt(tp1), tmt(tp2))
			case tp: TypeProxy => tmt(tp.underlying)
			case tp: ErrorType => ErrorTmt()
			case _ => UnannotatedTmt()
		}

	/// Finds the TMT of the given type, but only if tp is an AnnotatedType (type aliases and proxies return UnannotatedTmt).
	/// TODO: Maybe type bounds are best taken care of by examining type trees -- transform type trees (to add annots directly?)
	def tmtNoRecurse(tp: Type)(implicit ctx: Context): Tmt =
		tp match {
			case AnnotatedType(annot, underlying) =>
				val tm = tmt(annot)
				if (tm.exists) tm else tmt(underlying)
			case tp: ErrorType => ErrorTmt()
			case _ => UnannotatedTmt()
		}

	/// Finds the TMT for a type bounds declaration, e.g., T <: A >: B.
	def tmtFromTypeBounds(lo: Type, hi: Type)(implicit ctx: Context): Tmt = {
		/// If a TMT annotation is immediately present (not via an alias or proxy),
		/// then only consider immediate annotations.
		val (tmtLo, tmtHi) = (tmtNoRecurse(lo), tmtNoRecurse(hi))
		if (tmtLo.exists || tmtHi.exists)
			lub(tmtLo, tmtHi)
		else
			lub(tmt(lo), tmt(hi))  // default: look through alias/proxy types.
	}

	/** Adds the given TMT to the given type. Existing top-level TMT annotations are removed.
	    Assumes the given type is not a methodic type - annotating methodic types directly can break stuff. **/
	def withTmt(tp: Type, tm: Tmt)(implicit ctx: Context): Type =
		if (tmt(tp).isError) tp
		else
			tp dealias match {  // remove type aliases (to find annotations underneath)
				case AnnotatedType(annot, underlying) =>
					if (tmt(annot).exists)   // this is a TMT annotation: remove it
						withTmt(underlying, tm)
					else  // this is not a TMT annotation (but may have a TMT annotation underneath): keep it
						new AnnotatedType(annot, withTmt(underlying, tm))
				case _ =>
					if (!tm.isUnannotated) new AnnotatedType(toAnnot(tm), tp)
					else if (tmt(tp).exists) new AnnotatedType(toAnnot(Mutable()), tp)  // add @mutable to mask underlying stuff
					else tp  // tp and tmt are both unannotated -- no action needed
			}

	/** Adds the given TMT to the given type. For methodic types, adds the TMT to the result type. **/
	def withResultTmt(tp: Type, tm: Tmt)(implicit ctx: Context): Type =
		tp dealias match {
			//case mt @ MethodType(paramNames, paramTypes) =>
			//	mt.derivedMethodType(paramNames, paramTypes, withResultTmt(mt.resultType, tm))
			
			//case ex @ ExprType(resultType) =>
			//	ex.derivedExprType(withResultTmt(resultType, tm))
			
			//case pt @ PolyType(paramNames) =>
			//	pt.derivedPolyType(paramNames, pt.paramBounds, withResultTmt(pt.resultType, tm))
			
			case _ => withTmt(tp, tm)
		}
	
	
	def assignOrigin(tp: Type, origin: Symbol)(implicit ctx: Context): Type = {
		val tm = tmt(tp)
		val tmAssigned = tm withOrigin origin
		if (tm == tmAssigned) tp
		else withTmt(tp, tmAssigned)
	}

	//--- Function/method applications ---//

	/** Finds the TMT adaptation for a given argument. **/
	def adaptToArg(argument: Type, parameter: Type)(implicit ctx: Context): Tmt =
		// if parameter is polyread, returns argument TMT.
		if (tmt(parameter).isPolyread) tmt(argument) else UnannotatedTmt()

	/****/
	private[this] def partiallyAppliedResultType(tp: Type, adaptations: Tmt)(implicit ctx: Context): Type =
		tp match {
			case mt @ MethodType(paramNames, paramTypes) =>
				mt.derivedMethodType(paramNames, paramTypes, partiallyAppliedResultType(mt.resultType, adaptations))
			case _ =>
				val resultTmt = tmt(tp)
				if (resultTmt.isPolyread) {
					//println(s" PARTIAL RESULT 1: ${resultTmt}")
					//println(s" PARTIAL RESULT ADAPT: ${adaptations}")
					//println(s" PARTIAL RESULT 2: ${resultTmt withAdaptations adaptations}")
					//println(s" PARTIAL RESULT TYPE: ${withTmt(tp, resultTmt withAdaptations adaptations)}")
					withTmt(tp, resultTmt withAdaptations adaptations)
				}
				else tp
		}

	/** Adaptations should include adaptations on the method's final result type. **/
	private[this] def resultTypeWithAdaptations(resultType: Type, adaptations: Tmt)(implicit ctx: Context): Type =
		// if result type is a method, partially apply.
		/*resultType match {
			case mt: MethodicType if (tmt(mt.finalResultType).isPolyread) =>
				val partialTmt = tmt(mt.finalResultType) withAdaptations adaptations
				withResultTmt(mt.finalResultType, partialTmt)
			case _ if (tmt(resultType).isPolyread) =>
				withResultTmt(resultType, adaptations)
			case _ =>
				resultType // no change: result is not a @polyread
		}*/
		resultType match {
			case mt: MethodType =>  // perform a partial application
				partiallyAppliedResultType(mt, adaptations)
			case _ =>  // perform a final application
				val resultTmt = tmt(resultType)
				if (resultTmt.isPolyread) {
					//println(s" FINAL RESULT PRE: ${resultType.dealias}")
					//println(s" FINAL RESULT 1: $resultTmt")
					//println(s" FINAL RESULT ADAPT: ${adaptations}")
					//println(s" FINAL RESULT TYPE: ${withTmt(resultType.dealias, adaptations)}")
					withTmt(resultType, adaptations)
				}
				else resultType
		}

	/** Finds the TMT-annotated result type of the given method, after the LUB of all argument
	adaptations are applied. See adaptToArg to get the adaptation of an individual argument. **/
	def methodResultWithAdapations(/*termRef: TermRef,*/ methType: Type, resultType: Type, argAdaptations: Tmt)(implicit ctx: Context): Type = {
		methType match {
			case methType: MethodType =>
				//val receiverTmt = tmt(termRef.prefix)  // TODO: how do we know if the method's `this` parameter is polyread?
				val resultAdaptations = tmt(methType.finalResultType).adaptations
				val allAdaptations = lub(argAdaptations, resultAdaptations)
				println(s"  RESULT XFORM: ${resultType.show} to ${resultTypeWithAdaptations(resultType, allAdaptations).show}")
				resultTypeWithAdaptations(resultType, allAdaptations)
			case et: ErrorType => et
		}
	}
	
	//--- Modifiers for denotations ---//
	
	def pluginSymDenot(denot: SymDenotation): SymDenotation = denot
	/*def pluginSymDenot(denot: SymDenotation): SymDenotation =
		var newInfo = denot.info
		// transfer annotations on the symbol to annotations on the symbol's type.
		denot.annotations.foreach { annot =>
			if (tmt(annot).exists)   // only transfer if really a TMT annotation
				//denot.info match {
				//	case _: MethodicType
				//}
		}
	//denot.symbol.
	//copySymDenotation(info = )*/

	def pluginNamedTypeDenot(tp: Type, denot: Denotation)(implicit ctx: Context): Denotation =
		denot /*match {  // TODO: selection
			case denot: SingleDenotation =>
				tp match {
					case TermRef(prefix, name) =>  // adapt denot to viewpoint: prefix.name
						val derivedTmt = lub(tmt(denot.info), tmt(prefix))  // TODO: What about methods? receiver (prefix) for a method is actually an argument
						denot.derivedSingleDenotation(denot.symbol, withResultTmt(denot.info, derivedTmt))
					case _ => denot  // type does not cause denotation to change
				}
			case _ => denot  // denotation was overloaded -- do nothing
		}*/

	//ctx.warning(s"No origin set for this @polyread annotation", annot.tree.pos)


	/// Text representation of a type, with special formatting.
	/// If this method breaks a line, the new line must be indented by the indents amount.
	def showSpecial(tp: Type, indents: Int = 1)(implicit ctx: Context): String =
		tp match {

			case tp @ TypeRef(prefix, name) =>
				if (tp.underlying.isInstanceOf[ClassInfo])
					s"TypeRef($name)"
				else
					s"TypeRef($name => ${showSpecial(tp.underlying,indents)})"
		
			case tp @ TermRef(prefix, name) =>
				s"TermRef(${showSpecial(prefix,indents)}.$name => ${showSpecial(tp.underlying,indents)})"
	
			case tp @ ThisType(cls) =>
				s"this(${cls.name})"
		
			case tp @ SuperType(thistpe,supertpe) =>
				s"super(${thistpe.asInstanceOf[ThisType].cls.name} => ${showSpecial(supertpe,indents)})"
	
			case ConstantType(value) =>
				value.toString
	
			case rt @ RefinedType(parent, refinedName) =>
				s"RefinedType(${showSpecial(parent,indents+1)}" + " {\n" +
				indent(indents) + s"${refinedName} => ${showSpecial(rt.refinedInfo,indents+1)}\n" +
				indent(indents-1) + "})"
	
			case mt @ MethodType(paramNames, paramTypes) =>
				var (sig, i) = ("", 0)
				while (i < paramNames.size) {
					sig = sig + s"${paramNames(i)}: ${showSpecial(paramTypes(i),indents+1)}"
					i = i + 1
				}
				val ret = showSpecial(mt.resultType, indents+1)
				s"MethodType(\n" +
					(if (i > 0) indent(indents) + s"$sig\n" else "") +
					indent(indents) + s"=>\n" +
					indent(indents) + s"result: $ret\n" +
					indent(indents-1) + s")"
					
			case pt @ PolyType(paramNames) =>
				var (sig, i) = ("", 0)
				while (i < paramNames.size) {
					sig = sig + s"${paramNames(i)}: ${showSpecial(pt.paramBounds(i),indents+1)}"
					i = i + 1
				}
				val ret = showSpecial(pt.resultType, indents+1)
				s"PolyType(\n" +
					(if (i > 0) indent(indents) + s"$sig\n" else "") +
					indent(indents) + s"=>\n" +
					indent(indents) + s"$ret\n" +
					indent(indents-1) + s")"
			
			case ExprType(resultType) =>
				s"ExprType(${showSpecial(resultType,indents)})"
			
			case RefinedThis(binder) =>
				s"RefinedThis(${showSpecial(binder,indents)})"
			
			case tp: TypeVar =>
				s"TypeVar(\n" +
				indent(indents) + s"${showSpecial(tp.origin,indents+1)}\n" +
				(if (tp.inst.exists)
					indent(indents) + s"=>\n" + indent(indents) + s"${showSpecial(tp.inst,indents+1)}\n") +
				indent(indents-1) + s")"
			
			case tp @ ClassInfo(prefix, cls, parents, decls, selfInfo) =>
				s"cls.name"
			
			case tp @ TypeBounds(lo, hi) =>
				val variance =
					if (tp.variance == 0) ""
					else if (tp.variance == 1) "<cov> "
					else "<contra> "
				if (lo eq hi) s"TypeAlias($variance${showSpecial(hi,indents)})"
				else s"$variance${showSpecial(lo,indents)} to ${showSpecial(hi,indents)}"
			
			case AnnotatedType(annot, underlying) =>
				val tm = tmt(annot)
				val text = if (tm.isUnannotated) annot.toString else tm.toString
				text + s"(${showSpecial(underlying,indents)})"
			
			case tp: ImportType =>
				s"ImportType"
			
			case WildcardType(bounds) =>
				s"WildcardType(${showSpecial(bounds,indents)})"
			
			case _ =>
				if (tp eq NoType) "NoType"
				else if (tp eq NoPrefix) "NoPrefix"
				else if (tp eq ErrorType) "ErrorType"
				else
					tp.toString
		}

	def indent(indents: Int = 0): String =
		if (indents > 0) "    " + indent(indents-1)
		else ""


	// OLD STUFF
	
	/** Viewpoint adapation for simple field selection. */
	/*def viewpointAdapt(view: Tmt, underlying: Tmt): Tmt = underlying match {
		case minpolyread() => if (view.isInstanceOf[UnannotatedTmt]) underlying else view
		case Polyread()    => if (view.isInstanceOf[UnannotatedTmt]) underlying else view
		case _             => lub(view, underlying)
	}*/
	
	/** Viewpoint adapation for simple field selection. */
	/*def viewpointAdapt(qual: Type, actual: Type)(implicit ctx: Context): Type = {
		val qualMut = tmt(qual)
		val actualMut = tmt(actual)
		withSimpleMutability(actual, viewpointAdapt(qualMut, actualMut))
	}*/
		
	def tmtMismatch(tree: Tree, pt: Type)(implicit ctx: Context): Tree = {
		typer.ErrorReporting.errorTree(tree, tmtMismatchStr(tree.tpe, pt))
	}

	def tmtMismatchStr(found: Type, expected: Type)(implicit ctx: Context) = {
		s"mutability mismatch:\n"+
		s" found   : ${found.show}\n"+
		s" required: ${expected.show}"
		//s" found   : ${tmt(found)}\n"+
		//s" required: ${tmt(expected)}"
	}
	
	/** Applies a function of the given type t with arguments.
	Type t can be a MethodType, a FunctionClass, or an alias thereof.
	Returns the result type, adapted.
	*/
	//def applyFunction(t: Type)(implicit ctx: Context): Type = {
		// 1. Find mutabilities of enclosing variables
		// 2. If t is a method type, wrap final result type in modifier
		// 2b. If result type is a FunctionClass, 
		// 3. Check args to polyread params
		//var 
		//t  // temp
	//}
	
	//def checkEnclosingVarMutabilities(applyNode: Tree) = {}


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
				(if (tmt(t).exists) t.underlying else t),
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
	/*def narrowestBound(m1: Tmt, m2: Tmt): Tmt = {
		if (m1.isInstanceOf[ErrorTmt] || m2.isInstanceOf[ErrorTmt]) ErrorTmt()
		else m2 match {
			case Readonly() => if (m1.canBeMutable) Polyread() else if (m1.isPolyread) minpolyread() else Readonly()
			case minpolyread() => if (m1.canBeMutable) Polyread() else minpolyread()
			case Polyread() => Polyread()
			case _ => m2   // anything else - just return the upper bound
		}
	}*/

	/*/// The greatest lower bound / intersection of two mutabilities.
	def glb(m1: Tmt, m2: Tmt): Tmt = {
		if (m1.isInstanceOf[ErrorTmt] || m2.isInstanceOf[ErrorTmt]) ErrorTmt()
		else if (!m1.exists || !m2.exists) UnannotatedTmt()
		else if (m1.isInstanceOf[Mutable] || m2.isInstanceOf[Mutable]) Mutable()
		else if (m1.isInstanceOf[Polyread] || m2.isInstanceOf[Polyread]) Polyread()
		else if (m1.isInstanceOf[minpolyread] || m2.isInstanceOf[minpolyread]) minpolyread()
		else Readonly()
	}
	def lower = glb _   // alias*/

	/*/// The least upper bound / union of two mutabilities.
	def lub(m1: Tmt, m2: Tmt): Tmt = {
		if (m1.isInstanceOf[ErrorTmt] || m2.isInstanceOf[ErrorTmt]) ErrorTmt()
		else if (m1.isInstanceOf[Readonly] || m2.isInstanceOf[Readonly]) Readonly()
		else if (m1.isInstanceOf[minpolyread] || m2.isInstanceOf[minpolyread]) minpolyread()
		else if (m1.isInstanceOf[Polyread] || m2.isInstanceOf[Polyread]) Polyread()
		else if (m1.isInstanceOf[Mutable] || m2.isInstanceOf[Mutable]) Mutable()
		else UnannotatedTmt()
	}
	def upper = lub _   // alias*/

	/*def simpleSubtype(m1: Tmt, m2: Tmt): Boolean =
		m1.isInstanceOf[ErrorTmt] || m2.isInstanceOf[ErrorTmt] ||   // don't generate extra errors for error types
			(m2 match {
				//case m2: MethodMutability => m1.isInstanceOf
				case m2: Readonly => true
				case m2: minpolyread => !m1.isInstanceOf[Readonly]
				case m2: Polyread => !m1.isInstanceOf[Readonly] && !m1.isInstanceOf[minpolyread]
				case _ => !m1.isInstanceOf[Readonly] && !m1.isInstanceOf[minpolyread] && !m1.isInstanceOf[Polyread]
			})*/
	
	/*def tmt(annot: Annotation)(implicit ctx: Context): Tmt =
		annot.symbol.name.toString match {
			case "mutable" => Mutable()
			case "polyread" => Polyread()
			case "minpolyread" => minpolyread()
			case "readonly" => Readonly()
			case _ => UnannotatedTmt()
		}
	
	def toAnnotation(mut: Tmt)(implicit ctx: Context): Annotation = {
		//Ident(tpnme.readonly)
		val ann = mut match {
			case Mutable() => Annotation(ctx.definitions.MutableClass, List())
			case Polyread() => Annotation(ctx.definitions.PolyreadClass, List())
			case minpolyread() => Annotation(ctx.definitions.MinpolyreadClass, List())
			case Readonly() => Annotation(ctx.definitions.ReadonlyClass, List())
		}
		ann
	}*/
	
	
	/** If a top-level automatically-added type annotation is incompatible with
	 *  the underlying mutability type, then return it.
	 *  Returns a pair: (found_annotation, expected_mutability)
	 */
	/*def incompatibleAutoMutabilityTypes(t: Type)(implicit ctx: Context): Option[(Annotation,Tmt)] = t match {
		case t @ AnnotatedType(annot, tpe) if (t.isInstanceOf[AutoType]) =>
			// If t is an automatic mutability type annotation, then it must be a supertype of the underlying type.
			val found = tmt(annot)
			val expected = tmt(t.underlying)
			if (found.exists && !tmtSubtypeOf(expected, found)) Some((annot, expected))
			else incompatibleAutoMutabilityTypes(t.underlying)
		case t: MethodType => incompatibleAutoMutabilityTypes(t.resultType)  // annotations on methods go on the result type
		case t: TypeProxy => incompatibleAutoMutabilityTypes(t.underlying)
		case _ => None
	}*/
	
	/// Convenience method for adding a list of mutability annotations.
	/*def addAnnotations(t: Type, annots: List[Annotation])(implicit ctx: Context): Type = {
		var newT = t
		annots.foreach { annot =>
			if (tmt(annot).exists)  // skip non-mutability annotations
				newT = addAnnotation(newT, annot)
		}
		newT
	}*/
	
	/** Returns t without top-level mutability annotations. */
	/*def stripMutabilityAnnots(t: Type)(implicit ctx: Context): Type = t match {
		// TODO: this method stops as soon as a non-mutability annotation is found.
		case AnnotatedType(ann, underlying) if (tmt(ann).exists) =>
			stripMutabilityAnnots(underlying)
		case _ => t
	}*/
	
	/** Returns the given type with the given mutability applied.
	Do not use this method for MethodTypes/PolyTypes/ExprTypes! (Modify the result types/modifiers instead.)
	*/
	/*def withMutability(t: Type, mut: Tmt)(implicit ctx: Context): Type =
		if (mut.exists)
			new AnnotatedType(toAnnot(mut), stripMutabilityAnnots(t))
		else {
			// No annotation supplied - try to return an unannotated result.
			// If stripping annotations does not remove t's mutability, then apply a @mutable annotation.
			val stripped = stripMutabilityAnnots(t)
			if (tmt(stripped).exists) new AnnotatedType(toAnnot(Mutable()), stripped)
			else stripped
		}*/
	
	/** Returns type t wrapped in the given annotations.
	If t is a method type, returns a copy of t with the annotation applied to the result type.
	*/
	/*def addAnnotation(t: Type, annot: Annotation)(implicit ctx: Context): Type = t match {
		case mt @ MethodType(paramNames, paramTypes) =>
			mt.derivedMethodType(paramNames, paramTypes, addAnnotation(mt.resultType, annot))
			/ *val newResultType = addAnnotation(mt.resultType, annot)
			if (newResultType != mt.resultType) {  // if result type has changed, make a copy of the method type
				if (mt.isJava) JavaMethodType(paramNames, paramTypes)(_ => newResultType)
				if (mt.isImplicit) ImplicitMethodType(paramNames, paramTypes)(_ => newResultType)
				else MethodType(paramNames, paramTypes)(_ => newResultType)
			}
			else mt  // no copy needed* /
		case _ =>
			if (tmt(annot).exists)   // only strip annotations if annot will override them
				new AnnotatedType(annot, stripMutabilityAnnots(t)) //with AutoType
			else
				new AnnotatedType(annot, t) //with AutoType
	}*/
	/*def addAnnotation(t: Type, mut: Tmt)(implicit ctx: Context): Type = {
		addAnnotation(t, toAnnot(mut))
	}*/
	
	/** Returns true if the given mutability can override the given annotation in a type expression.
	I.e., where the given mutability is applied, can the given annotation be ignored
	without changing the meaning of the program?
	The rule implemented here is: If both mut and annot have specified mutabilities,
	    then annot can be ignored (mut can override annot).
	*/
	//def overridesAnnotation(mut: Tmt, annot: Annotation)(implicit ctx: Context): Boolean =
	//	mut.exists && tmt(annot).exists
	
	//def overridesAnnotation(annot1: Annotation, annot2: Annotation)(implicit ctx: Context): Boolean =
	//	overridesAnnotation(tmt(annot1), annot2)

	/** Finds the top-level (simple) mutability of a given type.
	Returns one of: mutable, polyread, Readonly, UnannotatedTmt, ErrorTmt.
	*/
	/*def getSimpleMutability(tp: Type)(implicit ctx: Context): Tmt = tp match {
		case tp @ AnnotatedType(annot, tpe) => annot.symbol.name.toString match {
			case "mutable" => Mutable()
			case "polyread" => Polyread()
			case "minpolyread" => minpolyread()
			case "readonly" => Readonly()
			case _ => getSimpleMutability(tpe)  // unrecognized annotation: check underlying type
		}
		//case tp @ TermRef(prefix, name) => viewpointAdapt(getSimpleMutability(prefix), getSimpleMutability(tp.underlying))
		//case tp @ TypeRef(prefix, name) => getSimpleMutability(tp.underlying)
		case tp @ TypeBounds(lo, hi) => simpleTypeBoundsMutability(lo, hi)
		case tp @ AndType(tp1, tp2) => glb(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp @ OrType(tp1, tp2) => lub(getSimpleMutability(tp1), getSimpleMutability(tp2))
		case tp: TypeProxy => getSimpleMutability(tp.underlying)
		case tp: ErrorType => ErrorTmt()
		case _ => UnannotatedTmt()
	}*/

	/** Finds the mutability bounds for the given type bounds.
	If any annotations are explicitly present, bounds are determined entirely from explicit annotations.
	Else-wise, bounds are determined from the default mutabilities of the bounding types.
	*/
	/*def simpleTypeBoundsMutability(lo: Type, hi: Type)(implicit ctx: Context): Tmt = {
		val mLo = explicitSimpleMutability(lo)
		val mHi = explicitSimpleMutability(hi)
		if (mLo.isInstanceOf[ErrorTmt] || mHi.isInstanceOf[ErrorTmt]) ErrorTmt()
		else if (mLo.exists && mHi.exists) narrowestBound(mLo, mHi)
		else if (mLo.exists) mLo
		else if (mHi.exists) mHi
		else narrowestBound(tmt(lo), tmt(hi))
	}*/
	
	/** If t refers a sequence of annotations, returns the mutability of the outermost mutability annotation. */
	/*def explicitSimpleMutability(t: Type)(implicit ctx: Context): Tmt = t match {
		case t @ AnnotatedType(annot, underlying) =>
			val mut = tmt(annot)
			if (mut.exists) mut
			else explicitSimpleMutability(underlying)
		case t: ErrorType => ErrorTmt()
		case _ => UnannotatedTmt()
	}*/

	/*def simpleMemberMutability(tp: Type, name: Name)(implicit ctx: Context): Tmt = tp match {
		// Search for the member in a ClassInfo.
		// If more than one member matches, then take the upper bound of all matched members.
		case tp @ ClassInfo(prefix, cls, classParents, decls, selfInfo) =>
			var mut: Tmt = notfound()
			cls.denot.findMember(name, prefix, EmptyFlags).alternatives.foreach { d =>
				if (mut.exists) mut = lub(mut, tmt(d.info))
				else mut = tmt(d.info)
			}
			mut
			
		case tp @ RefinedType(parent, refinedName) =>
			if (name eq refinedName)
				tmt(tp.refinedInfo)
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
	}*/
	
	/*def typeRefinementMutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = t2 match {
		case t2 @ RefinedType(parent, refinedName) => {
			val mutability1 = simpleMemberMutability(t1, refinedName)
			val mutability2 = simpleMemberMutability(t2, refinedName)
			// Possibilities:
			// * mutability1 is notfound -- should not happen if t1 is really a subtype of t2,
			//   but I am ignoring PolyTypes and a few other things, so we'll just return true in this case.
			// * mutability1 is something else -- simple mutability subtyping applies.
			// Also makes sure refinements in the parent type are OK.
			tmtSubtypeOf(mutability1, mutability2) && typeRefinementMutabilitySubtype(t1, parent)
		}
		
		// TODO: And/Or/Bounds/etc.?
		
		case t2: TypeProxy =>
			typeRefinementMutabilitySubtype(t1, t2.underlying)
		
		case _ => true   // can't reject the subtype relation based on type refinements
	}*/
	
	/*def mutabilitySubtype(t1: Type, t2: Type)(implicit ctx: Context): Boolean = {
		// Simple test: if same type, then same mutability
		if (t1 eq t2) return true
	
		// Check simple (top-level) mutability.
		if (!tmtSubtypeOf(tmt(t1), tmt(t2))) return false
	
		// Check mutability of method signatures and type refinements.
		typeRefinementMutabilitySubtype(t1, t2)
		// TODO: (unapplied) method mutability
	}*/
	
	// TODO: copy all enclosing types if underlying method result type changes? (Yes.)
	//private[this] def copyMethodWithResult(mt: MethodType, mut: Tmt)(implicit ctx: Context): Type =
	//	addAnnotation(mt, toAnnot(mut))


	/** If t refers to a function type, then t can have: a mutability on its apply method,
	and a mutability on the closure object itself. When a closure is created, that closure's apply
	should have the same result mutability as the method it is closing over - a copying of the
	method's signature is involved here. Therefore, signtaures should also have result mutabilities.
	
	When a multi-parameterset method is called, its result is either a raw method type
	or a closure object. If a raw method type, then it suffices to place the result mutability
	on the returned method type (or on the returned method's final result type).
	If a closure object, then the closure's apply signature/type must take the resulting mutability. */
	/*def copyWithResult(t: Type, mut: Tmt)(implicit ctx: Context): Type = {
		t match {
			case mt @ MethodType(paramNames, paramTypes) =>
				mt.derivedMethodType(paramNames, paramTypes, copyWithResult(mt.resultType, mut))
			case _ =>
				if (mut.exists)
					new AnnotatedType(toAnnot(mut), t)    // TODO: strip off old annotations.
				else
					new AnnotatedType(toAnnot(Mutable()), t)  // TODO: strip off old annotations.
		}
	}*/
	
	/*def argMutToResult(mut: Tmt): Tmt = mut match {
		case Mutable() => Polyread()
		case Polyread() => minpolyread()
		case _ => mut
	}*/
	
	/** Upgrade the return type of a polyread method. Assumes t is a MethodType, and argMut
	is the LUB of arguments that have been applied to polyread parameters.
	*/
	/*def copyMethodWithModifiedResult(t: Type, argMut: Tmt)(implicit ctx: Context): Type = t match {
		case mt @ MethodType(paramNames, paramTypes) =>
			val prevResultMut = tmt(mt.finalResultType)
			if (prevResultMut.isPolyread && tmtSubtypeOf(prevResultMut, argMutToResult(argMut)))
				copyWithResult(t, argMutToResult(argMut))
			else
				t
	}*/
	
	/*def isParameter(sym: Symbol)(implicit ctx: Context): Boolean = {
		if (sym.denot.flags.is(Param)) true
		else false  // TODO: more?
	}*/
	


	/*private[dotc] def withSimpleMutability(tp: Type, tm: Tmt)(implicit ctx: Context): Type = tp.dealias match {
		case AnnotatedType(annot, underlying) =>
			if (tmt(annot).exists)
				withSimpleMutability(underlying, tm)  // strip existing simple TMT annotations
			else
				new AnnotatedType(annot, withSimpleMutability(underlying, tm))  // leave non-TMT annotations in place
		case _ =>
			if (tm.exists)
				new AnnotatedType(toAnnot(tm), tp)  // add new TMT annotation
			else if (tmt(tp).exists)
				new AnnotatedType(toAnnot(Mutable()), tp)  // tmt is UnannotatedTmt, but t is - so apply @mutable
			else
				tp   // tmt and t are both unannotated - leave as unannotated
	}*/

	/*private[this] def modifyFinalResultType(t: Type, tmt: Tmt)(implicit ctx: Context): Type = t match {
		case mt @ MethodType(paramNames, paramTypes) =>
			mt.derivedMethodType(paramNames, paramTypes, modifyFinalResultType(mt.resultType, tmt))
		case _ => withSimpleMutability(t, tmt)
	}*/

	/*def modifiedResultType(res: Type, modifier: Tmt)(implicit ctx: Context): Type = res match {
		case mt: MethodType =>
			modifyFinalResultType(mt, lub(modifier, getSimpleMutability(mt.finalResultType)))
		case _ =>
			withSimpleMutability(res, lub(modifier, getSimpleMutability(res)))
	}*/
	
	/*def modifiedResultType(res: Type, modifier: Tmt)(implicit ctx: Context): Type = { //res match {
		def mutabilityOnPartialApplication(tmt: Tmt) = tmt match {
			case Mutable()  => Mutable()
			case Polyread() => modifier match {
				case Mutable()     => Polyread()
				case Polyread()    => minpolyread()
				case Readonly()    => Readonly()
				case UnannotatedTmt() => Polyread()
				case ErrorTmt()   => ErrorTmt()
			}
			case minpolyread() => modifier match {
				case Mutable()     => minpolyread()
				case Polyread()    => minpolyread()
				case Readonly()    => Readonly()
				case UnannotatedTmt() => minpolyread()
				case ErrorTmt()   => ErrorTmt()
			}
			case Readonly()    => Readonly()
			case UnannotatedTmt() => UnannotatedTmt()
			case ErrorTmt()   => ErrorTmt()
		}
		def mutabilityOnTotalApplication(tmt: Tmt) = tmt match {
			case Mutable()  => Mutable()
			case Polyread() => modifier match {
				case Mutable()     => Mutable()
				case Polyread()    => Polyread()
				case Readonly()    => Readonly()
				case UnannotatedTmt() => UnannotatedTmt()
				case ErrorTmt()   => ErrorTmt()
			}
			case minpolyread() => modifier match {
				case Mutable()     => Polyread()
				case Polyread()    => Polyread()
				case Readonly()    => Readonly()
				case UnannotatedTmt() => Polyread()
				case ErrorTmt()   => ErrorTmt()
			}
			case Readonly()    => Readonly()
			case UnannotatedTmt() => UnannotatedTmt()
			case ErrorTmt()   => ErrorTmt()
		}
		res match {
			case mt: MethodType =>
				modifyFinalResultType(mt, mutabilityOnPartialApplication(tmt(mt.finalResultType)))
			case _ =>
				// if the result is a Function class, then use the partial application mutability
				if (ctx.definitions.isFunctionType(res))
					withSimpleMutability(res, mutabilityOnPartialApplication(tmt(res)))
				else
					withSimpleMutability(res, mutabilityOnTotalApplication(tmt(res)))
		}
	}*/


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
	The idea of @polyread is that @polyread will be adapted to either @mutable or @readonly (or UnannotatedTmt),
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