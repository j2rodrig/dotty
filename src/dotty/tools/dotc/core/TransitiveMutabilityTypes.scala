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
import Symbols._
import util.Positions._
import Types._
import typer.ProtoTypes._
import printing.Texts.Text

object TransitiveMutabilityTypes {

	/**
	 * The Tmt trait is a base trait of all simple Transitive Mutability Types.
	 *
	 * A simple TMT is normally just a @readonly, @polyread, or @mutable (or error).
	 * It does not contain any information about generic type members or method parameters.
	 */
	trait Tmt {
	
		def <:<(other: Tmt): Boolean = tmtSubtypeOf(this, other)
		
		def isReadonly = this.isInstanceOf[Readonly]
		def isPolyread = this.isInstanceOf[Polyread]
		def isMutable = this.isInstanceOf[Mutable]
		def isUnannotated = this.isInstanceOf[UnannotatedTmt]
		def isError = this.isInstanceOf[ErrorTmt]
	
		def exists = isReadonly || isPolyread || isMutable
	}
	
	case class Readonly() extends Tmt {
		override def toString = "@readonly"
	}
	
	case class Polyread(val origin: Symbol, val adaptations: Tmt) extends Tmt {
		override def toString(): String = {
			val orig = if (origin eq NoSymbol) "NoSymbol" else s"${origin}"
			if (adaptations.exists) s"@polyread($orig,adapted with $adaptations)"
			else s"@polyread($orig)"
		}
	}
	
	case class Mutable() extends Tmt {
		override def toString = "@mutable"
	}
	
	/** The mutability of a type where no mutability has been specified. */
	case class UnannotatedTmt() extends Tmt {
		override def toString = "<unannotated TMT>"
	}
	
	/** The mutability of an ErrorType. */
	case class ErrorTmt() extends Tmt {
		override def toString = "<TMT Error Type>"
	}
	
	//ctx.warning(s"No origin set for this @polyread annotation", annot.tree.pos)



	//--- Conversions to/from Scala Annotations ---//

	/**
	 * TmtAnnotation is a ConcreteAnnotation that carries a simple TMT.
	 * Some TMTs, specifically Polyread, contain more information than is expressible
	 * by a raw concrete annotation. For example, a "raw" @polyread annotation found
	 * in the source code is intrepreted as Polyread(NoSymbol,UnannotatedTmt()),
	 * but internally, it is possible to create a concrete annotation that contains
	 * a Polyread with other parameters.
	 */
	class TmtAnnotation(tree: Tree, val tmt: Tmt) extends ConcreteAnnotation(tree) {
		override def toText(implicit ctx: Context): Text = tmt.toString  // change how the annotation is displayed
	}

	/**
	 * Finds the TMT of the given annotation.
	 */
	def tmt(annot: Annotation)(implicit ctx: Context): Tmt =
		annot match {
			case annot: TmtAnnotation => annot.tmt   // This annotation carries TMT information.
			case _ =>  // get TMT based on annotation class.
				if (annot matches ctx.definitions.ReadonlyClass) Readonly()
				else if (annot matches ctx.definitions.PolyreadClass) Polyread(NoSymbol,UnannotatedTmt())
				else if (annot matches ctx.definitions.MutableClass) Mutable()
				else UnannotatedTmt()
		}

	/**
	 * Creates a new annotation representing the given TMT.
	 */
	def toAnnot(tm: Tmt)(implicit ctx: Context): Annotation =
		new TmtAnnotation(
			New (
				tm match {
					case tm: Mutable  => ctx.definitions.MutableClass.typeRef
					case tm: Polyread => ctx.definitions.PolyreadClass.typeRef
					case tm: Readonly => ctx.definitions.ReadonlyClass.typeRef
					//case tm: ErrorTmt => ctx.definitions.ReadonlyClass.typeRef  // we don't have an @error annotation, so just use @readonly
				},
				Nil
			),
			tm)


	//--- Querying/Modifying the TMT of Scala Types ---//

	/**
	 * Finds the TMT of the given Scala type.
	 * 
	 * In general, the outermost TMT annotation overrides any inner annotations.
	 * E.g., the type T @readonly @mutable is equivalent to T @mutable.
	 * This overriding behaviour allows the TMT of any Scala type to be overridden at will,
	 * by the programmer or by the compiler.
	 *
	 *  AnnotatedType - if it represents a mutability annotation, the annotation's TMT is returned.
	 *  AndType/OrType - returns GLB/LUB.
	 *  TypeProxy - returns TMT of the underlying type. Originally, there was a special case
	 *              for TermRef types, but they are now handled automatically because calling
	 *              underlying() calls info() which calls the termRef() adaptation method.
	 *  MethodType/ExprType/PolyType - returns the TMT of the result.
	 *  ErrorType - returns ErrorTmt.
	 *  all other types - by definition, these are all Ground types without any TMT annotations.
	 */
	def tmt(tp: Type)(implicit ctx: Context): Tmt =
		tp.finalResultType match {
			case AnnotatedType(annot, underlying) =>
				val tm = tmt(annot)
				if (tm.exists) tm else tmt(underlying)  // skip non-TMT annotations
			case tp @ AndType(tp1, tp2) => glb(tmt(tp1), tmt(tp2))
			case tp @ OrType(tp1, tp2) => lub(tmt(tp1), tmt(tp2))
			case tp: TypeProxy => tmt(tp.underlying)
			case tp: ErrorType => ErrorTmt()
			case _ => UnannotatedTmt()
		}

	/**
	 * Finds the immediate TMT of the given Scala type.
	 * 
	 * Unlike tmt() above, immTmt() only looks for immediately-present annotations.
	 * For example:
	 *   type T = AnyRef @readonly
	 *   val t: T
	 * tmt() will return Readonly for the type of t, and immTmt() will return UnannotatedTmt.
	 *
	 * Use case: For TypeBounds declarations, immTmt is used to determine if the programmer
	 * placed any annotations directly on the lower or upper bounding type. See the type
	 * assignment rules for TypeBounds.
	 */
	def immTmt(tp: Type)(implicit ctx: Context): Tmt =
		tp.finalResultType match {
			case AnnotatedType(annot, underlying) =>
				val tm = tmt(annot)
				if (tm.exists) tm else tmt(underlying)  // skip non-TMT annotations
			case tp: ErrorType => ErrorTmt()
			case _ => UnannotatedTmt()
		}
	
	/**
	 * Returns a version of the given type with the given TMT.
	 *
	 * For MethodType/ExprType/PolyType, returns a copy of the type where the result has
	 * the given TMT.
	 *
	 * Does not attempt to add annotations to ErrorTypes or add ErrorTmts to any type.
	 *
	 * By default, any immediate TMT annotations on the type are removed before adding
	 * the new given TMT. To prevent this removal, set removeExisting to false.
	 */
	def withTmt(tp: Type, tm: Tmt, removeExisting: Boolean = true)(implicit ctx: Context): Type = tp match {
	
		case tp @ MethodType(paramNames, paramTypes) =>
			tp.derivedMethodType(paramNames, paramTypes, withTmt(tp.resultType, tm, removeExisting))
		
		case tp: ExprType =>
			tp.derivedExprType(withTmt(tp.resultType, tm, removeExisting))
		
		case tp @ PolyType(paramNames) =>
			tp.derivedPolyType(paramNames, tp.paramBounds, withTmt(tp.resultType, tm, removeExisting))
		
		case AnnotatedType(annot, underlying) if removeExisting =>
			if (tmt(annot).exists) withTmt(underlying, tm, true)  // existing top-level TMT annotations are removed
			else new AnnotatedType(annot, withTmt(underlying, tm, true))  // but preserve non-TMT annotations
		
		case _ if tp.isError => tp  // don't add anything to an ErrorType
		
		case _ if tm.isError => tp  // don't add an ErrorTmt to anything
		
		case _ if tm.exists => new AnnotatedType(toAnnot(tm), tp)   // if the given tm exists, then add it
		
		case _ if tmt(tp).exists =>
			// Trying to add an UnannotatedTmt to an annotated type.
			new AnnotatedType(toAnnot(Mutable()), tp)  // add @mutable to mask underlying annotations
		
		case _ => tp  // tp and tmt are both unannotated -- no action needed
	}
	

	//--- Type Assignment/Adaptation ---//

	/**
	 * Changes the type returned from a TermRef's info method.
	 * A TermRef is created wherever a field or method is selected, but may also be created
	 * wherever another TermRef's denotations are re-wrapped. Instead of attempting to
	 * insert a viewpoint adapation wherever a TermRef is created, the adaptation is
	 * simply inserted at the TermRef's info method.
	 * The TermRef's denotation(s) are not changed directly, but instead the TermRef's info
	 * method and underlying method reflect the adaptation operation.
	 * 
	 * In particular, field selection must be adapted to take the prefix type into account.
	 * Methodic types are adapated elsewhere: they are only adapted on application,
	 * and only if they have @polyread return types.
	 */
	def termRef(info: Type, tRef: TermRef)(implicit ctx: Context): Type =
		if (info.isInstanceOf[MethodicType]) info             // methods: no adapation needed here
		else withTmt(info, lub(tmt(tRef.prefix), tmt(info)))  // fields: use Least Upper Bound of info and the prefix type
	
	/**
	 * Changes the type returned from a TypeRef's info method.
	 * 
	 * No adapation is needed here.
	 */
	def typeRef(info: Type, tRef: TypeRef)(implicit ctx: Context): Type = info
	
	


	//--- Simple Subtyping Relationships ---//

	/**
	 * Least upper bound.
	 */
	def lub(tmt1: Tmt, tmt2: Tmt): Tmt =
		if (tmt1.isError || tmt2.isError) ErrorTmt()
		else if (tmt1.isReadonly || tmt2.isReadonly) Readonly()
		else if (tmt1.isPolyread && tmt2.isPolyread) {
			if (tmt1.asInstanceOf[Polyread].origin equals tmt2.asInstanceOf[Polyread].origin) tmt1      // polyread types are equal if same origin
			else Readonly()     // if different origin, conservatively return readonly
		}
		else if (tmt1.isPolyread) tmt1   // any polyread dominates a mutable
		else if (tmt2.isPolyread) tmt2
		else if (tmt1.isMutable || tmt2.isMutable) Mutable()
		else UnannotatedTmt()
	
	/**
	 * Greatest lower bound.
	 */
	def glb(tmt1: Tmt, tmt2: Tmt): Tmt =
		if (tmt1.isError || tmt2.isError) ErrorTmt()
		else if (!tmt1.exists || !tmt2.exists) UnannotatedTmt()  // preserve unannotated-ness
		else if (tmt1.isMutable || tmt2.isMutable) Mutable()
		else if (tmt1.isPolyread && tmt2.isPolyread) {
			if (tmt1.asInstanceOf[Polyread].origin equals tmt2.asInstanceOf[Polyread].origin) tmt1      // polyread types are equal if same origin
			else Mutable()     // if different origin, conservatively return mutable
		}
		else if (tmt1.isPolyread) tmt1   // any polyread is below readonly
		else if (tmt2.isPolyread) tmt2
		else Readonly()
	
	/**
	 * Simple TMT subtype relation. Returns true if tmt1 <:< tmt2.
	 *
	 * Considers Mutable =:= UnannotatedTmt.
	 * Polyread types are only considered equal if their origins are the same.
	 *
	 * Returns true if either tmt1 or tmt2 is an ErrorTmt. This is done to prevent generation
	 * spurious errors when some other type error has already occurred.
	 */
	def tmtSubtypeOf(tmt1: Tmt, tmt2: Tmt): Boolean =
		if (tmt1.isError || tmt2.isError) true
		else
			tmt2 match {
				case tmt2: Readonly => true
				case tmt2: Polyread => tmt1 match {
					case tmt1: Readonly => false
					case tmt1: Polyread => (tmt1.origin == tmt2.origin // Polyreads are only equal if their origins are equal.
									 || tmt2.origin == NoSymbol)   // Or if tmt2's origin has not been set --
									                               //  this can happen when Function types are created.
																   //  Should be OK provided we don't try to instantiate
																   //  an object with a non-origin polyread type.
					case _ => true
				}
				case tmt2: Mutable => !tmt1.isReadonly && !tmt1.isPolyread
				case tmt2: UnannotatedTmt => !tmt1.isReadonly && !tmt1.isPolyread
			}




	//--- Full Subtyping Relationships ---//

	/**
	 * Returns false if tp1 is not a subtype of tp2, with respect to Transitive Mutability.
	 *
	 * alreadySeen contains types that have already been considered by refinementSubtypeOf.
	 *
	 * tmtSubtypeOf does not attempt to determine if tp1's Scala type is a subtype of
	 * tp2's Scala type. tmtSubtypeOf only returns false if a TMT violation is detected.
	 * 
	 */
	def tmtSubtypeOf(tp1: Type, tp2: Type, alreadySeen: List[Type])(implicit ctx: Context): Boolean =
		tmtSubtypeOf(tmt(tp1), tmt(tp2)) &&   // check simple TMT
		refinementSubtypeOf(tp1, tp2, alreadySeen).result  // check type refinements & methods

	def tmtSubtypeOf(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean =
		tmtSubtypeOf(tp1, tp2, List())
	
	/**
	 * A SubtypeResult holds the result of a subtype comparison.
	 * It may also hold other information that more precisely specifies what happened during the comparision.
	 * The result member is true if the comparision succeeded, false otherwise.
	 */	
	trait SubtypeResult {
		def result: Boolean
	}
	case class AcceptedSubtype() extends SubtypeResult {
		def result = true
	}
	trait FailedSubtype extends SubtypeResult {
		def result = false
	}
	case class FailedParameterSubtype(val mt1: MethodType, val pIndex1: Int,
			val mt2: MethodType, val pIndex2: Int) extends FailedSubtype {
	}
	case class FailedRefinementSubtype(val refinedName: Name, val info1: Type, val parent1: Type,
		val info2: Type, val variance2: Int) extends FailedSubtype {
	}
	
	/**
	 * A TMT subtype check that returns an error string with specific failure information.
	 */
	def tmtExplainingSubtypeOf(tp1: Type, tp2: Type, location1: String, location2: String, alreadySeen: List[Type])(implicit ctx: Context): String = {
		val (tm1, tm2) = (tmt(tp1), tmt(tp2))
				
		val givenLocations = (location1 ne "") && (location2 ne "")
	
		if (!tmtSubtypeOf(tm1, tm2)) {    // check simple TMT
			if (!tp1.isInstanceOf[MethodicType])
				tmtMismatchStr(tm1, tm2, (if (givenLocations) s" $location1 does not match $location2:" else ""))
			else
				tmtMismatchStr(tm1, tm2, (if (givenLocations) s" result of $location1 does not match $location2:" else ""))
		}

		else refinementSubtypeOf(tp1, tp2, alreadySeen) match {  // check type refinements & methods

			case FailedParameterSubtype(
				MethodType(paramNames1, paramTypes1), pIndex1,
				MethodType(paramNames2, paramTypes2), pIndex2)
				=>
				tmtMismatchStr(
					paramTypes1(pIndex1), paramTypes2(pIndex2),
					(if (givenLocations)
						s" parameter ${paramNames1(pIndex1)} of $location1 " +
						s"does not match overridden parameter of $location2:"
					 else
						s" parameter ${paramNames1(pIndex1)} does not match overridden parameter:")
					)
			
			case FailedRefinementSubtype(name, info1, parent1, info2, variance2) =>
				val varStr = variance2 match {
					case 0 => "invariant"
					case 1 => "covariant"
					case -1 => "contravariant"
				}
				tmtMismatchStr(info1, info2,
					s" in $varStr type parameter $name of ${parent1.show}:")

			case AcceptedSubtype() => ""
		}
	}
	
	def tmtExplainingSubtypeOf(tp1: Type, tp2: Type, location1: String, location2: String)(implicit ctx: Context): String =
		tmtExplainingSubtypeOf(tp1, tp2, location1, location2, List())

	def tmtExplainingSubtypeOf(tp1: Type, tp2: Type)(implicit ctx: Context): String =
		tmtExplainingSubtypeOf(tp1, tp2, "", "")

	/**
	 * Checks named type members for compatibility.
	 * Also checks for compatibility of method parameters and result types.
	 */
	private[this] def refinementSubtypeOf(tp1: Type, tp2: Type, _alreadySeen: List[Type])(implicit ctx: Context): SubtypeResult = {
		if (_alreadySeen.exists { t => t eq tp1}) return AcceptedSubtype()   // stop recursing if we've already seen tp1
		val alreadySeen = tp1 :: _alreadySeen
	
		tp1 match {
		
			case tp1 @ MethodType(paramNames1, paramTypes1) =>
				reduceToMethodOrExpr(tp2) match {
					case tp2 @ MethodType(paramNames2, paramTypes2) =>
						/**
						 * Check each parameter in method tp1 against the same-indexed parameter in method tp2.
						 * tp2's parameters should be <:< tp1's parameters (parameters are contravariant).
						 */
						var r: SubtypeResult = AcceptedSubtype()
						(paramTypes1 zip paramTypes2) forall { case (tpParam1, tpParam2) =>
							if (!tmtSubtypeOf(tpParam2, tpParam1)) {   // check tpParam2 <:< tpParam1. (parameters are contravariant)
								r = FailedParameterSubtype(
									tp1, paramTypes1.indexOf(tpParam1),
									tp2, paramTypes2.indexOf(tpParam2))
								false
							} else true
						}
						r
					case tp2: ExprType =>  // Can't check parameters (since there are none)
						refinementSubtypeOf(tp1, tp2.resultType, alreadySeen)  // but try to check tp1 against result type
					case _ =>
						refinementSubtypeOf(tp1.resultType, tp2, alreadySeen)  // otherwise, try to check tp1's result type against tp2
				}
			
			case tp1: PolyType => refinementSubtypeOf(tp1.resultType, tp2, alreadySeen)
			
			case tp1: ExprType => refinementSubtypeOf(tp1.resultType, tp2, alreadySeen)
			
			case tp1: RefinedType =>
				val refinedInfo = ctx.typeComparer.normalizedInfo(tp1)
				val sr = namedTypeMemberSubtype(tp1.parent, refinedInfo, tp2, tp1.refinedName, alreadySeen)
				
				if (!sr.result) sr
				else refinementSubtypeOf(tp1.parent, tp2, alreadySeen)   // type refinement checks out OK. Now check parent type
			
			// TODO: special case for TypeBounds?
				
			case AndType(tp11, tp12) =>
				val r1 = refinementSubtypeOf(tp11, tp2, alreadySeen)
				if (!r1.result) r1 else refinementSubtypeOf(tp12, tp2, alreadySeen)
	
			case OrType(tp11, tp12) =>
				val r1 = refinementSubtypeOf(tp11, tp2, alreadySeen)
				if (r1.result) r1 else refinementSubtypeOf(tp12, tp2, alreadySeen)
	
			case tp1: TypeProxy =>
				refinementSubtypeOf(tp1.underlying, tp2, alreadySeen)  // recursively examine other proxy types
			
			case _ => AcceptedSubtype()  // ignore underlying structure of other ground types
		}
	}
	
	/**
	 * Checks whether info1 is a subtype of the named member of tp2.
	 * Returns FailedSubtype if definitely not a subtype, AcceptedSubtype otherwise.
	 */
	private[this] def namedTypeMemberSubtype(parent1: Type, info1: Type, tp2: Type, name2: Name, alreadySeen: List[Type])(implicit ctx: Context): SubtypeResult = {
		tp2 match {
			case tp2: RefinedType =>
				if (tp2.refinedName == name2 && tp2.refinedName.isTypeName) {
					val info2 = ctx.typeComparer.normalizedInfo(tp2)
					if (tmtSubtypeWithVariance(info1, info2, varianceOf(info2), alreadySeen))
						AcceptedSubtype()
					else
						FailedRefinementSubtype(name2, info1, parent1, info2, varianceOf(info2))
				}
				else namedTypeMemberSubtype(parent1, info1, tp2.parent, name2, alreadySeen)

			case tp2: ClassInfo =>
				tp2.cls.findMember(name2, tp2.cls.thisType, EmptyFlags).alternatives foreach { denot2 =>
					if (denot2.isType) {
						if (!tmtSubtypeWithVariance(info1, denot2.info, varianceOf(denot2.info), alreadySeen))
							return FailedRefinementSubtype(name2, info1, parent1, denot2.info, varianceOf(denot2.info))
					}
				}
				AcceptedSubtype()
			
			// TODO: special case for TypeBounds?
			
			case AndType(tp21, tp22) =>
				val r1 = namedTypeMemberSubtype(parent1, info1, tp21, name2, alreadySeen)
				if (!r1.result) r1 else namedTypeMemberSubtype(parent1, info1, tp22, name2, alreadySeen)
			
			case OrType(tp21, tp22) =>
				val r1 = namedTypeMemberSubtype(parent1, info1, tp21, name2, alreadySeen)
				if (r1.result) r1 else namedTypeMemberSubtype(parent1, info1, tp22, name2, alreadySeen)
			
			case tp2: TypeProxy =>
				namedTypeMemberSubtype(parent1, info1, tp2.underlying, name2, alreadySeen)
			
			case _ => AcceptedSubtype()   // can't find a reason to reject the subtype
		}
	}
	
	
	/// On incremental development of the TMT type system:
	/// It is always safe for tmtSubtypeOf to return true.
	/// I return false whenever I find a case that is provably in violation of TMT constraints.
	/// This approach allows for incremental development.
	
	/// Possible topic to write about in a chapter:
	/// That I couldn't treat the existing type system as "black magic".
	/// Due to the complexity of the system, and my own limited knowledge of it,
	/// I couldn't easily take advantage of large parts of it...

	private[this] def varianceOf(tp: Type)(implicit ctx: Context): Int = tp match {
		case tp: TypeBounds => tp.variance
		case tp: TypeProxy => varianceOf(tp.underlying)
		case _ => 0
	}
	
	private[this] def tmtSubtypeWithVariance(tp1: Type, tp2: Type, variance2: Int, alreadySeen: List[Type])(implicit ctx: Context): Boolean = {
		// If covariant or invariant, check that tp1 is a subtype of tp2.
		val covOk = variance2 < 0 || tmtSubtypeOf(tp1, tp2, alreadySeen)
		// If contravariant or invariant, check that tp2 is a subtype of tp1.
		val contravOk = variance2 > 0 || tmtSubtypeOf(tp2, tp1, alreadySeen)
		// All variance constraints satisfied?
		covOk && contravOk
	}
	
	/**
	 * If tp is reducible to a MethodType or ExprType by widening PolyTypes and dealiasing,
	 * return the reduced tp.
	 */
	private[this] def reduceToMethodOrExpr(tp: Type)(implicit ctx: Context): Type = tp.dealias match {
		case tp: MethodType => tp
		case tp: ExprType => tp
		case tp: PolyType => reduceToMethodOrExpr(tp.resultType)
		case _ => tp
	}

	/**
	 * Check that an overriding symbol's type matches the overridden symbol's type.
	 * Issues an error if the overriding type is incompatible.
	 */
	def tmtCheckOverride(sym: Symbol, tp: Type, overriddenSym: Symbol, overriddenTp: Type)(implicit ctx: Context): Unit = {
		if (!tmtSubtypeOf(tp, overriddenTp))
			ctx.error(
				tmtExplainingSubtypeOf(
					tp, overriddenTp,
					s"${sym} in ${sym.owner}", s"${overriddenSym.name} in ${overriddenSym.owner}"),
				sym.pos)
	}

	
	//--- Error Reporting / Diagnostics ---//

	def tmtMismatch(tree: Tree, pt: Type)(implicit ctx: Context) =
		ctx.error(tmtMismatchStr(tree.tpe, pt, ""), tree.pos)

	def tmtMismatch(pos: Position, tp1: Type, tp2: Type)(implicit ctx: Context) =
		ctx.error(tmtMismatchStr(tp1, tp2, ""), pos)

	def tmtMismatchStr(found: Type, expected: Type, priorMessage: String)(implicit ctx: Context) =
		s"mutability mismatch:\n"+
		(if (priorMessage ne "") priorMessage + "\n" else "") +
		s" found   : ${found.show}\n"+
		s" required: ${expected.show}"

	def tmtMismatchStr(found: Tmt, expected: Tmt, priorMessage: String)(implicit ctx: Context) =
		s"mutability mismatch:\n"+
		(if (priorMessage ne "") priorMessage + "\n" else "") +
		s" found   : ${mutableIfUnannotated(found)}\n"+
		s" required: ${mutableIfUnannotated(expected)}"

	def mutableIfUnannotated(tm: Tmt) = if (tm.exists) tm else Mutable()

	/**
	 * Human-readable text representation of a type, with special formatting intended to
	 * show deep type structure in a reasonably compact and readable way.
	 * Aliases are followed, up to a maximum depth specified by layers.
	 * If this method breaks a line, the new line is indented by the indents amount.
	 */
	def showSpecial(tp: Type, indents: Int = 1, layers: Int = 20)(implicit ctx: Context): String = {
		def indent(indents: Int = 0): String =
			if (indents > 0) "    " + indent(indents-1)
			else ""

		if (layers <= 0) "..."
		else tp match {

			case tp @ TypeRef(prefix, name) =>
				if (tp.underlying.isInstanceOf[ClassInfo])
					s"TypeRef($name)"
				else
					s"TypeRef($name => ${showSpecial(tp.underlying,indents,layers-1)})"
		
			case tp @ TermRef(prefix, name) =>
				s"TermRef(${showSpecial(prefix,indents)}.$name => ${showSpecial(tp.underlying,indents,layers-1)})"
	
			case tp @ ThisType(cls) =>
				s"this(${cls.name})"
		
			case tp @ SuperType(thistpe,supertpe) =>
				s"super(${thistpe.asInstanceOf[ThisType].cls.name} => ${showSpecial(supertpe,indents,layers-1)})"
	
			case ConstantType(value) =>
				value.toString
	
			case tp: LazyRef =>
				s"LazyRef(${showSpecial(tp.underlying,indents,layers-1)})"
	
			case rt @ RefinedType(parent, refinedName) =>
				s"RefinedType(${showSpecial(parent,indents+1,layers-1)}" + " {\n" +
				indent(indents) + s"${refinedName} => ${showSpecial(rt.refinedInfo,indents+1,layers-1)}\n" +
				indent(indents-1) + "})"
	
			case mt @ MethodType(paramNames, paramTypes) =>
				var (sig, i) = ("", 0)
				while (i < paramNames.size) {
					if (i > 0) sig = sig + ",\n"+indent(indents)
					sig = sig + s"${paramNames(i)}: ${showSpecial(paramTypes(i),indents+1,layers-1)}"
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
					if (i > 0) sig = sig + ",\n"+indent(indents)
					sig = sig + s"${paramNames(i)}: ${showSpecial(pt.paramBounds(i),indents+1,layers-1)}"
					i = i + 1
				}
				val ret = showSpecial(pt.resultType, indents+1)
				s"PolyType(\n" +
					(if (i > 0) indent(indents) + s"$sig\n" else "") +
					indent(indents) + s"=>\n" +
					indent(indents) + s"$ret\n" +
					indent(indents-1) + s")"
			
			case ExprType(resultType) =>
				s"ExprType(${showSpecial(resultType,indents,layers-1)})"
			
			case IgnoredProto(ignored) =>
				s"IgnoredProto(${showSpecial(ignored,indents,layers-1)})"
			
			case SelectionProto(name, memberProto, compat) =>
				s"SelectionProto($name, ${showSpecial(memberProto,indents,layers-1)})"
		
			case FunProto(args, resultType, typer) =>
				s"FunProto(\n" +
				(if (args.size > 0) indent(indents) + s"${args mkString "\n"}\n" else "") +
				indent(indents) + s"=>\n" +
				indent(indents) + s"${showSpecial(resultType,indents+1,layers-1)}\n" +
				indent(indents-1) + s")"
			
			case ViewProto(argType, resultType) =>
				s"ViewProto(\n" +
				indent(indents) + s"${showSpecial(argType,indents+1,layers-1)}\n" +
				indent(indents) + s"=>\n" +
				indent(indents) + s"${showSpecial(resultType,indents+1,layers-1)}\n" +
				indent(indents-1) + s")"
		
			case PolyProto(targs, resultType) =>
				var (sig, i) = ("", 0)
				while (i < targs.size) {
					if (i > 0) sig = sig + ",\n"+indent(indents)
					sig = sig + s"${showSpecial(targs(i),indents+1,layers-1)}"
					i = i + 1
				}
				s"PolyProto(\n" +
				(if (targs.size > 0) indent(indents) + s"$sig\n" else "") +
				indent(indents) + s"=>\n" +
				indent(indents) + s"${showSpecial(resultType,indents+1,layers-1)}\n" +
				indent(indents-1) + s")"
		
			case tp @ PolyParam(binder, paramNum) =>
				s"PolyParam(${binder.paramNames(paramNum)} => ${showSpecial(tp.underlying,indents,layers-1)})"
		
			case RefinedThis(binder) =>
				s"RefinedThis(${showSpecial(binder,indents,layers-1)})"
			
			case tp: TypeVar =>
				s"TypeVar(\n" +
				indent(indents) + s"${showSpecial(tp.origin,indents+1,layers-1)}\n" +
				(if (tp.inst.exists)
					indent(indents) + s"=>\n" + indent(indents) + s"${showSpecial(tp.inst,indents+1,layers-1)}\n") +
				indent(indents-1) + s")"
			
			case tp @ ClassInfo(prefix, cls, parents, decls, selfInfo) =>
				s"${cls.name}"
			
			case tp @ TypeBounds(lo, hi) =>
				val variance =
					if (tp.variance == 0) ""
					else if (tp.variance == 1) "<cov> "
					else "<contra> "
				if (lo eq hi) s"TypeAlias($variance${showSpecial(hi,indents,layers-1)})"
				else s"$variance${showSpecial(lo,indents,layers-1)} to ${showSpecial(hi,indents,layers-1)}"
			
			case AnnotatedType(annot, underlying) =>
				val tm = tmt(annot)
				val text = if (tm.isUnannotated) s"<non-TMT>@${annot.toString}" else tm.toString
				text + s"(${showSpecial(underlying,indents,layers-1)})"
			
			case tp: ImportType =>
				s"ImportType"
			
			case WildcardType(bounds) =>
				s"WildcardType(${showSpecial(bounds,indents,layers-1)})"
			
			case _ =>
				if (tp eq NoType) "NoType"
				else if (tp eq NoPrefix) "NoPrefix"
				else if (tp eq ErrorType) "ErrorType"
				else
					tp.toString
		}
	}
}