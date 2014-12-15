package dotty.tools
package dotc
package core

import util.common._
import ast._
import tpd._
import ast.untpd
import Trees.{Apply, Typed, ValDef, DefDef, TypeDef, Literal, SeqLiteral}
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

// Another use for purity:
// Issuing a warning when calling a pure method but not using the result.


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
	 * If the given class symbol derives from a TMT annotation class, the symbol's Tmt type.
	 * Otherwise, UnannoatedTmt().
	 */
	def tmt(sym: Symbol)(implicit ctx: Context): Tmt =
		if (sym.derivesFrom(ctx.definitions.ReadonlyClass)) Readonly()
		else if (sym.derivesFrom(ctx.definitions.PolyreadClass)) Polyread(NoSymbol,UnannotatedTmt())
		else if (sym.derivesFrom(ctx.definitions.MutableClass)) Mutable()
		else UnannotatedTmt()

	/**
	 * Finds the TMT of the given annotation.
	 */
	def tmt(annot: Annotation)(implicit ctx: Context): Tmt =
		annot match {
			case annot: TmtAnnotation => annot.tmt   // This annotation carries TMT information.
			case _ => tmt(annot.symbol)  // get TMT based on annotation class.
				//if (annot matches ctx.definitions.ReadonlyClass) Readonly()
				//else if (annot matches ctx.definitions.PolyreadClass) Polyread(NoSymbol,UnannotatedTmt())
				//else if (annot matches ctx.definitions.MutableClass) Mutable()
				//else UnannotatedTmt()
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
				},
				List(EmptyTree)
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
			//case tp: TermRef => lub(tmt(tp.prefix), tmt(tp.info), tp.tmt())
			//case tp: TypeRef => lub(tmt(tp.info), tp.tmt())
			//case tp: ThisType => lub(tmt(tp.underlying), tp.tmt)
			//case tp: SuperType => lub(tmt(tp.thistpe), tmt(tp.underlying))
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
	

	//----- Outer parameters -----//
	
	// There are 2 flavours of outer parameters:
	// - term-type parameters. (These are variables in the symbol's environment.)
	// - receiver-type parameters. (These include "this" and "super".)
	
	// TO TRY:
	// - register symbols with annotation info (map)
	// - register untyped trees with annotations (stack)
	// -> to look up: if any tree in current context matches head of stack, then use stack to look up annotation info.
	//    else: look up annotations on given symbol.
	
	// Stack of contextual annotations. _1 is the untyped tree, _2 is the typed annotations on that tree.
	private[this] var contextAnnots = List[(untpd.Tree, List[(Tmt,Type,Tree)])]()
	//private[this] var contextAnnots = List[(untpd.Tree, List[Tree])]()
	
	// The annotations in this list get ignored.
	private[this] var currentlyTypingModifiersOf = List[untpd.Tree]()
	
	/** Adds any annotations on the given tree to the contextual annotations stack. */
	def typedPush(tree: untpd.Tree, pt: Type)(implicit ctx: Context): Unit = {
	/*	tree match {
			case mdef: untpd.MemberDef =>
				val annotations1 = untpd.modsDeco(mdef).mods.annotations mapconserve ctx.typer.typedAnnotation
				val listOfList = annotations1 map (typedAnnotations _)
				contextAnnots = (mdef, listOfList.flatten) :: contextAnnots
				//annotations1 foreach (checkAnnotation _)
				//contextAnnots = (mdef, annotations1) :: contextAnnots
				//contextAnnots = (mdef, annotations1) :: contextAnnots
			case _ =>
		}*/
	}

	/** Removes the most recent item from the contextual annotations stack. */	
	def typedPop(tree: untpd.Tree, tree1: Tree)(implicit ctx: Context): Unit = {
	/*	tree match {
			case mdef: untpd.MemberDef =>
				assert(mdef eq contextAnnots.head._1)
				contextAnnots = contextAnnots.tail
			case _ =>
		}*/
	}
	
	/** Causes the annotations on the given tree to be ignored temporarily. */
	def onTypedModifiersBegin(tree: untpd.Tree)(implicit ctx: Context): Unit = {
	//	currentlyTypingModifiersOf = tree :: currentlyTypingModifiersOf
	}
	
	/** Stops ignoring the annotations on the most recently-ignored tree. */
	def onTypedModifiersEnd(tree: untpd.Tree)(implicit ctx: Context): Unit = {
	//	assert(tree eq currentlyTypingModifiersOf.head)
	//	currentlyTypingModifiersOf = currentlyTypingModifiersOf.tail
	}
	
	/**
	 * Returns a list of outer-parameter annotations that are valid in the current context.
	 * The returned list includes arguments' TMTs, types, and trees.
	 *
	 * This method is separate from tmtAnnotationsOnSymbol --
	 *
	 *   tmtAnnotationsInCurrentContext is necessary where RHS definitions are typed before
	 *   their assigned symbols. E.g.:
	 *
	 *     @readonly(this) def m() = { this }
	 *
	 *   Because m does not have a specified result type, the body { this } is typed first,
	 *   followed by m and @readonly(this). Unfortunately, the TMT of this is required to
	 *   type the body correctly. To get around this problem, a preliminary annotation typing
	 *   is performed before the body is typed (and possibly before the symbol m is completed).
	 *   See typedPush and typedPop.
	 */


	/*private[this] def testing(implict ctx: Context): List[(Tmt,Type,Tree)] =
		if (ctx eq NoContext)
			return List()
		else
			return ctx.tree match {
				case mdef: MemberDef[Untyped] =>
					if (currentlyTypingModifiersOf contains mdef) testing(ctx.outer)  // skip modifiers that are currently being typed
					else {
						val annotations1 = untpd.modsDeco(mdef).mods.annotations mapconserve ctx.typer.typedAnnotation
						val listOfList = annotations1 map (typedAnnotations _)
						listOfList.flatten ::: testing(ctx.outer)
					}
				case mdef: MemberDef[Type] =>
					val annotations1 = mdef.mods.annotations
					val listOfList = annotations1 map (typedAnnotations _)
					listOfList.flatten ::: testing(ctx.outer)
				case _ => testing(ctx.outer)
			}*/
	 



	private[this] def tmtAnnotationsInCurrentContext(implicit ctx: Context): List[(Tmt,Type,Tree)] = {
		tmtAnnotationsInGivenContext(contextAnnots)  // start with last-pushed element
	}
	def checkIdent(tree: Ident)(implicit ctx: Context) = {
		/*println(s"CHECK IDENT: $tree")
		tree.tpe match {
			case methRef: TermRef if methRef.underlying.isInstanceOf[MethodicType] => 
				val rcvMut = tmt(methRef.prefix)
				val (formalRcvMut, formalRcvTree) =
					receiverTmtInSymbolContext(methRef.denot.symbol.enclosingClass, methRef.denot.symbol)
				println(s"IDENT APPLY: this=$rcvMut  formal on ${methRef.denot.symbol}=$formalRcvMut")
	
				// if formal receiver type is polyread, then set the result adaptation to the receiver argument
				//if (formalRcvMut.isPolyread) resultModifier = rcvMut  // TODO: warning if resultType is not @polyread
	
				// otherwise check that the receiver argument is compatible with formal receiver
				//else
					if (!tmtSubtypeOf(rcvMut, formalRcvMut))
						ctx.error(s"""incompatible "this" mutability:\n""" +
							s" found   : ${mutableIfUnannotated(rcvMut)}\n"+
							s" required: ${mutableIfUnannotated(formalRcvMut)}",
							tree.pos)
			case _ =>
	
		}*/
	}
	def checkApply(tree: Apply[Type])(implicit ctx: Context) = {
		//println(s"CHECK APPLY: $tree")
	}
	private[this] def tmtAnnotationsInGivenContext(treeList: List[(untpd.Tree, List[(Tmt, Type, Tree)])])(implicit ctx: Context): List[(Tmt,Type,Tree)] = {
		var r = List[(Tmt, Type, Tree)]()
		treeList foreach { case (hostTree, knownAnnots) =>
			if (!(currentlyTypingModifiersOf contains hostTree)) {   // make sure this annotation is not currently being typed
				r = knownAnnots ::: r
				//knownAnnots foreach { case (tm, argTp, arg) =>
				//}
				/*annotTrees foreach { annTree =>
					//val Apply(meth,args) = annTree
					//val tm = if (annTree.tpe.isInstanceOf[NamedType]) tmt(annTree.tpe.asInstanceOf[NamedType].symbol) else UnannotatedTmt()
					//println(s"ANNOT TPE: ${showSpecial(annTree.tpe)}")
					val tm = tmt(annTree.symbol.owner)
					if (tm.exists) getArguments(annTree) foreach { arg =>
						r = (tm, arg.tpe, arg) :: r
					}
				}*/
			}
		}
		r.reverse
	}
	def typedAnnotations(annTree: Tree)(implicit ctx: Context): List[(Tmt,Type,Tree)] = {
		var r = List[(Tmt, Type, Tree)]()
		val tm = tmt(annTree.symbol.owner)
		if (tm.exists) getArguments(annTree) foreach { arg =>
			r = (tm, arg.tpe, arg) :: r
		}
		r
	}
	/// Finds the location in the context stack where the given symbol is defined. If not found, returns an empty stack.
	private[this] def findSymbolInCurrentContext(sym: Symbol)(implicit ctx: Context): List[(untpd.Tree, List[(Tmt, Type, Tree)])] =
		contextAnnots dropWhile { case (hostTree, _) =>
			hostTree match {
				case ValDef(name, _, _) => !(sym.name.isTermName && (name eq sym.name.asTermName))
				case DefDef(name, _, _, _, _) => !(sym.name.isTermName && (name eq sym.name.asTermName))
				case TypeDef(name, _) => !(sym.name.isTypeName && (name eq sym.name.asTypeName))
				case _ => true
			}
		}
	
	/**
	 * Returns a list of outer-parameter annotations that are valid in the context of the given symbol.
	 * The returned list includes arguments' TMTs, types, and trees.
	 */
	private[this] def tmtAnnotationsOnSymbol(sym: Symbol)(implicit ctx: Context): List[(Tmt,Type,Tree)] = {
		/*if (sym eq NoSymbol) List()
		else {
			//
			val currList = findSymbolInCurrentContext(sym)
			if (!currList.isEmpty) tmtAnnotationsInGivenContext(currList)
			else {
				var r = List[(Tmt, Type, Tree)]()
				sym.annotations foreach { annot =>
					//val tm = if (annot.tree.tpe.isInstanceOf[NamedType]) tmt(annot.tree.tpe.asInstanceOf[NamedType].symbol) else UnannotatedTmt()
					val tm = tmt(annot.tree.symbol.owner)
					if (tm.exists) getArguments(annot.tree) foreach { arg =>
						r = (tm, arg.tpe, arg) :: r
					}
				}
				r.reverse ::: tmtAnnotationsOnSymbol(sym.owner)
			}
		}*/
		List()
	}
	
	/** Gets a list of argument trees from the given annotation tree.
	 * The group number specifies which argument list is returned. */
	private[this] def getArguments(annTree: Tree, group: Int = 0): List[Tree] = {
		val arglist = ast.tpd.arguments(annTree)
		if (arglist.size > group) findArgTrees(arglist(group)) else List()
	}
	
	/** Maps an annotation argument tree to a list of specific arguments. */
	private[this] def findArgTrees(argTree: Tree): List[Tree] = argTree match {
		case SeqLiteral(elems) => elems
		case argTree: ProxyTree => findArgTrees(argTree.forwardTo)
		case argTree: DenotingTree => List(argTree)
		case x => List(x)
	}
	
	/** Performs some basic checks on the given annotation tree. */
	private[this] def checkAnnotation(annTree: Tree)(implicit ctx: Context): Unit = {
		val tm = tmt(annTree.symbol.owner)
		if (tm.exists) getArguments(annTree) foreach { arg =>
			arg.tpe match {
				case _: NamedType =>
					if (tm.isPolyread)
						ctx.error(s"@polyread can only have this or super references as arguments.", arg.pos)
				case _: ThisType =>
				case _: SuperType =>
				case _: RefinedThis =>
				case _ =>
					ctx.warning(s"Annotation argument has no effect: It does not specify a named value, object, or variable.", arg.pos)
			}
		}
	}
	
	/** Returns the outer-parameter TMT of the given symbol. Searches the current context. */
	def tmtInCurrentContext(sym: Symbol)(implicit ctx: Context): (Tmt, Tree) = {
		tmtAnnotationsInCurrentContext foreach { case (tm, argTp, arg) =>
			argTp match {
				case argTp: NamedType => argTp.denot.alternatives foreach { denot =>
					if (denot.symbol eq sym) return (tm, arg)
				}
				case _ =>
			}
		}
		(UnannotatedTmt(), EmptyTree)
	}
	/// A version of tmtInCurrentContext that finds the TMT of an arbitrary denotation.
	def tmtInCurrentContext(lookFor: Denotation)(implicit ctx: Context): Tmt = {
		var allTmt: Tmt = UnannotatedTmt()
		lookFor.alternatives foreach { sd =>
			allTmt = lub(allTmt, tmtInCurrentContext(sd.symbol)._1)
		}
		allTmt
	}
	/** Returns the outer-parameter TMT of the given symbol. Searches the given list of trees. */
	def tmtInGivenContext(sym: Symbol, treeList: List[(untpd.Tree, List[(Tmt, Type, Tree)])])(implicit ctx: Context): (Tmt, Tree) = {
		tmtAnnotationsInGivenContext(treeList) foreach { case (tm, argTp, arg) =>
			argTp match {
				case argTp: NamedType => argTp.denot.alternatives foreach { denot =>
					if (denot.symbol eq sym) return (tm, arg)
				}
				case _ =>
			}
		}
		(UnannotatedTmt(), EmptyTree)
	}
	
	/**
	 * Returns the outer-parameter TMT of the given symbol lookFor.
	 * Searches the context of the symbol ctxSym.
	 */
	def tmtInSymbolContext(lookFor: Symbol, ctxSym: Symbol)(implicit ctx: Context): (Tmt, Tree) = {
		tmtAnnotationsOnSymbol(ctxSym) foreach { case (tm, argTp, arg) =>
			argTp match {
				case argTp: NamedType => argTp.denot.alternatives foreach { denot =>
					if (denot.symbol eq lookFor) return (tm, arg)
				}
				case _ =>
			}
		}
		(UnannotatedTmt(), EmptyTree)
	}
	/// A version of tmtInSymbolContext that finds the TMT of an arbitrary denotation.
	def tmtInSymbolContext(lookFor: Denotation, ctxSym: Symbol)(implicit ctx: Context): Tmt = {
		var allTmt: Tmt = UnannotatedTmt()
		lookFor.alternatives foreach { sd =>
			allTmt = lub(allTmt, tmtInSymbolContext(sd.symbol, ctxSym)._1)
		}
		allTmt
	}
	
	/**
	 * Returns the outer-parameter TMT of "this" for the given class.
	 * An annotation parameter is considered to match when it is a supertype or
	 * subtype of the given class. (I.e., if the "this" classes have any inheritance relationship.)
	 * 
	 * Searches the current context.
	 */
	def receiverTmtInCurrentContext(cls: Symbol)(implicit ctx: Context): (Tmt,Tree) = {
		tmtAnnotationsInCurrentContext foreach { case (tm, argTp, arg) =>
			argTp match {
				case argTp: ThisType => if ((cls derivesFrom argTp.cls) || (argTp.cls derivesFrom cls)) return (tm,arg)
				case _ =>
			}
		}
		return (UnannotatedTmt(), EmptyTree)
	}
	
	/**
	 * Returns the outer-parameter TMT of "this" for the given class.
	 * Searches the context of the symbol ctxSym.
	 */
	def receiverTmtInSymbolContext(cls: Symbol, ctxSym: Symbol)(implicit ctx: Context): (Tmt, Tree) = {
		tmtAnnotationsOnSymbol(ctxSym) foreach { case (tm, argTp, arg) =>
			argTp match {
				case argTp: ThisType => if ((cls derivesFrom argTp.cls) || (argTp.cls derivesFrom cls)) return (tm,arg)
				case _ =>
			}
		}
		(UnannotatedTmt(), EmptyTree)
	}
	
	
	
	//--- Type Assignment/Adaptation ---//

	/**
	 * registerContext records the fact that the given type tp is created
	 * in the context ctx.
	 * At the moment, the only thing that must be remembered is the TMT
	 * of tp in the given context.
	 */
	def registerContext[T <: NamedType](tp: T)(implicit ctx: Context): T = tp
	def registerContext(tp: ThisType)(implicit ctx: Context): ThisType = tp
	/*	// The tmt field is a first-class function.
		// Basically, the eligible annotations are requested immediately, but
		// the actual computation of the TMT is deferred until requested.
		// The reason for this laziness is that requesting the denotation of
		// the given type will cause cyclic reference errors if called eagerly.
		var annots = contextAnnots filterNot {
			case (hostTree, _) => currentlyTypingModifiersOf contains hostTree
		}
		var tm: Tmt = UnannotatedTmt()
		tp.tmt = { () => //tmtInCurrentContext(tp.denot)
			if (annots != null) {   // has the final TMT for the type been computed?
				tp.denot.alternatives foreach { sd =>
					tm = lub(tm, tmtInGivenContext(sd.symbol, annots)._1)
				}
				annots = null   // set annots to null to free the memory used by the list (minimizes probability of holding onto a non-current context for a long time)
			}
			tm
		}
		tp
	}
		tp.tmt = receiverTmtInCurrentContext(tp.cls)._1
		tp
	}*/
	
	/**
	 * The TMT of some types depends on what context they were originally created in.
	 * The contextual information needs to be remembered when the type is copied.
	 */
	def registerDerivation[T <: Type](from: T, to: T): T = to
	def registerDerivation(from: ThisType, to: ThisType): ThisType = to
	/*def registerDerivation[T <: NamedType](from: T, to: T): T = {
		to.tmt = from.tmt
		to
	}
	def registerDerivation(from: ThisType, to: ThisType): ThisType = {
		to.tmt = from.tmt
		to
	}*/
	
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
	/*def termRefInfo(info: Type, tp: TermRef)(implicit ctx: Context): Type = {
		// Methodic types don't need to be adapted.
		// TODO: if denot.symbol has polyread receiver annotation, and info has polyread result, then adapt with prefix tmt.
		if (info.isInstanceOf[MethodicType]) info
		else {
			def currentContextTm = {
				testing foreach { case (tm, argTp, arg) =>
					argTp match {
						case argTp: NamedType => argTp.denot.alternatives foreach { sd =>
							if (sd.symbol == tp.denot.symbol)
								return tm
						}
						case _ =>
					}
				}
				return UnannotatedTmt()
			}
			withTmt(info, lub(currentContextTm, tmt(tp.prefix), tmt(info)))
		}
	}*/
	def termRefInfo(info: Type, tp: TermRef)(implicit ctx: Context): Type = {
		// Methodic types don't need to be adapted.
		// TODO: if denot.symbol has polyread receiver annotation, and info has polyread result, then adapt with prefix tmt.
		if (info.isInstanceOf[MethodicType]) info
		else {
			//val ctxTmt = tmtInCurrentContext(tp.denot)
			//if (ctxTmt.exists)
			//	withTmt(info, lub(ctxTmt, tmt(tp.prefix)))
			//else
				// Still problems with the following line (causes a "stale symbol" error when taking the denotation of the underlying info):
				withTmt(info, lub(tmt(info), tmt(tp.prefix)))
				// Possible solution: Just bake the TMT into the type when it is created.
				// Whenever the term is accessed from some other context, there should be a TermRef pointing to it,
				// so viewpoints should still work correctly.
				
				// - TMTs should be assigned to types when types are assigned to trees.
				
				// - Type derivations should copy the TMT of their prototypes.
				
				// - ?
				// Perhaps the problem here is that I'm asking for the denotation of a symbol, e.g.:
				// tp.denot.symbol.denot
				// Probably the first denot could be in a different phase than the second denot.
		}
	}
	
	/**
	 * Changes the type returned from a TypeRef's info method.
	 * 
	 * No adapation is needed here.
	 */
	//def typeRef(info: Type, tRef: TypeRef)(implicit ctx: Context): Type = info
	
	/**
	 * Changes the TMT of a ThisType.
	 * 
	 * The adaptation is: Find the receiver mutability in the current context,
	 * and wrap the type with an annotation.
	 *
	 * ThisType mutabilities are distinguished by class.
	 * The annotation @readonly(C.this) means that any D.this where D is derived from C
	 * is considered readonly.
	 */
	/*def thisTypeUnderlying(tp: Type, underlying: Type)(implicit ctx: Context): Type = {
		val ThisType(tRef) = tp
		withTmt(underlying, receiverTmtInCurrentContext(tRef.symbol)._1)
	}*/
	
	/**
	 * Changes the TMT of a SuperType.
	 * 
	 * Takes the LUB of the supertype's "this" and the member it refers to.
	 */
 	/*def superTypeUnderlying(tp: Type, underlying: Type)(implicit ctx: Context): Type = {
		val SuperType(thistpe, _) = tp
		withTmt(underlying, lub(tmt(thistpe), tmt(underlying)))
	}*/
	
		
	
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
	
	def lub(tmt1: Tmt, tmt2: Tmt, tmt3: Tmt): Tmt = lub(lub(tmt1, tmt2), tmt3)
	
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
			
			case TypeBounds(tp11, tp12) =>
				/// TypeBounds is handled like AndType -- both ends of the bound must be correct.
				val r1 = refinementSubtypeOf(tp11, tp2, alreadySeen)
				if (!r1.result) r1 else refinementSubtypeOf(tp12, tp2, alreadySeen)
	
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
			
			case TypeBounds(tp21, tp22) =>
				/// TypeBounds is handled like AndType -- both ends of the bound must be correct.
				val r1 = namedTypeMemberSubtype(parent1, info1, tp21, name2, alreadySeen)
				if (!r1.result) r1 else namedTypeMemberSubtype(parent1, info1, tp22, name2, alreadySeen)
			
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
	
	//----- Override Checking -----//
	
	/**
	 * Finds the outer-parameter TMTs of all Term annotations on the given symbol.
	 * Each item returned represents a single denotation.
	 * Each outer-parameter symbol is reported exactly once.
	 */
	private[this] def filteredTermAnnotationsOnSymbol(sym: Symbol)(implicit ctx: Context): List[(Tmt,Symbol,Tree)] = {
		var r = List[(Tmt,Symbol,Tree)]()
		var alreadyFound = List[Symbol]()
		
		tmtAnnotationsOnSymbol(sym) foreach { case (tm, argTpe, arg) =>
			argTpe match {
				case argTpe: NamedType =>
					argTpe.denot.alternatives.foreach { singleDenot =>
						if (!(alreadyFound contains singleDenot.symbol)) {
							r = (tm, singleDenot.symbol, arg) :: r
							alreadyFound = singleDenot.symbol :: alreadyFound
						}
					}
				case _ =>
			}
		}
		r.reverse
	}

	/**
	 * Finds the outer-parameter TMTs of all This annotations on the given symbol.
	 * Each item returned represents the "this" of a single class.
	 * Each class is reported exactly once.
	 */
	private[this] def filteredThisAnnotationsOnSymbol(sym: Symbol)(implicit ctx: Context): List[(Tmt,ClassSymbol,Tree)] = {
		var r = List[(Tmt,ClassSymbol,Tree)]()
		var alreadyFound = List[ClassSymbol]()
		
		tmtAnnotationsOnSymbol(sym) foreach { case (tm, argTpe, arg) =>
			argTpe match {
				case argTpe: ThisType =>
					if (!(alreadyFound contains argTpe.cls)) {
						r = (tm, argTpe.cls, arg) :: r
						alreadyFound = argTpe.cls :: alreadyFound
					}
				case _ =>
			}
		}
		r.reverse
	}


	/**
	 * Check that an overriding symbol's type matches the overridden symbol's type.
	 * Issues an error if the overriding type is incompatible.
	 */
	def tmtCheckOverride(sym: Symbol, tp: Type, overriddenSym: Symbol, overriddenTp: Type)(implicit ctx: Context): Unit = {
		assert(contextAnnots.isEmpty)  // sanity check: contextAnnots should be empty after typer phase is complete
	
		// TODO: polyread parameters/returns should be allowed to match (although their origins will be different)
		if (!tmtSubtypeOf(tp, overriddenTp))
			ctx.error(
				tmtExplainingSubtypeOf(
					tp, overriddenTp,
					s"${sym} in ${sym.owner}", s"${overriddenSym.name} in ${overriddenSym.owner}"),
				sym.pos)
		
		else tmtCheckOverriddenOuterParams(sym, overriddenSym)
	}
	
	/**
	 * Check that an overriding symbol's outer parameter annotations match the overridden
	 * symbol's outer parameter annotations.
	 * Issues an error if an incompatible override is found.
	 */
	private[this] def tmtCheckOverriddenOuterParams(sym: Symbol, overriddenSym: Symbol)(implicit ctx: Context): Unit = {
	
		// Check outer parameters for consistency.
		// This check requires that the entire environment of the overriding symbol be compatiable
		// with the environment of the overridden symbol -- it is not enough to know what annotations
		// are on each symbol -- annotations on enclosing symbols must also be examined.
		// Step 1. For each outer parameter on sym (or owner of sym), either overriddenSym
		//  does not have a matching parameter, or sym's parameter is not a subtype of
		//  overriddenSym's parameter (parameters are contravariant).
		filteredTermAnnotationsOnSymbol(sym).foreach { case (argTm, argSym, argTree) =>
			val (overTm, overArgTree) = tmtInSymbolContext(argSym, overriddenSym)
			if (!(overTm <:< argTm))
				ctx.error("override error:\n" +
					s" cannot override $overTm outer parameter" +
					s" on ${overriddenSym} in ${overriddenSym.owner} with $argTm in ${sym.owner}",
					argTree.pos)
		}
		filteredThisAnnotationsOnSymbol(sym).foreach { case (argTm, clazz, argTree) =>
			val (overTm, overArgTree) = receiverTmtInSymbolContext(clazz, overriddenSym)
			if (!(overTm <:< argTm))
				ctx.error("override error:\n" +
					s" cannot override $overTm outer parameter" +
					s" on ${overriddenSym} in ${overriddenSym.owner} with $argTm in ${sym.owner}",
					argTree.pos)
		}
		// Step 2. Any parameters on overriddenSym (or owners) must be subtypes of the
		//  parameters on sym. This step is necessary because overriddenSym may have
		//  @readonly parameters that are unannotated with respect to sym.
		filteredTermAnnotationsOnSymbol(overriddenSym).foreach { case (overTm, overArgSym, overArgTree) =>
			val (argTm, argTree) = tmtInSymbolContext(overArgSym, sym)
			if (!(overTm <:< argTm) && !argTm.exists)
				ctx.error("override error:\n" +
					s" ${sym} in ${sym.owner} needs an annotation for $overTm(${overArgSym.name})" +
					s" when overridding ${overriddenSym} in ${overriddenSym.owner}",
					sym.pos)
		}
		filteredThisAnnotationsOnSymbol(overriddenSym).foreach { case (overTm, overClazz, overArgTree) =>
			val (argTm, argTree) = receiverTmtInSymbolContext(overClazz, sym)
			if (!(overTm <:< argTm) && !argTm.exists)
				ctx.error("override error:\n" +
					s" ${sym} in ${sym.owner} needs an annotation for $overTm(${overClazz.name}.this)" +
					s" when overridding ${overriddenSym} in ${overriddenSym.owner}",
					sym.pos)
		}
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