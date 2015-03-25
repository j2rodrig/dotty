package dotty.tools.dotc

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.tpd
import core._
import Types._, Symbols._, Annotations._, Contexts._
import dotty.tools.dotc.config.Printers.{noPrinter, Printer}
import dotty.tools.dotc.transform.OverridingPairs
import dotty.tools.dotc.transform.TreeTransforms.{TreeTransform, MiniPhase, TransformerInfo, MiniPhaseTransform}
import dotty.tools.dotc.typer.ProtoTypes.FunProto
import dotty.tools.dotc.typer._
import ErrorReporting._
import Decorators._
import Flags._
import util.Attachment

object Reim {
  val reim: Printer = noPrinter

  sealed abstract class BoundSpec
  case object Hi extends BoundSpec
  case object Lo extends BoundSpec
  case object Inferred extends BoundSpec


  abstract class ReimType {
    def cls(implicit ctx: Context): ClassSymbol
    def origin(implicit ctx: Context): Symbol

    def isMutable(implicit ctx: Context) = cls eq defn.MutableAnnot
    def isReadOnly(implicit ctx: Context) = cls eq defn.ReadOnlyAnnot

    def show(implicit ctx: Context) = if (origin eq NoSymbol) s"@${cls.name}" else s"@${cls.name}[${origin.name}}]"

    def <:<(other: ReimType)(implicit ctx: Context): Boolean = {
      (cls eq defn.MutableAnnot) || (other.cls eq defn.ReadOnlyAnnot) ||
        (cls eq other.cls)
        //((cls eq other.cls) && (origin eq other.origin))
    }
    def lub(other: ReimType)(implicit ctx: Context): ReimType = {
      if (this <:< other) other
      else if (other <:< this) this
      else ReimType.ReadOnly
    }
    def glb(other: ReimType)(implicit ctx: Context): ReimType = {
      if (this <:< other) this
      else if (other <:< this) other
      else ReimType.Mutable
    }
    def =:=(other: ReimType)(implicit ctx: Context): Boolean =
      (this <:< other) && (other <:< this)
  }
  object ReimType {
    val Mutable: ReimType = new ReimType {
      def cls(implicit ctx: Context) = defn.MutableAnnot
      def origin(implicit ctx: Context) = NoSymbol
    }
    val ReadOnly: ReimType = new ReimType {
      def cls(implicit ctx: Context) = defn.ReadOnlyAnnot
      def origin(implicit ctx: Context) = NoSymbol
    }
    def PolyRead(_origin: Symbol)(implicit ctx: Context): ReimType = new ReimType {
      def cls(implicit ctx: Context) = defn.PolyReadAnnot
      def origin(implicit ctx: Context) = _origin.enclosingClassOrMethod
    }
    def apply(cls: ClassSymbol, _origin: Symbol)(implicit ctx: Context) =
      if (cls eq defn.ReadOnlyAnnot) ReadOnly
      else if (cls eq defn.PolyReadAnnot) PolyRead(_origin)
      else Mutable
  }

  abstract class ReimAnnotation extends Annotation {
    val reimType: ReimType
    def show(implicit ctx: Context) = reimType.show
  }
  object ReimAnnotation {
    def apply(_reimType: ReimType)(implicit ctx: Context) = new ReimAnnotation {
      private val _tree = tpd.New(_reimType.cls.typeRef, Nil)
      def tree(implicit ctx: Context) = _tree
      val reimType = _reimType
    }
  }

  implicit class TypeDecorator(val tpe: Type) extends AnyVal {
    def withAnnotation(reimType: ReimType)(implicit ctx: Context) =
      AnnotatedType(ReimAnnotation(reimType), tpe.removeAnnotations)

    def removeAnnotations(implicit ctx: Context): Type = tpe match {
      case tpe: AnnotatedType if isAnnotation(tpe.annot.symbol) => tpe.tpe.removeAnnotations
      case tpe: AnnotatedType => tpe.derivedAnnotatedType(tpe.annot, tpe.tpe.removeAnnotations)
      case _ => tpe
    }

    def directReimAnnotation(implicit ctx: Context): Option[ReimType] = tpe match {
      case tpe: AnnotatedType =>
        if (isAnnotation(tpe.annot.symbol)) Some(ReimType(tpe.annot.symbol.asInstanceOf[ClassSymbol], tpe.termSymbol))
        else tpe.tpe.directReimAnnotation
      case _ => None
    }

    def reimAnnotation(bound: BoundSpec = Inferred)(implicit ctx: Context): Option[ReimType] = {
      def ifConsistent(tpe: Type): Option[ReimType] = {
        val hi = tpe.reimAnnotation(Hi).get
        val lo = tpe.reimAnnotation(Lo).get
        if (hi =:= lo) Some(hi) else None
      }
      def anyAnnot: Option[ReimType] = {
        bound match {
          case Inferred => None
          case Hi => Some(ReimType.ReadOnly)
          case Lo => Some(ReimType.Mutable)
        }
      }
      tpe.directReimAnnotation.orElse(tpe.stripAnnots match {
        case tpe: TypeRef => tpe.underlying.reimAnnotation(bound)
        case tpe: TermRef =>
          val symbol = tpe.symbol

          if ((tpe.prefix eq NoPrefix) && !insidePureBoundary(tpe.symbol)) Some(ReimType.ReadOnly)
          else if(symbol.is(Method) || symbol.hasAnnotation(defn.NonRepAnnot)) symbol.info.reimAnnotation(bound)
          else bound match {
            case Inferred =>
              val preAnnot = tpe.prefix.reimAnnotation(Inferred)
                .getOrElse(tpe.prefix.reimAnnotation(Hi).get)
              ifConsistent(symbol.info).map(_.lub(preAnnot))
            case _ =>
              val preAnnot = tpe.prefix.reimAnnotation(bound).get
              val symAnnot = symbol.info.reimAnnotation(bound).get
              Some(symAnnot.lub(preAnnot))
          }
        case tpe: ThisType =>
          var annot = getAnnotationOfThis(tpe.cls)
          if ((annot.cls eq defn.MutableAnnot) && !insidePureBoundary(tpe.cls)) annot = ReimType.PolyRead(tpe.cls)
          Some(annot)
        case tpe: SuperType => tpe.thistpe.reimAnnotation(bound)
        case tpe: ConstantType => Some(ReimType.Mutable)
        case tpe: MethodParam => tpe.underlying.reimAnnotation(bound)
        // TODO: SkolemType
        case tpe: PolyParam => ctx.typerState.constraint.fullBounds(tpe).reimAnnotation(bound)
        case tpe: RefinedType => tpe.parent.reimAnnotation(bound)
        case tpe: TypeBounds => bound match {
          case Inferred => ifConsistent(tpe)
          case Lo => tpe.lo.reimAnnotation(Lo)
          case Hi => tpe.hi.reimAnnotation(Hi)
        }
        //case tpe: TypeAlias => tpe.underlying.reimAnnotation(bound)  // covered by TypeBounds
        case tpe: ExprType => tpe.underlying.reimAnnotation(bound)
        case tpe: TypeVar => tpe.underlying.reimAnnotation(bound)
        case tpe: AndType => bound match {
          case Inferred => ifConsistent(tpe)
          case _ =>
            Some(tpe.tp1.reimAnnotation(bound).get.lub(tpe.tp2.reimAnnotation(bound).get))
        }
        case tpe: OrType => bound match {
          case Inferred => ifConsistent(tpe)
          case _ =>
            Some(tpe.tp1.reimAnnotation(bound).get.glb(tpe.tp2.reimAnnotation(bound).get))
        }
        case tpe: ClassInfo => Some(ReimType.Mutable)
        case NoPrefix => Some(ReimType.Mutable)
        case tpe: ErrorType => Some(ReimType.Mutable)
        case tpe: WildcardType => tpe.optBounds match {
          case info: TypeBounds => info.reimAnnotation(bound)
          case NoType => anyAnnot
        }
        case tpe: ImportType => anyAnnot
        case tpe: MethodType => anyAnnot
        case tpe: PolyType => anyAnnot
        case NoType => anyAnnot
      })
    }
  }

  implicit class TreeDecorator(val tree: tpd.Tree) extends AnyVal {
    def withAnnotation(reimType: ReimType)(implicit ctx: Context) = tree.withType(tree.tpe.withAnnotation(reimType))
  }

  implicit class SymbolDecorator(val symbol: Symbol) extends AnyVal {
    def reimAnnotation(implicit ctx: Context): ReimType =
      reimAnnotationWithDefault(defn.MutableAnnot)

    def reimAnnotationWithDefault(default: ClassSymbol)(implicit ctx: Context): ReimType =
      ReimType(
        symbol.annotations.map(_.symbol).filter(isAnnotation(_))
          .headOption.getOrElse(default).asInstanceOf[ClassSymbol], symbol)

    def getAnnotTrees(cls: ClassSymbol)(implicit ctx: Context): List[tpd.Tree] = {
      if (symbol.isCompleted) {
        symbol.annotations.filter(_.symbol eq cls).map(_.tree)
      } else {
        // Evil hack: We only need the annotations, but the symbol is not yet completed.
        // So we reach into the completer to get the corresponding tree, and look at the
        // annotations on the tree.
        val typer = ctx.typer
        val completer = symbol.completer.asInstanceOf[typer.Completer]
        val tree = completer.original.asInstanceOf[untpd.MemberDef]
        val annotTrees = untpd.modsDeco(tree).mods.annotations.mapconserve(typer.typedAnnotation)
        annotTrees.filter { tree =>
          cls eq (if (tree.symbol.isConstructor) tree.symbol.owner else tree.tpe.typeSymbol)
        }
      }
    }

    def isAnnotatedWith(cls: ClassSymbol)(implicit ctx: Context): Boolean = getAnnotTrees(cls).nonEmpty

    def enclosingClassOrMethod(implicit ctx: Context): Symbol =
      if (!symbol.exists || (symbol is Method) || symbol.isClass) symbol
      else symbol.effectiveOwner.enclosingClassOrMethod

    def isVal(implicit ctx: Context): Boolean =
      !(symbol.isClass || symbol.isType || (symbol is Method))
  }

  implicit class ClassSymbolDecorator(val symbol: ClassSymbol) extends AnyVal {
    def <:<(other: ClassSymbol)(implicit ctx: Context) = {
      assert(isAnnotation(symbol))
      assert(isAnnotation(other))
      symbol == other || symbol == defn.MutableAnnot || other == defn.ReadOnlyAnnot
    }

    def lub(other: ClassSymbol)(implicit ctx: Context): ClassSymbol =
      if(symbol <:< other) other else symbol
    def glb(other: ClassSymbol)(implicit ctx: Context): ClassSymbol =
      if(symbol <:< other) symbol else other
  }

  private def isAnnotation(symbol: Symbol)(implicit ctx: Context) =
    (symbol == defn.MutableAnnot) || (symbol == defn.ReadOnlyAnnot) || (symbol == defn.PolyReadAnnot)

  val PolyReadKey = new Attachment.Key[PolyReadAttachments]
  sealed abstract class PolyReadAttachments
  object Arg extends PolyReadAttachments
  object PolyReadNeedsToBeReadOnly extends PolyReadAttachments
  case class PolyReadNeedsToBePolyRead(origin: Symbol) extends PolyReadAttachments

  class ReimTyper extends Typer {

    private def explicitAnnotIfNeeded(tree: tpd.Tree)(implicit ctx: Context) = {
      val reimAnnotation = tree.tpe.reimAnnotation(Inferred)
      if (reimAnnotation.exists(_ != tree.tpe.directReimAnnotation.getOrElse(defn.MutableAnnot)))
        tree.withType(tree.tpe.withAnnotation(reimAnnotation.get))
      else tree
    }

    private def checkValDefAnnot(tree: tpd.Tree)(implicit ctx: Context) = tree match {
      case tree: tpd.ValDef =>
        val annot = tree.tpe.reimAnnotation(Lo).get
        if((annot.cls eq defn.MutableAnnot) && !tree.symbol.hasAnnotation(defn.NonRepAnnot))
          tree.withType(tree.tpe.withAnnotation(ReimType.PolyRead(tree.symbol)))
        else tree
      case _ => tree
    }

    override def adapt(tree0: tpd.Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): tpd.Tree = {

      /** Perform viewpoint adaptation on a method call. */
      def viewPointAdapt(tree: tpd.Tree): tpd.Tree =
        if ((tree.symbol is Method) && (!tree.isInstanceOf[tpd.DefTree])
          && (tree.tpe.reimAnnotation(Hi).get.cls == defn.PolyReadAnnot || tree.symbol.isConstructor)) {
          def fun(tree: tpd.Tree): tpd.Tree = tree match {
            case _: tpd.Ident | _: tpd.Select => tree
            case tree: tpd.Apply => fun(tree.fun)
            case tree: tpd.TypeApply => fun(tree.fun)
            case tree: tpd.Typed => fun(tree.expr)
          }
          val receiverType = fun(tree).tpe.stripAnnots.asInstanceOf[TermRef].prefix
          var replacement = ReimType.Mutable
          // is the receiver parameter polyread and is the argument receiver non-mutable?
          if (tree.symbol.reimAnnotation.cls == defn.PolyReadAnnot) replacement = receiverType.reimAnnotation(Hi).get
          reim.println(s"reimAnnotation of ${tree.symbol} is ${tree.symbol.reimAnnotation}")
          // do any polyread parameters have non-mutable arguments?
          original match {
            case original: untpd.Apply => for(arg <- original.args) {
              arg.removeAttachment(PolyReadKey).foreach { attachment =>
                replacement = attachment match {
                  case PolyReadNeedsToBeReadOnly => ReimType.ReadOnly
                  case PolyReadNeedsToBePolyRead(origin) => replacement.lub(ReimType.PolyRead(origin))
                  case _ => replacement
                }
              }
            }
            case _ =>
          }

          tree.withAnnotation(replacement)
        } else tree


      pt match {
        case proto: FunProto => proto.args.foreach(arg => arg.putAttachment(PolyReadKey, Arg))
        case _ =>
      }

      val tree = checkValDefAnnot(explicitAnnotIfNeeded(viewPointAdapt(super.adapt(tree0, pt, original))))

      val dontCheck = (
           pt == WildcardType
        || !tree.tpe.exists
        || pt.isInstanceOf[ProtoType]
        || tree.tpe <:< defn.AnyValType
        )

      if(dontCheck) tree else {
        val ptAnnot = pt.reimAnnotation(Lo).get
        val treeAnnot = tree.tpe.reimAnnotation(Hi).get
        if((ptAnnot.cls eq defn.PolyReadAnnot) && original.removeAttachment(PolyReadKey).contains(Arg)) {
          if(treeAnnot.cls eq defn.ReadOnlyAnnot) original.putAttachment(PolyReadKey, PolyReadNeedsToBeReadOnly)
          if(treeAnnot.cls eq defn.PolyReadAnnot) original.putAttachment(PolyReadKey, PolyReadNeedsToBePolyRead(treeAnnot.origin))
          tree
        } else if (!((tree.tpe == pt) || (treeAnnot <:< ptAnnot))) {
          println(s"${treeAnnot.origin} does not match ${ptAnnot.origin}")
          err.typeMismatch(tree, pt)
        } else tree
      }
    }
  }

  def isExplicitlyPureMethod(methodSym: Symbol)(implicit ctx: Context): Boolean = {
    assert(methodSym is Method)
    methodSym.isAnnotatedWith(defn.PureAnnot) ||
      (methodSym.isConstructor && methodSym.enclosingClass.isAnnotatedWith(defn.PureAnnot))
  }

  def isDefaultedPureMethod(methodSym: Symbol)(implicit ctx: Context): Boolean = {
    assert(methodSym is Method)
    methodSym.isConstructor || methodSym.isGetter
  }

  def isPureMethod(sym: Symbol, allowDefaultPure: Boolean)(implicit ctx: Context): Boolean =
    (sym is Method) && (isExplicitlyPureMethod(sym) || (allowDefaultPure && isDefaultedPureMethod(sym)))

  def isPureMethodOrClassOrVal(sym: Symbol, allowDefaultPure: Boolean)(implicit ctx: Context): Boolean =
    isPureMethod(sym, allowDefaultPure) ||
      ((sym.isClass || sym.isVal) && sym.isAnnotatedWith(defn.PureAnnot))

  def innermostEnclosingPure(symbol: Symbol, allowDefaultPure: Boolean = true)(implicit ctx: Context): Symbol = {
    if (!symbol.exists) {
      NoSymbol
    } else if (isPureMethodOrClassOrVal(symbol, allowDefaultPure)) {
      if (symbol.isConstructor) symbol.enclosingClass else symbol
    }
    else innermostEnclosingPure(symbol.owner)
  }

  /** Is the given symbol not outside a @pure-delimited boundary? (As seen from the current context.) */
  def insidePureBoundary(symbol: Symbol)(implicit ctx: Context): Boolean = {
    val pureBoundary = innermostEnclosingPure(ctx.owner)
    (pureBoundary eq NoSymbol) || symbol.isContainedIn(pureBoundary)
  }

  def getAnnotationOfThis(thisSym: Symbol)(implicit ctx: Context): ReimType = {
    val owner = ctx.owner
    assert(owner ne null)
    if (!owner.exists) {
      // We have walked the owner chain and not found the symbol. This can happen in two cases:
      // 1. `thisSym` is a module class (i.e. a globally static object) referenced from outside.
      // 2. `thisSym` appears in a declaration of a self-type, which gets type-checked *outside* the
      //     scope of its defining class.
      // I don't know of a condition to assert for case 2 (we would need the `Tree`), so we have
      // to comment the assertion for case 1 out.
//      assert(thisSym is ModuleClass)
      ReimType.Mutable
    } else if (owner == thisSym || ((owner is Flags.Method) && (owner.owner == thisSym))) {
      if (owner.isCompleted) owner.reimAnnotation
      else {
        // Evil hack: We only need the annotations, but the symbol is not yet completed.
        // So we reach into the completer to get the corresponding tree, and look at the
        // annotations on the tree.
        val typer = ctx.typer
        val completer = owner.completer.asInstanceOf[typer.Completer]
        val tree = completer.original.asInstanceOf[untpd.MemberDef]
        val annots = untpd.modsDeco(tree).mods.annotations.mapconserve(typer.typedAnnotation(_))
        def annot(annots: List[tpd.Tree]): ReimType = annots match {
          case Nil => ReimType.Mutable
          case head :: tail =>
            val symbol = head.symbol
            // TODO: I think we need to test symbol.owner here, not symbol (which could be a constructor)... verify?
            if (isAnnotation(symbol.owner)) ReimType(symbol.owner.asInstanceOf[ClassSymbol], thisSym) else annot(tail)
        }
        annot(annots)
      }
    } else getAnnotationOfThis(thisSym)(ctx.outer)
  }


  /** This phase checks that the prefix of the target of any assignment is mutable. */
  class ReimPhase extends MiniPhaseTransform {
    override def phaseName: String = "reim"

    override def transformAssign(tree: tpd.Assign)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
      tree.lhs match {
        case _: tpd.Select | _: tpd.Ident =>
          val annot = tree.lhs.tpe.stripAnnots match {
            case tr: TermRef =>
              if (tr.prefix eq NoPrefix) {
                if (insidePureBoundary(tr.symbol)) ReimType.Mutable else ReimType.ReadOnly
              } else tr.prefix.reimAnnotation(Hi).get
          }
          if (annot.isMutable) tree else errorTree(tree, s"assignment to field of @${annot.cls.name} reference")
      }
  }

  class ReimTypeComparer(initctx: Context) extends TypeComparer(initctx) {
    override def isSubType(tp1: Type, tp2: Type): Boolean = super.isSubType(tp1, tp2) && (
      !tp1.widenExpr.stripAnnots.isValueType || !tp2.widenExpr.stripAnnots.isValueType || tp1 == tp2 || {
      val annot1 = tp1.widenExpr.reimAnnotation(Hi).get
      val annot2 = tp2.widenExpr.reimAnnotation(Lo).get
      annot1 <:< annot2
    })

    override def copyIn(ctx: Context): TypeComparer = new ReimTypeComparer(ctx)
  }

  /** This phase runs the regular Scala RefChecks with the Reim type comparer to enforce necessary
    * subtyping relationships between symbols.
    */
  class ReimRefChecks extends RefChecks {
    override def run(implicit ctx: Context): Unit = super.run(ctx.fresh.setTypeComparerFn(c => new ReimTypeComparer(c)))
    override def phaseName: String = "reimrefchecks"
  }

  /** This phase checks addition Reim-specific subtyping relationships between symbols.
    */
  class ReimRefChecks2 extends MiniPhase { thisTransformer =>
    val treeTransform = new TreeTransform {

      private def checkPurity(fun: tpd.Tree, tree: tpd.Tree)(implicit ctx: Context) = {
        val methodSym = tree.symbol
        if (!isPureMethod(methodSym, allowDefaultPure = true)) {  // the method is not independently pure
          val ctxPurityBoundary = innermostEnclosingPure(ctx.owner)
          if (ctxPurityBoundary ne NoSymbol) {  // there exists a purity boundary here
            if (!methodSym.isContainedIn(ctxPurityBoundary)) {  // the method is not inside the local purity boundary
              if (!isPureMethodOrClassOrVal(ctxPurityBoundary, allowDefaultPure = false))   // issue a warning instead of an error if the purity boundary was defaulted
                ctx.warning(s"call of $methodSym is not contained by the default-pure boundary $ctxPurityBoundary", ctx.tree.pos)
              else
                ctx.error(s"call of $methodSym is not contained by pure boundary $ctxPurityBoundary", ctx.tree.pos)
            }
          }
        }
      }

      private def checkReceiver(fun: tpd.Tree, tree: tpd.Tree)(implicit ctx: Context) = {
        val receiverType = tree.tpe.stripAnnots.asInstanceOf[TermRef].prefix
        val receiverAnnot = receiverType.reimAnnotation(Hi).get
        val methodSym = tree.symbol
        val methodAnnot = methodSym.reimAnnotation
        if (methodAnnot.isMutable && !receiverAnnot.isMutable) {
          // TODO: upgrade to an error when pure-related defaults are correctly implemented
          ctx.warning(s"call of $methodSym on ${methodSym.enclosingClass} taking @${methodAnnot.cls.name} this on @${receiverAnnot.cls.name} receiver", tree.pos)
          tree
          //errorTree(tree, s"call of $methodSym on ${methodSym.enclosingClass} taking @${methodAnnot.name} this on @${receiverAnnot.name} receiver")
        } else tree
      }

      private def checkPurityOverride(overriding: Symbol, overridden: Symbol, allowDefaultPure: Boolean = true)(implicit ctx: Context): Boolean = {
        if (!isPureMethod(overriding, allowDefaultPure)) {  // overriding method is not independently pure
          val overriddenPurityBoundary = innermostEnclosingPure(overridden, allowDefaultPure)
          if (overriddenPurityBoundary ne NoSymbol) {  // the overridden method is inside a purity boundary
            if (!overriding.isContainedIn(overriddenPurityBoundary)) {  // overriding method is not within the overridden purity boundary
              if (allowDefaultPure) {  // if either overridden or overriding purity was defaulted, issue a warning instead of an error
                if (checkPurityOverride(overriding, overridden, allowDefaultPure = false))
                  ctx.warning(s"$overriding should not override $overridden due to incompatible purity", overriding.pos)
              } else {
                ctx.error(s"$overriding cannot override $overridden due to incompatible purity", overriding.pos)
                return false
              }
            }
          }
        }
        true
      }

      override def transformSelect(tree: tpd.Select)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        checkPurity(tree, tree)
        if (tree.symbol is Method) checkReceiver(tree, tree) else tree
      }

      override def transformIdent(tree: tpd.Ident)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        checkPurity(tree, tree)
        if (tree.symbol is Method) checkReceiver(tree, tree) else tree
      }

      override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
        def adjustedAnnotation(symbol: Symbol): ReimType =
          if (symbol is Method) symbol.reimAnnotation
          else ReimType.ReadOnly
        val cursor = new OverridingPairs.Cursor(ctx.owner)
        while (cursor.hasNext) {
          val overridingAnnot = adjustedAnnotation(cursor.overriding)
          val overriddenAnnot = adjustedAnnotation(cursor.overridden)
          if(!(overriddenAnnot <:< overridingAnnot))
            ctx.error(s"${cursor.overriding} with @${overridingAnnot.cls.name} this cannot override ${cursor.overridden} with @${overriddenAnnot.cls.name} this", cursor.overriding.pos)
          // If a val overrides a method, the method's return type must be at least @polyread, since the val will be viewpoint-adapted.
          if(cursor.overriding.isVal && !cursor.overriding.hasAnnotation(defn.NonRepAnnot)) {
            if (cursor.overridden.info.finalResultType.reimAnnotation(Lo).get.isMutable)
              ctx.error(s"${cursor.overriding} cannot override ${cursor.overridden} that could return @mutable", cursor.overriding.pos)
            else if (cursor.overridden.is(Method) &&  (cursor.overridden.reimAnnotation.cls ne defn.PolyReadAnnot))
            // TODO: upgrade to an error when pure-related defaults are correctly implemented
              ctx.warning(s"${cursor.overriding} cannot override ${cursor.overridden} that does not have @polyread this", cursor.overriding.pos)
          }

          checkPurityOverride(cursor.overriding, cursor.overridden)

          cursor.next()
        }
        tree
      }

      def phase = thisTransformer
    }

    def phaseName: String = "reimrefchecks2"
  }
}
