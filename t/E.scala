import annotation.{StaticAnnotation, TypeConstraint}

class readonly extends TypeConstraint {}
class polyread extends TypeConstraint {}
class mutable extends TypeConstraint {}
class bounded extends TypeConstraint {}

/*PackageDef(Ident(<empty>),
	List(TypeDef(Modifiers(<trait>,,List()),E,Template(
		DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
		List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List())),
		ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
		List(
		*/
trait E {

/* TypeDef(Modifiers(<trait>,,List()),F,
	Template(
		DefDef(Modifiers(,,List()),<init>,List(),List(List()),
			TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
		List(TypeTree[TypeRef(ThisType(module class lang),Object)]),
		ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
		List(TypeDef(Modifiers(,,List()),T,
			TypeTree[TypeBounds(
				TypeRef(ThisType(module class scala),Nothing),   // TYPE BOUNDS
				TypeRef(ThisType(module class scala),Any))])))),
		*/
	trait F {
		type T <: AnyRef @readonly  // T is readonly
		//@maybe(@readonly)  // @polyread  // T @ri, where @ri <: @readonly // @bounded(@mutable, @readonly)
	}

	type T = F @readonly;
	type U = F @mutable;   // OK: T and U are unrelated types
	var t: T = new U()   // readonly
	var u: U = new T()   // mutable



/*	TypeDef(Modifiers(<trait>,,List()),G,
		Template(
			DefDef(Modifiers(,,List()),<init>,
				List(TypeDef(Modifiers(<param>,,List()),U,TypeTree[TypeBounds(  // constructor takes a type parameter
					TypeRef(ThisType(module class scala),Nothing),
					TypeRef(ThisType(module class scala),Any))])),
				List(List()),
				TypeTree[TypeRef(ThisType(module class scala),Unit)],
				EmptyTree),
			List(TypeTree[TypeRef(ThisType(module class lang),Object)]),
			ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
			List(
				TypeDef(Modifiers(<param> <expandedname> <covariant>,,List()),E$G$$U,   // AUTOGEN TYPE BOUNDS
					TypeTree[TypeBounds(
						TypeRef(ThisType(module class scala),Nothing),
						TypeRef(ThisType(module class scala),Any))]),
				TypeDef(Modifiers(private[this] <paramaccessor> <synthetic>,,List()),U,   // AUTOGEN TYPE PARAM ACCESSOR
					TypeTree[TypeAlias(TypeRef(ThisType(class G),E$G$$U))])))),
					*/
	trait G[+U >: F <: F @readonly] {
	}


/*	ValDef(Modifiers(,,List()),f,TypeTree[TypeRef(NoPrefix,$anon)],
		Block(
			List(TypeDef(Modifiers(final,,List()),$anon,
				Template(
					DefDef(Modifiers(,,List()),<init>,List(),List(List()),
						TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
					List(
						Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
						Ident(F)),
					ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
					List(TypeDef(Modifiers(,,List()),T,
						TypeTree[TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))]))))),
			Apply(Select(New(Ident($anon)),<init>),List()))),
			*/
	val f = new F { type T = AnyRef }

/*	ValDef(Modifiers(,,List()),g,TypeTree[TypeRef(NoPrefix,$anon)],
		Block(
			List(TypeDef(Modifiers(final,,List()),$anon,
				Template(
					DefDef(Modifiers(,,List()),<init>,List(),List(List()),
						TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
					List(
						Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
						AppliedTypeTree(Ident(G),List(Ident(AnyRef)))),
					ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
					List()))),
			Apply(Select(New(Ident($anon)),<init>),List()))),
			*/
	val g = new G[AnyRef] {}


// presumably, a NAMED TYPE
/*	ValDef(Modifiers(,,List()),h0,Ident(F),EmptyTree), */
/*	ValDef(Modifiers(,,List()),h0,Annotated(Apply(Select(New(Ident(readonly)),<init>),List()),Ident(F)),EmptyTree) */
	val h0: F @readonly
	
/*	ValDef(Modifiers(,,List()),h1,Ident(G),EmptyTree), */
	val h1: G   // Presumably, G is a NamedType

//	REFINED TYPE
/*	ValDef(Modifiers(,,List()),h2,
		RefinedTypeTree(Ident(F),
			List(TypeDef(Modifiers(,,List()),T,
				TypeTree[TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))]))),
		EmptyTree),*/
/*	ValDef(Modifiers(,,List()),h2,  // WITH ANNOTATION AT TOP LEVEL
		Annotated(Apply(Select(New(Ident(readonly)),<init>),List()),RefinedTypeTree(Ident(F),List(TypeDef(Modifiers(,,List()),T,TypeTree[TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))])))),EmptyTree), */
/*	ValDef(Modifiers(,,List()),h2,
		RefinedTypeTree(Ident(F),
			List(TypeDef(Modifiers(,,List()),T,
				TypeTree[TypeAlias(
					AnnotatedType(  // WITH ANNOTATION ON TYPE PARAMETER
						ConcreteAnnotation(Apply(Select(New(Ident(readonly)),<init>),List())),
						TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)))]))),
		EmptyTree), */
	val h2: F { type T = AnyRef } @readonly
/*Annotated(Apply(Select(New(Ident(readonly)),<init>),List()),RefinedTypeTree(Ident(F),List(TypeDef(Modi
fiers(,,List()),T,TypeTree[TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))]),
DefDef(Modifiers(<method>,,List()),f,List(),List(List(ValDef(Modifiers(<param>,,List()),i,Ident(Int),EmptyTree))),TypeTree[TypeRef(T
ermRef(ThisType(module class <root>),scala),Int)],Ident(i)))))*/


//	REFINED TYPE
/*	ValDef(Modifiers(,,List()),h3,AppliedTypeTree(Ident(G),List(Ident(AnyRef))),EmptyTree)))))) */
/*	ValDef(Modifiers(,,List()),h3,AppliedTypeTree(Ident(G),List(
			Annotated(Apply(Select(New(Ident(readonly)),<init>),List()),Ident(AnyRef)))),  // WITH ANNOTATION
		EmptyTree) */
	val h3: G[AnyRef @readonly]

	// METHOD TYPE with TYPE PARAMETERS
/*	DefDef(Modifiers(<method>,,List()),m,
		List(TypeDef(Modifiers(<param>,,List()),U,
			TypeTree[TypeBounds(
				TypeRef(ThisType(module class scala),Nothing),
				TypeRef(ThisType(module class scala),Any))])),
		List(List(ValDef(Modifiers(<param>,,List()),a,Ident(U),EmptyTree))),
		Ident(Unit),
		EmptyTree)
		*/
	def m[U](a: U): Unit
	
	// TYPE PARAM and APPLIED/REFINED TYPE
	def n[U](a: List[U]): Unit
}

// TYPE REF is like SELECT for types?

/*
PackageDef(Ident(<empty>),
	List(TypeDef(Modifiers(<trait>,,List()),E,Template(
		DefDef(Modifiers(,,List()),<init>,List(),List(List()),TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
		List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List())),
		ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
		List(
		
			TypeDef(Modifiers(<trait>,,List()),F,Template(
				DefDef(Modifiers(,,List()),<init>,List(),List(List()),
					TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
				List(TypeTree[TypeRef(ThisType(module class lang),Object)]),
				ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
				List(TypeDef(Modifiers(,,List()),T,
					TypeTree[TypeBounds(TypeRef(ThisType(module class scala),Nothing),
					TypeRef(ThisType(module class scala),Any))])))),
					
			TypeDef(Modifiers(<trait>,,List()),G,
				Template(
					DefDef(Modifiers(,,List()),<init>,
						List(TypeDef(Modifiers(<param>,,List()),U,TypeTree[TypeBounds(
							TypeRef(ThisType(module class scala),Nothing),
							TypeRef(ThisType(module class scala),Any))])),
						List(List()),
						TypeTree[TypeRef(ThisType(module class scala),Unit)],
						EmptyTree),
					List(TypeTree[TypeRef(ThisType(module class lang),Object)]),
					ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
					List(
						TypeDef(Modifiers(<param> <expandedname>,,List()),E$G$$U,
							TypeTree[TypeBounds(
								TypeRef(ThisType(module class scala),Nothing),
								TypeRef(ThisType(module class scala),Any))]),
						TypeDef(Modifiers(private[this] <paramaccessor> <synthetic>,,List()),U,
							TypeTree[TypeAlias(TypeRef(ThisType(class G),E$G$$U))])))),
							
			ValDef(Modifiers(,,List()),f,TypeTree[TypeRef(NoPrefix,$anon)],
				Block(
					List(TypeDef(Modifiers(final,,List()),$anon,
						Template(
							DefDef(Modifiers(,,List()),<init>,List(),List(List()),
								TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
							List(
								Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
								Ident(F)),
							ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
							List(TypeDef(Modifiers(,,List()),T,
								TypeTree[TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))]))))),
					Apply(Select(New(Ident($anon)),<init>),List()))),
					
			ValDef(Modifiers(,,List()),g,TypeTree[TypeRef(NoPrefix,$anon)],
				Block(List(TypeDef(Modifiers(final,,List()),$anon,
					Template(
						DefDef(Modifiers(,,List()),<init>,List(),List(List()),
							TypeTree[TypeRef(ThisType(module class scala),Unit)],EmptyTree),
						List(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),List()),
							AppliedTypeTree(Ident(G),List(Ident(AnyRef)))),
						ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
						List()))),
					Apply(Select(New(Ident($anon)),<init>),List())))
					
			ValDef(Modifiers(,,List()),h0,Ident(F),EmptyTree),
			ValDef(Modifiers(,,List()),h1,Ident(G),EmptyTree),
			ValDef(Modifiers(,,List()),h2,
				RefinedTypeTree(Ident(F),
					List(TypeDef(Modifiers(,,List()),T,
						TypeTree[TypeAlias(TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))]))),
				EmptyTree),
			ValDef(Modifiers(,,List()),h3,AppliedTypeTree(Ident(G),List(Ident(AnyRef))),EmptyTree),

			DefDef(Modifiers(<method>,,List()),m,
				List(TypeDef(Modifiers(<param>,,List()),U,
					TypeTree[TypeBounds(
						TypeRef(ThisType(module class scala),Nothing),
						TypeRef(ThisType(module class scala),Any))])),
				List(List(ValDef(Modifiers(<param>,,List()),a,Ident(U),EmptyTree))),
				Ident(Unit),
				EmptyTree)
*/
