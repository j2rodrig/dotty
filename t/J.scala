import annotation._

trait J {  // for testing closure creation and partials

	var p: AnyRef @polyread
	//var r: AnyRef @readonly

	def d1(a: AnyRef @polyread)(b: AnyRef @polyread): AnyRef @polyread
	
	val f1 = d1(p) _
	
	//val f2 = f1(r)
	
	//val x = { a: AnyRef => r }

}


/***

// Listing 1:
import annotation._
trait J {
	var p: AnyRef @polyread
	def d1(a: AnyRef @polyread)(b: AnyRef @polyread): AnyRef @polyread
	val f1 = d1(p) _
}

// AST for Listing 1:
PackageDef(Ident(<empty>),
List(Import(Ident(annotation),List(Ident(_))),
TypeDef(
	Modifiers(<trait>,,List()),
	J,
	Template(
		DefDef(
			Modifiers(,,List()),
			<init>,
			List(),
			List(List()),
			TypeTree[TypeRef(ThisType(module class scala),Unit)],
			EmptyTree),
		List(
			TypeTree[TypeRef(ThisType(module class lang),Object)]),
		ValDef(Modifiers(private,,List()),_,EmptyTree,EmptyTree),
		List(
			ValDef(Modifiers(mutable,,List()),p,Annotated(Apply(Select(New(Ident(polyread)),<init>),List()),Ident(AnyRef)),EmptyTree),
			DefDef(
				Modifiers(<accessor> mutable,,List()),
				p_$eq,
				List(),
				List(
					List(
						ValDef(
							Modifiers(<param> <synthetic>,,List()),
							x$1,
							TypeTree[
								AnnotatedType(
									ConcreteAnnotation(Apply(Select(New(Ident(polyread)),<init>),List())),
									TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
							EmptyTree))),
				TypeTree[TypeRef(ThisType(module class scala),Unit)],
				EmptyTree),
			DefDef(
				Modifiers(<method>,,List()),
				d1,
				List(),
				List(
					List(
						ValDef(
							Modifiers(<param>,,List()),
							a,
							Annotated(Apply(Select(New(Ident(polyread)),<init>),List()),Ident(AnyRef)),
							EmptyTree)),
					List(
						ValDef(
							Modifiers(<param>,,List()),
							b,
							Annotated(Apply(Select(New(Ident(polyread)),<init>),List()),Ident(AnyRef)),
							EmptyTree))),
				Annotated(Apply(Select(New(Ident(polyread)),<init>),List()),Ident(AnyRef)),
				EmptyTree),
			ValDef(
				Modifiers(,,List()),
				f1,
				TypeTree[
					RefinedType(
						RefinedType(
							TypeRef(ThisType(module class scala),Function1),
							scala$Function1$$T1,
							ContraTypeAlias(
								AnnotatedType(
									ConcreteAnnotation(Apply(Select(New(Ident(polyread)),<init>),List())),
									TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)))),
						scala$Function1$$R,
						CoTypeAlias(
							AnnotatedType(
								ConcreteAnnotation(
									Apply(Select(New(TypeTree[TypeRef(ThisType(module class annotation),minpolyread)]),<init>),List())),
									TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))))],
				Block(
					List(
						ValDef(
							Modifiers(,,List()),
							$1$,
							TypeTree[
								AnnotatedType(
									ConcreteAnnotation(Apply(Select(New(Ident(polyread)),<init>),List())),
									TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
							Ident(p))),
					Block(
						List(
							DefDef(
								Modifiers(<synthetic>,,List()),
								$anonfun,
								List(),
								List(
									List(
										ValDef(
											Modifiers(<param> <synthetic>,,List()),
											b,
											TypeTree[
												AnnotatedType(
													ConcreteAnnotation(Apply(Select(New(Ident(polyread)),<init>),List())),
													TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
											EmptyTree))),
								TypeTree[
									AnnotatedType(
										ConcreteAnnotation(Apply(Select(New(TypeTree[TypeRef(ThisType(module class annotation),minpolyread)]),<init>),List())),
										TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
								Apply(
									Apply(
										Ident(d1),
										List(Ident($1$))),
									List(Ident(b))))),
						Closure(List(),Ident($anonfun),EmptyTree)))))))))

						***/