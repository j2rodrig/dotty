package test

import annotation._

//import annotation.{StaticAnnotation, TypeConstraint}

//class readonly extends TypeConstraint {}
//class polyread extends TypeConstraint {}
//class mutable extends TypeConstraint {}

trait H {
	//var na: AnyRef
	//var mu: AnyRef @mutable
	//var pr: AnyRef @polyread
	//var ro: AnyRef @readonly
	
trait wrapper(
	var na: AnyRef,
	var mu: AnyRef @mutable,
	var prx: AnyRef @polyread,    // should error
	var ro: AnyRef @readonly) {
	
def wrapF(pr: AnyRef @polyread) = {
	
	
	def noargs(): Unit = {}
	
	def f_mr(m: AnyRef @mutable): AnyRef @readonly = { m }
	@readonly def f_mr2(@mutable m: AnyRef): AnyRef = { m }
	
	def f_rm(r: AnyRef @readonly): AnyRef @mutable = { mu }
	@mutable def f_rm2(@readonly r: AnyRef): AnyRef = { mu }

	def f_pp(p: AnyRef @polyread): AnyRef @polyread = { p }
	@polyread def f_pp2(p: AnyRef @polyread): AnyRef = { p }

	def dbl(a: AnyRef @polyread)(b: AnyRef @polyread): AnyRef @polyread = { a }

	f_mr(na)
	f_mr(mu)
	f_mr(pr)    // error expected
	f_mr(ro)    // error expected
	f_mr2(na)
	f_mr2(mu)
	f_mr2(pr)   // error expected
	f_mr2(ro)   // error expected
	na = f_mr(mu)   // error expected
	mu = f_mr(mu)   // error expected
	ro = f_mr(mu)
	na = f_mr2(mu)   // error expected
	mu = f_mr2(mu)   // error expected
	ro = f_mr2(mu)
	
	f_rm(na)
	f_rm(mu)
	f_rm(pr)
	f_rm(ro)
	f_rm2(na)
	f_rm2(mu)
	f_rm2(pr)
	f_rm2(ro)
	na = f_rm(na)
	mu = f_rm(mu)
	ro = f_rm(mu)
	na = f_rm2(na)
	mu = f_rm2(mu)
	ro = f_rm2(mu)
	
	f_pp(na)
	f_pp(mu)
	f_pp(pr)
	f_pp(ro)
	na = f_pp(na)
	mu = f_pp(na)
	ro = f_pp(na)
	na = f_pp(mu)
	mu = f_pp(mu)
	ro = f_pp(mu)
	na = f_pp(pr)   // error expected
	mu = f_pp(pr)   // error expected
	ro = f_pp(pr)
	na = f_pp(ro)   // error expected
	mu = f_pp(ro)   // error expected
	ro = f_pp(ro)
	
	mu = dbl(pr)(mu)  // error expected
	mu = dbl(mu)(pr)  // error expected
	mu = dbl(ro)(mu)  // error expected
	mu = dbl(mu)(ro)  // error expected
	
	val k_na = dbl(na) _
	val k_mu = dbl(mu) _
	val k_pr = dbl(pr) _      // (expected result minpolyread)
	val k_ro = dbl(ro) _      // (expected result readonly)
	na = k_na(mu)  // (expected result mutable)
	na = k_na(pr)  // error expected
	na = k_na(ro)  // error expected (expected result readonly)
	na = k_pr(mu)  // error expected (expected result polyread)
	na = k_pr(pr)  // error expected
	na = k_pr(ro)  // error expected (expected result readonly)
	na = k_ro(mu)  // error expected
	na = k_ro(pr)  // error expected
	na = k_ro(ro)  // error expected
	

	val k_pr2 = dbl(pr) _
	k_pr2(mu)
	
	/* Test Partial Applications */
	//def d(@mutable a: AnyRef)(@readonly b: AnyRef): AnyRef @polyread = a
	//val e = d(mu) _
	//e(ro)
	/* AST:
	DefDef(Modifiers(<method>,,List()),d,List(),
		List(List(ValDef(Modifiers(<param>,,List(Apply(Select(New(Ident(mutable)),<init>),List()))),a,Ident(AnyRef),EmptyTree)),
			List(ValDef(Modifiers(<param>,,List(Apply(Select(New(Ident(readonly)),<init>),List()))),b,Ident(AnyRef),EmptyTree))),
		Annotated(Apply(Select(New(Ident(polyread)),<init>),List()),Ident(AnyRef)),
		Ident(a)),

	ValDef(Modifiers(,,List()),e,
		TypeTree[RefinedType(RefinedType(TypeRef(ThisType(module class scala),Function1), scala$Function1$$T1,
			ContraTypeAlias(AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(readonly)),<init>),List())),
			TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)))), scala$Function1$$R,
			CoTypeAlias(AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(polyread)),<init>),List())),
			TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))))],
		Block(
			List(
				ValDef(Modifiers(,,List()),$1$,
					TypeTree[AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(mutable)),<init>),List())),
						TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
					Ident(mu))),
			Block(
				List(DefDef(Modifiers(<synthetic>,,List()),$anonfun,List(),
					List(List(ValDef(
						Modifiers(<param> <synthetic>,,List()),
						b,
						TypeTree[AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(readonly)),<init>),List())),
							TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
						EmptyTree))),
					TypeTree[AnnotatedType(ConcreteAnnotation(Apply(Select(New(Ident(polyread)),<init>),List())),
						TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))],
					Apply(Apply(Ident(d),List(Ident($1$))),List(Ident(b))))),
				Closure(List(),Ident($anonfun),EmptyTree)))),

	Apply(Select(Ident(e),apply),List(Ident(ro)))
	*/

}
}
}