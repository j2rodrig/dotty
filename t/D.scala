package test

import annotation.{StaticAnnotation, TypeConstraint}

class readonly extends TypeConstraint {}
class polyread extends TypeConstraint {}
class mutable extends TypeConstraint {}

//class readonly(args: Any*) extends StaticAnnotation with TypeConstraint {}
//class polyread(args: Any*) extends StaticAnnotation with TypeConstraint {}
//class mutable(args: Any*) extends StaticAnnotation with TypeConstraint {}


object D {}

trait C {
	var ro: AnyRef @readonly
	var pr: AnyRef @polyread
	var mu: AnyRef @mutable
	
	@readonly var ro2: AnyRef
	@mutable var mu2: AnyRef
	
	var rr: List[AnyRef @readonly] @readonly
	var rm: List[AnyRef @mutable] @readonly
	var mr: List[AnyRef @readonly] @mutable
	var mm: List[AnyRef @mutable] @mutable
	
/*	var test: Map[AnyRef @mutable,AnyRef @mutable]
RefinedType(
	RefinedType(
		TypeRef(ThisType(module class immutable),Map),
		scala$collection$immutable$Map$$A,
		TypeAlias(AnnotatedType(
			ConcreteAnnotation(Apply(Select(New(Ident(mutable)),<init>),List())),
			TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)))),
	scala$collection$immutable$Map$$B,
	CoTypeAlias(AnnotatedType(
		ConcreteAnnotation(Apply(Select(New(Ident(mutable)),<init>),List())),
		TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef))))*/

	def m1() = {
		// Basic Annotations
		ro = ro
		ro = pr
		ro = mu
		pr = ro   // should error
		pr = pr
		pr = mu
		mu = ro   // should error
		mu = pr   // should error
		mu = mu
		
		// Prefix Annotations
		ro2 = ro2
		ro2 = mu2
		ro2 = ro
		ro2 = mu
		mu2 = ro2  // should error
		mu2 = mu2
		mu2 = ro   // should error
		mu2 = mu
		
		// Generic Type Annotations
		rr = rr
		rr = rm
		rr = mr
		rr = mm
		rm = rr   // should error
		rm = rm
		rm = mr   // should error
		rm = mm
		mr = rr   // should error
		mr = rm   // should error
		mr = mr
		mr = mm
		mm = rr   // should error
		mm = rm   // should error
		mm = mr   // should error
		mm = mm
	}

/*AnnotatedType(
	ConcreteAnnotation(Apply(Select(New(Ident(readonly)),<init>),List())),
	RefinedType(
		TypeRef(ThisType(module class immutable),List),
		scala$collection$immutable$List$$A,
		CoTypeAlias(AnnotatedType(
			ConcreteAnnotation(Apply(Select(New(Ident(mutable)),<init>),List())),
			TypeRef(TermRef(ThisType(module class <root>),scala),AnyRef)))))*/

	val f: Option[Int] = new Option[Int]
	
	var v: AnyRef = f
	
	val g = f
	val h: AnyRef
	var c = new C
	def m(p1: AnyRef): AnyRef = None
	
	//@mutable(System.out,System.in) def m(@readonly p1: AnyRef @mutable): AnyRef @mutable = None


	// DOESN'T WORK:
	//@mutable(C) def m(): ... // error: not found: C
	
	// DOES WORK:
	//@mutable(c) def m(): ... // OK because c is a literal variable
	//@mutable(this) def m(): ...
	//@mutable(C.this) def m(): ...
	//@mutable(None) def m(): ...
}