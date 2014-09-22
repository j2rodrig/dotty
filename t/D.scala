//package test

import annotation._

//import annotation.{StaticAnnotation, TypeConstraint}

//class readonly extends TypeConstraint {}
//class polyread extends TypeConstraint {}
//class mutable extends TypeConstraint {}

//class readonly(args: Any*) extends StaticAnnotation with TypeConstraint {}
//class polyread(args: Any*) extends StaticAnnotation with TypeConstraint {}
//class mutable(args: Any*) extends StaticAnnotation with TypeConstraint {}


object D {}

trait C {
	var ro: AnyRef @readonly
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

	def m1(pr: AnyRef @polyread) = {
		// Basic Annotations
		ro = ro
		ro = pr
		ro = mu
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

		var mu3 = mu
		var pr3 = pr
		var ro3 = ro
		mu3 = pr3   // should error
		pr3 = ro3
		ro3 = pr3
		@mutable var mPre1 = mu
		@mutable var mPre2 = pr  // should error
		@mutable var mPre3 = ro  // should error
		@polyread var pPre1 = mu  // should error
		@polyread var pPre2 = pr  // should error
		@polyread var pPre3 = ro  // should error
		var ptPre1: AnyRef @polyread = mu  // should error (or silently convert to readonly)
		var ptPre2: AnyRef @polyread = pr  // should error (or silently convert to readonly)
		var ptPre3: AnyRef @polyread = ro  // should error (or silently convert to readonly)
		@readonly var rPre1 = mu
		@readonly var rPre2 = pr
		@readonly var rPre3 = ro
		/*** Prefix annotations on a variable definition can strengthen the symbol's type, but not weaken it.
		In contrast, annotations on a type expression are not checked -- they override
		the previously-inferred top-level mutability.
		**/
		
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


	//---------
	// METHODS
	//---------
	// DOESN'T WORK:
	//@mutable(C) def m(): ... // error: not found: C
	//
	// DOES WORK:
	//@mutable(c) def m(): ... // OK because c is a literal variable
	//@mutable(this) def m(): ...
	//@mutable(C.this) def m(): ...
	//@mutable(None) def m(): ...
	//@mutable(System.out,System.in) def m(@readonly p1: AnyRef @mutable): AnyRef @mutable = None
}