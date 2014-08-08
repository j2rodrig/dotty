package test

import annotation.{StaticAnnotation, TypeConstraint}

class readonly extends StaticAnnotation with TypeConstraint {}


class List[A] {}

trait B {
	val list: List[Int @readonly] @readonly
	
	val a: (() => Unit) @readonly
}