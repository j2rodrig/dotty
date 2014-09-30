import annotation.tmt._

trait PolyMethod {
	type A = Any
	
	var m: A @mutable
	var r: A @readonly

	def apap(p1: A @polyread): A @polyread = { p1 }
	
	apap(m)
	apap(r)
	
	type T
	def polytest1(param1: T @readonly): T @readonly
	
	def polytest2[T](param1: T @readonly): T @readonly
	
	def mx[X <: Any @polyread](a: List[X @readonly] @mutable): List[X @mutable] @readonly
	
	//def x = mx[List[A] @polyread] _
	
	//x
}