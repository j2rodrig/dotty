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
	
	def polytest3[T](param1: List[T @readonly] @readonly): List[T @readonly] @readonly
	
	//def polytest4[T](param1: Map[AnyRef @readonly,T @readonly] @readonly): T @readonly
	
	/*def polyreadtest1(param1: A @polyread): A @polyread
	
	polyreadtest1(m)
	polyreadtest1(r)
	m = polyreadtest1(m)
	m = polyreadtest1(r)
	r = polyreadtest1(m)
	r = polyreadtest1(r)
	r = polyreadtest1(polyreadtest1(m))
	r = polyreadtest1(polyreadtest1(r))*/
	
	def mx[X <: Any @polyread](a: List[X @readonly] @mutable): List[X @mutable] @readonly
	
	//def x = mx[List[A] @polyread] _
	
	//x
}