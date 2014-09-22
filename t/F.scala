import annotation.{StaticAnnotation, TypeConstraint}

class readonly extends TypeConstraint {}
class polyread extends TypeConstraint {}
class mutable extends TypeConstraint {}

trait F {

	trait Base {
		def m(lst: List[Int]): List[Double]
		
		def n: List[Double]
	}
	
	trait Derived extends Base {
		def m2(lst: List[Int]): List[Int] = m(lst)
		
		def n2: List[Int] = n
	}
	
}