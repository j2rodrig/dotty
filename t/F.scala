package t

trait F {

	trait Base {
		def m(lst: List[Int]): List[Int]
		def n(an: AnyRef): List[Int]
		
		val x: List[Int]
	}
	
	trait Derived extends Base {
		override def m(an: AnyRef): List[Int]
		override def n(lst: List[Int]): List[Int]
		
		val x: AnyRef
	}

}