//import annotation.tmt._

trait override1 {

	trait C {
		type E <: AnyRef
		//def method1(p: C): C = new D
	}
	
	trait D extends C {
		type E >: Any
		//override def method1(p: C): D = new D
	}
	
	
	class X extends D { type E = Int }
}