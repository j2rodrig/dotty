import annotation.tmt._

trait misc {
	trait C {
		def f: AnyRef
	}

	val c: C	
	var m = c.f

	//def f(p: misc) = p
	
	//var u: AnyRef
	//f(u)
}