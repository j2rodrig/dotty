import annotation.tmt._

// 

trait inheritance1 {

	trait T {
		type =+ = AnyRef @readonly
	}

	//trait T[+A <: AnyRef @readonly] {}

}

