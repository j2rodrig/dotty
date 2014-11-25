import annotation.tmt._

//  errors expected

trait inheritOuterParam {
	var outerU: AnyRef
	var outerM: AnyRef @mutable
	var outerR: AnyRef @readonly

	class C {
		@readonly(outerU) def o1(p: C): Unit = {}
		@readonly(outerM) def o2(p: C): Unit = {}
	}

	class D extends C {
		@mutable(outerU) def o1(p: C): Unit = {}
		@mutable(outerM) def o2(p: C): Unit = {}
	}

	class E extends D {}
}