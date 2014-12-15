import annotation.tmt._

// 4 errors expected

trait inheritOuterParam {
	var outerU: AnyRef
	var outerM: AnyRef @mutable
	var outerR: AnyRef @readonly

	class C {
		@readonly(outerU) def o1(p: C): Unit = {}
		@readonly(outerM) def o2(p: C): Unit = {}
		@readonly(outerR) def o3(p: C): Unit = {}
		@mutable(outerU) def o4(p: C): Unit = {}
		@mutable(outerM) def o5(p: C): Unit = {}
		//@mutable(outerR) def o6(p: C): Unit = {} // not included in test because override checking requires zero typer errors
		
		@readonly(outerU) def o7(p: C): Unit = {}
		@mutable(outerU) def o8(p: C): Unit = {}
		
		@readonly(this) def o9(p: C): Unit = {}
		@mutable(this) def o10(p: C): Unit = {}
	}

	class D extends C {
		@mutable(outerU) def o1(p: C): Unit = {}   // error expected
		@mutable(outerM) def o2(p: C): Unit = {}   // error expected
		//@mutable(outerR) def o3(p: C): Unit = {} // not included in test because override checking requires zero typer errors
		@readonly(outerU) def o4(p: C): Unit = {}
		@readonly(outerM) def o5(p: C): Unit = {}
		@readonly(outerR) def o6(p: C): Unit = {}
		
		def o7(p: C): Unit = {}  // error expected
		def o8(p: C): Unit = {}  // error expected, but only if all annotated references must be explicitly annotated on the overriding method
		
		def o9(p: C): Unit = {}  // error expected
		def o10(p: C): Unit = {}  // error expected, but only if all annotated references must be explicitly annotated on the overriding method
	}

	class E extends D {}
}