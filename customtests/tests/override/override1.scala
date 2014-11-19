import annotation.tmt._

// 3 errors expected

trait override1 {

	class C {
		def method1(p: C): C = new C
		def method2(p: C @readonly): C = new C
		
		def x = this
	}

	class D extends C {
		def method1(p: C): D @readonly = new D    // error expected: result types do not match
		def method2(p: C): D = new D              // error expected: parameter p does not match
		
		@readonly def x = this                    // error expected: result types do not match
	}
	
	class E extends D {}
	
}