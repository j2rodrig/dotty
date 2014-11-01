import annotation.tmt._

// 2 errors expected

trait methdef2 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	@mutable def md2() = u1
	@mutable def md3() = m1
	@mutable def md4() = r1  // should error
	@readonly def rd2() = u1
	@readonly def rd3() = m1
	@readonly def rd4() = r1
	
	@mutable def mdd2()() = u1
	@mutable def mdd3()() = m1
	@mutable def mdd4()() = r1  // should error
	@readonly def rdd2()() = u1
	@readonly def rdd3()() = m1
	@readonly def rdd4()() = r1
}