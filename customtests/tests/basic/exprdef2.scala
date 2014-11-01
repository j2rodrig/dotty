import annotation.tmt._

// 1 error expected

trait exprdef2 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	@mutable def me2 = u1
	@mutable def me3 = m1
	@mutable def me4 = r1  // should error
	@readonly def re2 = u1
	@readonly def re3 = m1
	@readonly def re4 = r1
}