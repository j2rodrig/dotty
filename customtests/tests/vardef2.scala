import annotation.tmt._

// 1 error expected

trait vardef2 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	@mutable var m2 = u1
	@mutable var m3 = m1
	@mutable var m4 = r1  // should error
	@readonly var r2 = u1
	@readonly var r3 = m1
	@readonly var r4 = r1
}