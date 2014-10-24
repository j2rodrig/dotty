import annotation.tmt._

// 2 errors expected

trait assignvars2 {
	var u1: AnyRef
	@mutable var m1: AnyRef
	@readonly var r1: AnyRef
	u1 = u1
	u1 = m1
	u1 = r1  // should error
	m1 = u1
	m1 = m1
	m1 = r1  // should error
	r1 = u1
	r1 = m1
	r1 = r1
}