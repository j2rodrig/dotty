import annotation.tmt._

// 3 errors expected

trait varcopydef1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	var u2 = u1  // u2 should be unannotated
	var u3 = m1  // u3 should be mutable
	var u4 = r1  // u4 should be readonly
	
	u2 = r1  // should error
	u1 = u2
	r1 = u2

	u3 = r1  // should error
	u1 = u3
	r1 = u3

	u4 = r1
	u1 = u4  // should error
	r1 = u4
}