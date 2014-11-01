import annotation.tmt._

// 2 errors expected

trait vardef1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	var u2: AnyRef = u1
	var u3: AnyRef = m1
	var u4: AnyRef = r1  // should error
	var m2: AnyRef @mutable = u1
	var m3: AnyRef @mutable = m1
	var m4: AnyRef @mutable = r1  // should error
	var r2: AnyRef @readonly = u1
	var r3: AnyRef @readonly = m1
	var r4: AnyRef @readonly = r1
}