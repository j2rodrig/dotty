import annotation.tmt._

// 2 errors expected

trait arguments1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	def fu1(up1: AnyRef): Unit
	def fm1(@mutable mp1: AnyRef): Unit
	def fr1(@readonly rp1: AnyRef): Unit
	
	fu1(u1)
	fu1(m1)
	fu1(r1)  // should error
	fm1(u1)
	fm1(m1)
	fm1(r1)  // should error
	fr1(u1)
	fr1(m1)
	fr1(r1)
}