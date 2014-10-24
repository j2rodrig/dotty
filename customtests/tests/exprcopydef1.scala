import annotation.tmt._

// 1 error expected

trait exprcopydef1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	def ue2 = u1  // u2 should be unannotated
	def ue3 = m1  // u3 should be mutable
	def ue4 = r1  // u4 should be readonly
	
	u1 = ue2
	r1 = ue2

	u1 = ue3
	r1 = ue3

	u1 = ue4  // should error
	r1 = ue4
}