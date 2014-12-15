import annotation.tmt._

// 2 errors expected

trait methcopydef1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	def ud2(p: AnyRef) = u1  // ud2 should be unannotated
	def ud3() = m1  // ud3 should be mutable
	def ud4() = r1  // ud4 should be readonly
	
	def udd2()() = u1  // udd2 should be unannotated
	def udd3()() = m1  // udd3 should be mutable
	def udd4()() = r1  // udd4 should be readonly
	
	u1 = ud2(u1)
	r1 = ud2(u1)
	u1 = udd2()()
	r1 = udd2()()

	u1 = ud3()
	r1 = ud3()
	u1 = udd3()()
	r1 = udd3()()

	u1 = ud4()  // should error
	r1 = ud4()
	u1 = udd4()()  // should error
	r1 = udd4()()
}