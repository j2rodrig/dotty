import annotation.tmt._

// 2 errors expected

trait results1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	def f_u1(): AnyRef
	def f_m1(): AnyRef @mutable
	def f_r1(): AnyRef @readonly
	
	u1 = f_u1()
	u1 = f_m1()
	u1 = f_r1()  // should error
	m1 = f_u1()
	m1 = f_m1()
	m1 = f_r1()  // should error
	r1 = f_u1()
	r1 = f_m1()
	r1 = f_r1()
}