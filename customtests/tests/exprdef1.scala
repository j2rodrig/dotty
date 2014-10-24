import annotation.tmt._

// 2 errors expected

trait exprdef1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly
	
	def ue2: AnyRef = u1
	def ue3: AnyRef = m1
	def ue4: AnyRef = r1  // should error
	def me2: AnyRef @mutable = u1
	def me3: AnyRef @mutable = m1
	def me4: AnyRef @mutable = r1  // should error
	def re2: AnyRef @readonly = u1
	def re3: AnyRef @readonly = m1
	def re4: AnyRef @readonly = r1
}