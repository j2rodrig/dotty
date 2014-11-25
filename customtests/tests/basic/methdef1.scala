import annotation.tmt._

// 4 errors expected

trait methdef1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly

	/*@readonly(m1) def ym: Unit = {
		def xm: Unit = {
			this.m1 = this.m1
		}
	}*/
	
	def ud2(): AnyRef = u1
	def ud3(): AnyRef = m1
	def ud4(): AnyRef = r1  // should error
	def md2(): AnyRef @mutable = u1
	def md3(): AnyRef @mutable = m1
	def md4(): AnyRef @mutable = r1  // should error
	def rd2(): AnyRef @readonly = u1
	def rd3(): AnyRef @readonly = m1
	def rd4(): AnyRef @readonly = r1
	
	def udd2()(): AnyRef = u1
	def udd3()(): AnyRef = m1
	def udd4()(): AnyRef = r1  // should error
	def mdd2()(): AnyRef @mutable = u1
	def mdd3()(): AnyRef @mutable = m1
	def mdd4()(): AnyRef @mutable = r1  // should error
	def rdd2()(): AnyRef @readonly = u1
	def rdd3()(): AnyRef @readonly = m1
	def rdd4()(): AnyRef @readonly = r1
}