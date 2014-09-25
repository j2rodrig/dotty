import annotation._

trait K {

	class C(var cp1: AnyRef @polyread) {
		@polyread var alias_cp1 = cp1
		def retF() = cp1
	}
	trait Upward { def apply(): Unit }
	def Cdef(cp1: AnyRef @polyread): Upward = {
		@polyread var alias_cp1 = cp1
		new Upward {
			def apply(): Unit = { }
		}
	}
	
	var km: AnyRef @mutable
	var kpr: AnyRef @polyread
	var kr: AnyRef @readonly
	
	
	
	//type T

	/*def afunction(afp1: AnyRef @polyread): AnyRef @polyread = {
		def anested(anp1: AnyRef @polyread): AnyRef @polyread = {
			anp1
			//if (true) anp1 else afp1
		}
		anested(afp1)
	}*/
	
	/*def afunctionparam[T](afp1: T): T = {
		def anestedparam[T](anp1: T): T = {
			if (true) anp1 else afp1
		}
		anestedparam(afp1)
	}*/
	
	/*val aclosure: (AnyRef @readonly => AnyRef @readonly) = { acp1 => afunction(acp1) }
	
	val aclosure2: Function1[AnyRef @polyread, AnyRef @polyread] = { acp1 => afunction(acp1) }

	val aclosure3 = { acp1: AnyRef @polyread => afunction(acp1) }
	
	def fn(m: AnyRef @mutable, p: AnyRef @polyread, r: AnyRef @readonly): AnyRef @polyread = {
		aclosure(p)
		aclosure2(p)
		aclosure3(p)
	}*/
	
	def dbl(dp1: AnyRef @polyread)(dp2: AnyRef @polyread): AnyRef @polyread
	
	def fn2(m: AnyRef @mutable, p: AnyRef @polyread, r: AnyRef @readonly): AnyRef @polyread = {
		/*dbl(m)(m)
		dbl(m)(r)
		dbl(m)(p)
		dbl(r)(m)
		dbl(r)(r)
		dbl(r)(p)
		dbl(p)(m)
		dbl(p)(r)
		dbl(p)(p)*/
		
		val dp = dbl(p) _   // should have type (@polyread[?])@polyread
		//val dp: AnyRef @polyread => AnyRef @polyread = dbl(m) _
		dp(m)
		
		class X {
			val temp1 = p
			def apply(v1: AnyRef @polyread): AnyRef @polyread = dbl(temp1)(v1)
		}
		val x = new X()
		x(m)
	}
	
}