import annotation._

trait K {

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
	
	class C {
		def f(a: Int => Int): Unit = { a }
		def f1[T](): Unit = {}
	}
	
	def outer(list: List[AnyRef] @mutable, c: C @mutable): Int = {
		var acc = 0
		//val list_alias = list
		//list.foreach ({ m: AnyRef => acc = acc + 1 })
		c.f({ m => m+1 })
		c.f1[C]()
		acc
	}
	
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