import annotation.tmt._

trait I {
	trait A {
		var u: AnyRef
		var m: AnyRef @mutable
		var r: AnyRef @readonly
		var a: A
	}
	
	def try1(ar: A @readonly, ap: A @polyread, am: A @mutable, au: A) = {
		// Simple selection testing
		au.u = au.u
		au.u = au.m
		au.u = au.r   // error expected
		
		au.u = am.u
		au.u = am.m
		au.u = am.r   // error expected
		
		au.u = ap.u   // error expected
		au.u = ap.m   // error expected
		au.u = ap.r   // error expected
		
		au.u = ar.u   // error expected
		au.u = ar.m   // error expected
		au.u = ar.r   // error expected
		
		
		// Selection assignment mutability
		ar.u = au.r   // error expected
		ar.a.u = au   // error expected
		ap.a.u = ar   // error expected
	}
}