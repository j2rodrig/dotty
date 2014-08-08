class C {
	trait Foreach[+A] extends Any {
		def foreach(f: A => Unit): Unit
	}
	trait Linear[+A] extends Any with Foreach[A] {
		type Tail <: Linear[A]
		def isEmpty: Boolean
		def head: A
		def tail: Tail
	}
	class LinearImpl[+A] extends Linear[A] {
		def foreach(f: A => Unit): Unit = {
			def loop(xs: Linear[A]): Unit =
				if (!xs.isEmpty) { f(xs.head) ; loop(xs.tail) }
			loop(LinearImpl.this)
		}
	}
	class ForeachOperations[A](val xs: Foreach[A]) extends AnyVal {
		def foldr[B](zero: B)(f: (A, B) => B): B = {
			var result = zero
			xs.foreach(x => result = f(x, result))
			result
		}
	}
}