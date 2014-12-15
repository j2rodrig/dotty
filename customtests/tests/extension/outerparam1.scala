import annotation.tmt._

// 5 errors expected

trait outerparam1 {
	var u1: AnyRef
	var m1: AnyRef @mutable
	var r1: AnyRef @readonly

	def o1() = {
		@readonly(u1) def o11(): Unit = {
			u1 = r1   // error expected: u1 is not assignable
		}
		@mutable(r1) def o12(): Unit = {   // error expected
			u1 = r1
		}
		@readonly(m1) def o13(): Unit = {
			m1 = r1   // error expected: m1 is not assignable
		}
	}
	
	@readonly(m1) def o2(): Unit = {
		@mutable(m1) def o21(): Unit = {    // error expected: m1 is readonly
			m1 = m1
		}
		def o22(): Unit = {
			m1 = m1    // error expected: m1 is not assignable
		}
	}

	/*@mutable(u1) def um = u1
	@mutable(m1) def mm = m1
	@mutable(r1) def rm = r1   // error expected

	@readonly(u1) def ur = u1
	@readonly(m1) def mr = m1
	@readonly(r1) def rr = r1

	@readonly(u1) def ur2: AnyRef = {
		@mutable(u1) def b = u1  // error expected
		m1 = m1
		b
	}
	@readonly(m1) def mr2 = {
		@mutable(m1) def b = m1  // error expected here
		m1 = m1
		b
	}
	@readonly(r1) def rr2 = {
		@mutable(r1) def b = r1  // error expected
		b
	}

	u1 = ur  // error expected
	m1 = mr  // error expected
	r1 = rr

	u1 = ur2  // error expected
	m1 = mr2  // error expected
	r1 = rr2*/

}