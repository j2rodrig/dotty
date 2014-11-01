import annotation.tmt._

// 9 errors expected. 12 expected when receiver mutability is checked.

trait listtype3 {
	var m: AnyRef @mutable
	
	trait L[+ElemType] {
		val head: ElemType
		def head2: ElemType = head  // constraint leak (if ElemType is instantiated to @readonly)?
		// we want to prevent violating user's constraint if local things are returned...
		// we want to prevent violating user's constraint if local things are modified...
		// but we want to allow returning/modifying fresh objects...
		// so what do we do?
		// Answer:
		//  check for the mutability of this. If head2 is @mutable(this), then callers with @readonly(this) are disallowed.
		//  @polyread(this) allows returning of local things (but not mutation).
	}
	trait L2[+A <: AnyRef @readonly] extends L[A] {
		val head: A
		def head2: A
	}
	trait L3[+B] extends L2[B] {
	}
	val l2: L2[AnyRef]
	m = l2.head          // no error because L2.A is instantiated to a non-readonly AnyRef
	
	var uu: L[AnyRef]
	var ur: L[AnyRef @readonly]
	var ru: L[AnyRef] @readonly
	m = uu.head
	m = ur.head   // error expected
	m = ru.head   // error expected
	m = uu.head2
	m = ur.head2  // error expected
	m = ru.head2  // error expected: @readonly(ru) is not compatible with @mutable(this)
	
	type T = AnyRef
	var uut: L[T]
	var urt: L[T @readonly]
	var rut: L[T] @readonly
	m = uut.head
	m = urt.head   // error expected
	m = rut.head   // error expected
	m = uut.head2
	m = urt.head2  // error expected
	m = rut.head2  // error expected: @readonly(rut) is not compatible with @mutable(this)
	
	type T2 = AnyRef @readonly
	var urt2: L[T2]
	var umt2: L[T2 @mutable]
	var rmt2: L[T2 @mutable] @readonly
	m = urt2.head   // error expected
	m = umt2.head
	m = rmt2.head   // error expected
	m = urt2.head2  // error expected
	m = umt2.head2
	m = rmt2.head2  // error expected: @readonly(rmt2) is not compatible with @mutable(this)
}