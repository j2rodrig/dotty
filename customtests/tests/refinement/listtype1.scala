import annotation.tmt._

// 9 errors expected. 10 expected when receiver mutability is checked.

trait listtype1 {
	var m: AnyRef @mutable

	var uu: List[AnyRef]
	var um: List[AnyRef @mutable]
	var ur: List[AnyRef @readonly]
	var mu: List[AnyRef] @mutable
	var mm: List[AnyRef @mutable] @mutable
	var mr: List[AnyRef @readonly] @mutable
	var ru: List[AnyRef] @readonly
	var rm: List[AnyRef @mutable] @readonly
	var rr: List[AnyRef @readonly] @readonly
	
	uu = uu
	uu = um
	uu = ur   // error expected
	uu = mu
	uu = mm
	uu = mr   // error expected
	uu = ru   // error expected
	uu = rm   // error expected
	uu = rr   // error expected
	
	ur = mr
	ur = ru   // error expected
	ur = rm   // error expected
	ur = rr   // error expected

	rr = mr
	rr = ru
	rr = rm
	rr = rr
	
	m = mm.head
	m = mr.head  // error expected
	m = rm.head  // error expected: @readonly(rm) is not compatible with @mutable(this)
}