import annotation.tmt._

// 7 errors expected. 8 expected when receiver mutability is checked.

trait listtype4 {
	var m: AnyRef @mutable

	var uu: List[List[AnyRef]]
	var um: List[List[AnyRef @mutable]]
	var ur: List[List[AnyRef @readonly]]
	var mu: List[List[AnyRef] @mutable]
	var mm: List[List[AnyRef @mutable] @mutable]
	var mr: List[List[AnyRef @readonly] @mutable]
	var ru: List[List[AnyRef] @readonly]
	var rm: List[List[AnyRef @mutable] @readonly]
	var rr: List[List[AnyRef @readonly] @readonly]
	
	uu = uu
	uu = um
	uu = ur   // Can't check this one due to imprecision in TMTs
	uu = mu
	uu = mm
	uu = mr   // Can't check this one due to imprecision in TMTs
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
	
	m = mm.head.head
	m = mr.head.head  // error expected
	m = rm.head.head  // error expected: @readonly(rm) is not compatible with @mutable(this)
}

