import annotation.tmt._

// 11 errors expected. 12 expected when receiver mutability is checked.

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
	
	var mmm: List[List[List[AnyRef @mutable] @mutable] @mutable]
	var mmr: List[List[List[AnyRef @readonly] @mutable] @mutable]
	
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
	
	mmm = mmr  // error expected
	mmr = mmm
	
	m = mm.head.head
	m = mr.head.head  // error expected
	m = rm.head.head  // error expected: @readonly(rm) is not compatible with @mutable(this)
	
	m = mmm.head.head.head
	mm = mmr.head     // error expected
}

