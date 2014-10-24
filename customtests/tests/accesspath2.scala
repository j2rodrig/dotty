import annotation.tmt._

// 14 errors expected

trait accesspath2 {

	trait PathObj {
		var u1: PathObj
		var m1: PathObj @mutable
		var r1: PathObj @readonly
	}
	var up: PathObj
	var mp: PathObj @mutable
	var rp: PathObj @readonly
	
	up.u1.u1 = up
	mp.u1.u1 = up
	rp.u1.u1 = up  // should error
	up.u1.u1 = rp  // should error
	mp.u1.u1 = rp  // should error
	rp.u1.u1 = rp  // should error
	
	up.m1.u1 = up
	mp.m1.u1 = up
	rp.m1.u1 = up  // should error
	up.m1.u1 = rp  // should error
	mp.m1.u1 = rp  // should error
	rp.m1.u1 = rp  // should error
	
	up.r1.u1 = up  // should error
	mp.r1.u1 = up  // should error
	rp.r1.u1 = up  // should error
	up.r1.u1 = rp  // should error
	mp.r1.u1 = rp  // should error
	rp.r1.u1 = rp  // should error
}