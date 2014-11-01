import annotation.tmt._

// 10 errors expected

trait accesspath1 {

	trait PathObj {
		var u1: PathObj
		var m1: PathObj @mutable
		var r1: PathObj @readonly
	}
	var up: PathObj
	var mp: PathObj @mutable
	var rp: PathObj @readonly
	
	up = up.u1
	up = up.m1
	up = up.r1  // should error
	
	up = mp.u1
	up = mp.m1
	up = mp.r1  // should error

	up = rp.u1  // should error
	up = rp.m1  // should error
	up = rp.r1  // should error
	
	up = up.u1.u1
	up = mp.u1.u1
	up = rp.u1.u1  // should error

	up = up.m1.u1
	up = mp.m1.u1
	up = rp.m1.u1  // should error

	up = up.r1.u1  // should error
	up = mp.r1.u1  // should error
	up = rp.r1.u1  // should error
}