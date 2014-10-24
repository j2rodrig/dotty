import annotation.tmt._

// 3 errors expected

trait assignpath1 {

	trait PathObj {
		var u1: PathObj
		var m1: PathObj @mutable
		var r1: PathObj @readonly
	}
	val up: PathObj
	val mp: PathObj @mutable
	val rp: PathObj @readonly
	
	up.u1 = up
	up.m1 = up
	up.r1 = up

	mp.u1 = up
	mp.m1 = up
	mp.r1 = up

	rp.u1 = up  // should error
	rp.m1 = up  // should error
	rp.r1 = up  // should error
}