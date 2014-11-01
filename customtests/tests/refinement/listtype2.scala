import annotation.tmt._

// 6 errors expected, but only 5 actual due to bug in dotty

trait listtype2 {

	trait PolyBug2 {
		var unconstrained: AnyRef
		trait R {
			type T <: AnyRef
			val constrained: T @readonly
			unconstrained = this.constrained  // causes an error (constraint is retained)
		}
		val r: R
		unconstrained = r.constrained         // CONSTRAINT ANNOTATION IS DROPPED HERE: bug in dotty
	}
	
	trait PolyBug2fixed {
		var unconstrained: AnyRef
		trait R {
			type T <: AnyRef
			val constrained: T @readonly
			unconstrained = this.constrained  // causes an error (constraint is retained)
		}
		val r: R { type T <: AnyRef @readonly }
		unconstrained = r.constrained         // causes an error (because @readonly on refined type is retained)
	}
	
	trait PolyBug2fixed2 {
		var unconstrained: AnyRef
		trait R {
			type T <: AnyRef @readonly
			val constrained: T
			unconstrained = this.constrained  // causes an error (constraint is retained)
		}
		val r: R
		unconstrained = r.constrained         // causes an error (because @readonly on AnyRef is retained)
	}
	
	/*
	trait L {
		type T <: AnyRef
		//type S = T @readonly
		var s: T @readonly                                 // @readonly(TypeRef(T => TypeRef(Nothing) to @mutable(TypeRef(AnyRef => TypeAlias(TypeRef(Object))))))
		m = s
		m = this.s
	}
	var l: L
	m = l.s
	//var ls = l.s
	// with l:L         :  TermRef(          TypeRef(L) .s =>           TypeRef(T => TypeRef(Nothing) to @mutable(TypeRef(AnyRef => TypeAlias(TypeRef(Object))))) )
	//  - w/ viewpoint  :  TermRef(          TypeRef(L) .s => @mutable (TypeRef(T => TypeRef(Nothing) to @mutable(TypeRef(AnyRef => TypeAlias(TypeRef(Object)))))))
	// with l:L @mutable:  TermRef(@mutable (TypeRef(L)).s =>           TypeRef(T => TypeRef(Nothing) to @mutable(TypeRef(AnyRef => TypeAlias(TypeRef(Object))))) )
	//  - w/ viewpoint  :  TermRef(@mutable (TypeRef(L)).s => @mutable (TypeRef(T => TypeRef(Nothing) to @mutable(TypeRef(AnyRef => TypeAlias(TypeRef(Object)))))))
	// with l:L @readonly: TermRef(@readonly(TypeRef(L)).s => @readonly(TypeRef(T => TypeRef(Nothing) to @mutable(TypeRef(AnyRef => TypeAlias(TypeRef(Object)))))))
	*/

	/*
	trait L {
		type T <: AnyRef @readonly
		var t: T
	}
	trait L1 extends L {
		type T <: AnyRef @mutable  // error expected: T is invariant
		type S = super.T
		type S1 = super.T @mutable
		var s: S
		var s1: S1
		u = s   // error expected: type U inherits @readonly
		u = s1
	}
	
	var l: L
	var l1: L1
	
	u = l.t    // error expected
	u = l1.t
	
	u = l1.s   // error expected: type U inherits @readonly
	u = l1.s1*/
	
	//var l1: L1
	//q(l1)
	//var l: L = l1
	//q(l)
}

// s1 where S1 = T @mutable:
// TermRef(this   (L1).s1 => TypeRef(S1 => TypeAlias(@mutable(TypeRef(T => TypeRef(Nothing) to @mutable (TypeRef(AnyRef => TypeAlias(TypeRef(Object)))))))))

// l1.s1 where S1 = T @mutable:
// TermRef(TypeRef(L1).s1 => TypeRef(S1 => TypeAlias(         TypeRef(T => TypeRef(Nothing) to @mutable (TypeRef(AnyRef => TypeAlias(TypeRef(Object))))) )))

// s1 where S1 = super.T @mutable:
// TermRef(this   (L1).s1 => TypeRef(S1 => TypeAlias(@mutable(TypeRef(T => TypeRef(Nothing) to @readonly(TypeRef(AnyRef => TypeAlias(TypeRef(Object)))))))))

// l1.s1 where S1 = super.T @mutable:
// TermRef(TypeRef(L1).s1 => TypeRef(S1 => TypeAlias(         TypeRef(T => TypeRef(Nothing) to @readonly(TypeRef(AnyRef => TypeAlias(TypeRef(Object))))) )))
