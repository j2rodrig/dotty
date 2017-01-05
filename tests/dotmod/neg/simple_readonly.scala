import dotty.{mutable, readonly}

object simple_readonly {
  class C
  val c: C = null  // ok
  val d: C @readonly = new C  // ok
  val e: C = d  // error
  val f: C @readonly = d  // ok
  val g: C = c  // ok
  val h: C @readonly @mutable = c  // ok
  val i: C = h  // ok
  val j: C @readonly @mutable = d  // error

  // Refinement checking
  val r: AnyRef { type __MUTABILITY__ = mutable } { type __MUTABILITY__ <: readonly } = ???
  val s: AnyRef { type __MUTABILITY__ <: readonly } = r
  val t: AnyRef { type __MUTABILITY__ = mutable } = r   // ok
  val u: AnyRef { type __MUTABILITY__ = mutable } = s   // error

  val v_r: Any { type __MUTABILITY__ = readonly } = ???
  val v_m1: Any { type __MUTABILITY__ = mutable } { type __MUTABILITY__ >: readonly <: readonly } = v_r   // error
  val v_m2: Any { type __MUTABILITY__ = mutable } { type __MUTABILITY__ = readonly } = v_r   // ok


  //val v_r: Any { type M = readonly } = ???
  //val v_m1: Any { type M >: mutable } { type M >: readonly <: readonly } = v_r   // ok? not sure if this is correct
  //val v_m2: Any { type M >: mutable } { type M = readonly } = v_r   // ok

  class D {
    type M >: readonly
  }
  val d1: D { type M = mutable } = ???  // ok, even though refined type has bad bounds
  val d2: D { type M = mutable } = new D { type M = mutable } // refchecks-error: new object has bad bounds

}
