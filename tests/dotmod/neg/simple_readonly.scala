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

  val r: AnyRef { type __MUTABILITY__ = mutable } { type __MUTABILITY__ <: readonly } = ???
  val s: AnyRef { type __MUTABILITY__ <: readonly } = r
}
