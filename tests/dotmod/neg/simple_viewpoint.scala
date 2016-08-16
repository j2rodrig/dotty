import dotty.{readonly, mutable}

object simple_viewpoint {

  class C {
    val x: Any = ???
    @readonly def y: Any = ???
  }
  val c: C @readonly = ???
  val c_x: Any = c.x  // error
  val c_y: Any = c.y  // error

  val c2_x: Any @readonly = c.x  // ok
  val c2_y: Any @readonly = c.y  // ok


  class D {
    val x: C = ???
    @readonly def y: C = ???
  }
  val d: D @readonly = ???
  val d_x_x: Any = d.x.x  // error
  val d_x_y: Any = d.x.y  // error
  val d_y_x: Any = d.y.x  // error
  val d_y_y: Any = d.y.y  // error

  val r: Any { type __MUTABILITY__ = mutable } { type __MUTABILITY__ <: readonly } = ???
  val s: Any { type __MUTABILITY__ <: readonly } = r
}