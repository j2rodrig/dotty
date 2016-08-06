import dotty.{readonly, mutable}

object simple_viewpoint {

  class C {
    val x: Any = ???
    def y: Any = ???
  }
  val c: C @readonly = ???
  val c_x: Any = c.x  // error
  val c_y: Any = c.y  // error


  class D {
    val x: C = ???
    def y: C = ???
  }
  val d: D @readonly = ???
  val d_x_x: Any = d.x.x  // error
  val d_x_y: Any = d.x.y  // error
  val d_y_x: Any = d.y.x  // error
  val d_y_y: Any = d.y.y  // error

  val r: Any { type _M = mutable } { type _M = readonly } = ???
  val s: Any { type _M = readonly } = r
}