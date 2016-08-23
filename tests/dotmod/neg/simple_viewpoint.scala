import dotty._

object simple_viewpoint {

  class C {
    val x: AnyRef = ???
    @polyread def y = x
  }
  val c: C @readonly = ???
  val c_x: AnyRef = c.x  // error
  val c_y: AnyRef = c.y  // error

  val c2_x: AnyRef @readonly = c.x  // ok
  val c2_y: AnyRef @readonly = c.y  // ok


  class D {
    val x: C = ???
    @polyread def y = x
  }
  val d: D @readonly = ???
  val d_x_x: AnyRef = d.x.x  // error
  val d_x_y: AnyRef = d.x.y  // error
  val d_y_x: AnyRef = d.y.x  // error
  val d_y_y: AnyRef = d.y.y  // error

}