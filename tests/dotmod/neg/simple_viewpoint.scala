import dotty.readonly

object simple_viewpoint {

  class C {
    val x: Any = ???
    def y: Any = ???
  }
  val c: C @readonly = ???
  val c_x: Any = c.x  // error
  val c_y: Any = c.y  // error


}