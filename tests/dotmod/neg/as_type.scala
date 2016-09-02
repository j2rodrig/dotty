import dotty._

object as_type {
  var a: AnyRef = ???

  @asType[Any](a) def m() = {
    val x: AnyRef = a   // error
    a = x  // ok
    val y: Any = a  // ok
    a = y  // error
  }

}