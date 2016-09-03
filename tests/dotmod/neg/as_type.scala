import dotty._

object as_type {
  var a: AnyRef = ???

  @asType[Any](a) def m() = {
    val x: AnyRef = a   // error
    a = x  // ok
    val y: Any = a  // ok
    a = y  // error
  }

  class C

  @asType[C](a)   // error
  def n() = {
  }

  @asType[Any](this)  // error
  def n1() = {
  }

  @asType[Any](a) def o() = {
    @asType[AnyRef](a)  // error
    def o1() = {
    }
  }

}