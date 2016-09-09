import dotty._

object pure_basic {

  var a: AnyRef = ???

  def m() = {
    a = ???
  }

  @pure def m2() = {
    a
    a = ???  // error

    var b: AnyRef = ???
    def n2() = {
      a = ???  // error
      b = ???
    }
  }

  class C {
    var x: AnyRef = ???
  }
  val c: C = ???
  def m3() = {
    c.x
    c.x = ???
  }
  @pure def m4() = {
    c.x
    c.x = ???  // error

    class D {
      var x: AnyRef = ???
    }
    val d: D = ???
    def n4() = {
      c.x
      c.x = ???  // error
      d.x
      d.x = ???
    }
  }

}