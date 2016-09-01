import dotty._

object as_final {

  var a: AnyRef = ???

  @asFinal(a) def m() = {
    a = ???  // error
  }

  @asFinal(a) class C {
    a = ???  // error
  }

  class D {
    var x: AnyRef = ???
    @polyread def m() = {
      x = ???   // error
      a = ???
    }
    @asFinal(x) def n() = {
      x = ???   // error
      a = ???
    }
  }
}