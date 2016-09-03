import dotty._

object as_final {

  var a: AnyRef = ???

  @asFinal(a) def m() = {
    a = ???  // error
  }

  @asFinal(a) class C {
    a = ???  // error
  }

  @asFinal(this)  // error
  def n() = {
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

  def M() = {
    var y: AnyRef = ???
    @asFinal(y) // why can't we find y here?...  // error shouldn't happen here
    def n() = {
      y = ??? // if we assign to y here?  // should error here
    }
  }

  def M0() = {
    var y: AnyRef = ???
    @asFinal(y) // ok
    def n(): Unit = {
      y = ???   // error
    }
  }

  def M1() = {
    var y: AnyRef = ???
    def M2() = {
      @asFinal(y) def n() = {
        y = ??? // error
        a = ???
      }
    }
  }
}