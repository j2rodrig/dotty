import dotty._

object mutable_in {

  // Field tests
  class C {
    var f = ???

    class D {
      var g = ???

      @mutableIn[D] def m1() = {
        f = ???  // error: f is not assignable here
        g = ???  // ok
      }

      @mutableIn[C] def m2() = {
        f = ???  // ok
        g = ???  // ok
      }

      @mutableIn[C] @readonly def m3() = {
        f = ???  // ok
        g = ???  // error: g is not assignable here
      }
    }
  }

  // Getter tests?

}