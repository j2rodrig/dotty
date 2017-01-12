import dotty._

object mutable_in {

  // Field tests
  class C {
    var f = ???

    class D {
      var g = ???

      @mutableIn[D] def m1(): Unit = {
        f = ???  // error: f is not assignable here
        g = ???  // ok
      }

      @mutableIn[C] def m2(): Unit = {
        f = ???  // ok
        g = ???  // ok
      }

      @mutableIn[C] @readonly def m3(): Unit = {
        f = ???  // ok
        g = ???  // error: g is not assignable here
      }
    }
  }

  // Call tests
  class E {
    @pure def m1(): Unit = {
      m1()
      m2()  // error
      m3()
      m4()  // error
    }
    @polyread def m2(): Unit = {
      m1()
      m2()
      m3()
      m4()  // error
    }
    @mutableIn[E] @polyread def m3(): Unit = {  // same as @pure
      m1()
      m2()  // error
      m3()
      m4()  // error
    }
    @mutableIn[E] def m4(): Unit = {
      m1()
      m2()  // error
      m3()
      m4()
    }
  }
}