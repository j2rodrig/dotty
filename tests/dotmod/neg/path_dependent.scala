import dotty._

object path_dependent {

  class C {
    class D {
    }
  }

  // Path-dependent types
  val c1: C @readonly = ???
  val d1: c1.D = ???
  val c2: C = ???
  val d2: c2.D = d1   // error: type mismatch (due to not c1 <: c2)

  // Path-independent types
  val xd1: (C @readonly)#D = ???
  val xd2: C#D = xd1  // error: type mismatch (due to not C @readonly <: C)
}