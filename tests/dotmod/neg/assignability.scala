import dotty.{mutable, readonly, mutabilityOf}

object assignability {
  class C {
    var x: Any = ???
    var y: Any @readonly = ???
    var z: Any @mutable = ???
  }
  val cr: C @readonly = ???
  cr.x = ???  // error
  cr.y = ???  // error
  cr.z = ???  // error

  val cm: C = ???
  cm.x = ???  // ok
  cm.y = ???  // ok
  cm.z = ???  // ok
}