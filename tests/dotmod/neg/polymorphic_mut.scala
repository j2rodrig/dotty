import dotty.{mutabilityOf, readonly}

object polymorphic_mut {

  class C {
    @readonly def ident(): C @mutabilityOf(this) = ???
  }
  val cm: C = ???
  val cr: C @readonly = ???

  val j: C = cr.ident()  // error
  val k: C = cm.ident()  // ok
}