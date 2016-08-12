import dotty._

object polymorphic_mut {

  class C {
    @polyread def ident(): C @mutabilityOf(this) = this
  }
  val cm: C = ???
  val cr: C @readonly = ???

  val j: C = cr.ident()  // error
  val k: C = cm.ident()  // ok
}