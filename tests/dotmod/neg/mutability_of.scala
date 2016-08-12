import dotty._

object mutability_of {
  class C
  val c: C @readonly = ???
  val d: C @mutabilityOf(c) = c  // ok
  val e: C = d  // error
  val f: C @mutabilityOf(d) = c  // ok
  val g: C @mutabilityOf(e) = c  // error

  class D {
    val c: C = ???
    def m(): C @mutabilityOf(this) = c  // ok
    @polyread def n(): C @mutabilityOf(this) = c  // ok
    @polyread def o(): C @mutabilityOf(this) = d  // error
  }
}