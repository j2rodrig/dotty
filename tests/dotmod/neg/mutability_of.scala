import dotty._

object mutability_of {
  class C
  val c: C @readonly = ???
  val d: C @mutabilityOfRef(c) = c  // ok
  val e: C = d  // error
  val f: C @mutabilityOfRef(d) = c  // ok
  val g: C @mutabilityOfRef(e) = c  // error

  class D {
    val c: C = ???
    def m(): C @mutabilityOfRef(this) = c  // ok
    @polyread def n(): C @mutabilityOfRef(this) = c  // ok
    @polyread def o(): C @mutabilityOfRef(this) = d  // error
  }
}