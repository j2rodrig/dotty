import dotty.readonly
import dotty.mutabilityOf

object mutability_of {
  class C
  val c: C @readonly = ???
  val d: C @mutabilityOf(c) = c  // ok
  val e: C = d  // error
}