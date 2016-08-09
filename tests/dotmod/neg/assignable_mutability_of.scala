import dotty._

object assignable_mutability_of {

  class C {
    var x: Any = ???
    var y: Any @readonly = ???

    def m(): Any @mutabilityOf(this) = {
      x = x  // ok: this is mutable
      x
    }

    @polyread def n(): Any @mutabilityOf(this) = {
      x = x   // error: this has polymorphic mutability
      x
    }

    @polyread def o(): Any @mutabilityOf(this) = {
      y  // error: readonly is not compatible with this.__MUTABILITY__
    }

    @readonly def p(): Any @mutabilityOf(this) = {
      x = x    // error
      x
    }
  }
}