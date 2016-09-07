import dotty._

object assignable_mutability_of {

  class C {
    var x: AnyRef = ???
    var y: AnyRef @readonly = ???

    def m(): AnyRef @mutabilityOfRef(this) = {
      x = x  // ok: this is mutable
      x  // ok
    }

    @polyread def n(): AnyRef @mutabilityOfRef(this) = {
      x = x   // error: this has polymorphic mutability
      x  // ok
    }

    @polyread def o(): AnyRef @mutabilityOfRef(this) = {
      y  // error: readonly is not compatible with this.__MUTABILITY__
    }

    @readonly def p(): AnyRef @mutabilityOfRef(this) = {
      x = x    // error
      x  // error: @mutabilityOfRef(this) is polymorphic, but the viewpoint-adapted x is @readonly
    }
  }
}