import dotty._

object value_override {
  class C {
    def t: AnyRef = ???
    @polyread def u: AnyRef @mutabilityOfRef(this) = ???
  }
  class D extends C {
    override val t: AnyRef @readonly = ???  // error
    override val u: AnyRef = ???  // ok
  }
}