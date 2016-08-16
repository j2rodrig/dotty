import dotty._

object value_override {
  class C {
    def t: Any = ???
    @polyread def u: Any @mutabilityOf(this) = ???
  }
  class D extends C {
    override val t: Any @readonly = ???  // error
    override val u: Any = ???  // ok
  }
}