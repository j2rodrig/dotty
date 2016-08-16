import dotty._

object result_override {
  class C {
    def m(): Any = ???
    @polyread def n(): Any @mutabilityOf(this) = ???
  }
  class D extends C {
    override def m(): Any @readonly = ???  // error
    @polyread override def n(): Any @mutabilityOf(this) = ???  // ok
  }
}