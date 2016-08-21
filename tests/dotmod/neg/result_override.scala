import dotty._

object result_override {
  class C {
    def m(): AnyRef = ???
    @polyread def n(): AnyRef @mutabilityOfRef(this) = ???
  }
  class D extends C {
    override def m(): AnyRef @readonly = ???  // error
    @polyread override def n(): AnyRef @mutabilityOfRef(this) = ???  // ok
  }
}