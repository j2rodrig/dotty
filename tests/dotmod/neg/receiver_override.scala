import dotty._

object receiver_override {
  class C {
    @readonly def m(): Unit = ???
    @polyread def n(): Unit = ???
    def o(): Unit = ???
  }
  class D extends C {
    override def m(): Unit = ???  // error
    override def n(): Unit = ???  // error
    override def o(): Unit = ???  // ok
  }
  class E extends C {
    @polyread override def m(): Unit = ???  // error
    @polyread override def n(): Unit = ???  // ok
    @polyread override def o(): Unit = ???  // ok
  }
}