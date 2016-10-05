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

  class F[T] {
    @mutabilityOf[T] def n(): Unit = ???
  }
  class G[T] extends F[T] {
    @mutabilityOf[T] override def n(): Unit = ???  // ok
  }
  class H[T] extends F[T] {
    @polyread override def n(): Unit = ???  // error
  }
  class I[T, U] extends F[T] {
    @mutabilityOf[U] override def n(): Unit = ???  // error
  }
}