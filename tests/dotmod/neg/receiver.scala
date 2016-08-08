import dotty.{readonly, mutabilityOf}

object receiver {

  class C {
    def m() = ???
    @mutabilityOf(d) def md() = ???
    @mutabilityOf(c) def mc() = ???
    @readonly def mr() = ???
  }

  val c: C @readonly = ???
  c.m()   // error
  c.md()  // error
  c.mc()  // ok
  c.mr()  // ok

  // Overloaded methods
  trait A {
    def m() = ???
    //@readonly def m() = ???  // would need to change overload resolution to allow this
  }
  class D extends A {
    override def m() = ???
  }

  val d: D = ???
  d.m()   // ok
}