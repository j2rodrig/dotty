import dotty._

object receiver {

  class C {
    def m() = ???
    @mutable def mm() = ???
    @polyread def mp() = ???
    @readonly def mr() = ???
  }

  val c: C @readonly = ???
  c.m()   // error
  c.mm()  // error
  c.mp()  // ok - receiver is @polyread
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


  // Check this-as-receiver
  class E {
    @mutable def mm(): Unit = {
      mm()
      mp()
      mr()
      this.mm()
      this.mp()
      this.mr()
    }
    @polyread def mp(): Unit = {
      mm()  // error
      mp()
      mr()
      this.mm()  // error
      this.mp()
      this.mr()
    }
    @readonly def mr(): Unit = {
      mm()  // error
      mp()  // ok - receiver is @polyread
      mr()
      this.mm()  // error
      this.mp()  // ok - receiver is @polyread
      this.mr()
    }
  }
}