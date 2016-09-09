import dotty._

object pure_refchecks {

  class C {
    def m() = ???
    @pure def n() = ???
  }
  class D extends C {
    override def m() = ???
    override def n() = ???  // error
  }
  class E extends C {
    @pure override def m() = {
      super.m()   // error
    }
    @pure override def n() = {
      super.n()
    }
  }


  @pure class C1 {
    def m() = ???
  }
  @pure class D1 extends C1 {
    override def m() = {
      super.m()
    }
  }
  class E1 extends C1 {
    override def m() = ???  // error
  }
  class F1 extends C1 {
    @pure override def m() = ???
  }

}