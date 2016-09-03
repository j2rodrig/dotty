import dotty._

object as_final_override {

  var x: AnyRef = ???

  class C {
    @asFinal(x) def m(): Unit = {
      m()
    }
  }
  class D extends C {
    override def m(): Unit = {  // error
      super.m()
    }
  }
  class E extends C {
    @asFinal(x) override def m(): Unit = {
      super.m()
    }
  }

  class C1 {
    def m(): Unit = {
      x = ???
    }
  }
  class D1 extends C1 {
    @asFinal(x) override def m(): Unit = {
      super.m()  // error
    }
  }

  class O {
    var y: Any = ???
    class C {
      @asFinal(y) def m(): Unit = {}
    }
  }
  val o: O = ???
  class P {

    class D extends o.C {
      override def m(): Unit = {} // error
    }

    class E extends o.C {
      @asFinal(o.y) override def m(): Unit = {}
    }
  }

}