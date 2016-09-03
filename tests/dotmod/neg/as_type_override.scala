import dotty._

object as_type_override {

  var x: AnyRef = ???

  class C {
    @asType[Any](x) def m(): Unit = {
      m()
    }
  }
  class D extends C {
    override def m(): Unit = {  // error
      super.m()
    }
  }
  class E extends C {
    @asType[Any](x) override def m(): Unit = {
      super.m()
    }
  }

  class C1 {
    def m(): Unit = {
      x = ???
    }
  }
  class D1 extends C1 {
    @asType[Any](x) override def m(): Unit = {
      super.m()  // error
    }
  }

  class O {
    var y: AnyRef = ???
    class C {
      @asType[Any](y) def m(): Unit = {}
    }
  }
  val o: O = ???
  class P {

    class D extends o.C {
      override def m(): Unit = {} // error
    }

    class E extends o.C {
      @asType[Any](o.y) override def m(): Unit = {}
    }
  }
}

