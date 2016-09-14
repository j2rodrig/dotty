import dotty._

object as_type_override {

  var x: AnyRef = ???

  class C {
    @asType[AnyRef @readonly](x) def m(): Unit = ???
  }
  class D extends C {
    override def m(): Unit = ???  // error
  }
  class E extends C {
    @asType[AnyRef @mutabilityOfRef(this)](x) override def m(): Unit = ???  // error
  }
  class F extends C {
    @asType[AnyRef @readonly](x) override def m(): Unit = ???
  }

  class C1 {
    def m(): Unit = ???
  }
  class D1 extends C1 {
    @asType[AnyRef @mutabilityOfRef(this)](x) override def m(): Unit = ???
  }
  class E1 extends C1 {
    @asType[AnyRef @readonly](x) override def m(): Unit = ???
  }

  class O {
    var y: AnyRef = ???
    class C {
      @asType[AnyRef @readonly](y) def m(): Unit = ???
    }
  }
  val o: O = ???
  class P {

    class D extends o.C {
      override def m(): Unit = ???  // error
    }

    class E extends o.C {
      @asType[AnyRef @readonly](o.y) override def m(): Unit = ???
    }
  }
}

