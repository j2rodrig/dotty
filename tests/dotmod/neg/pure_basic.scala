import dotty._

object pure_basic {

  // @pure and assignability
  var a: AnyRef = ???
  def m() = {
    a = ???
  }
  @pure def m2() = {
    a
    a = ???  // error

    var b: AnyRef = ???
    def n2() = {
      a = ???  // error
      b = ???
    }
  }

  // @pure and method applications
  def m11() = {
    m12()
  }
  @pure def m12(): Unit = {
    m11()  // error
    def m13(): Unit = {
      m11()  // error
      m12()
    }
    m13()
  }

  // @pure and field assignability
  class C {
    var x: AnyRef = ???
  }
  val c: C = ???
  def m3() = {
    c.x
    c.x = ???
  }
  @pure def m4() = {
    c.x
    c.x = ???  // error

    class D {
      var x: AnyRef = ???
    }
    val d: D = ???
    def n4() = {
      c.x
      c.x = ???  // error
      d.x
      d.x = ???
    }
  }

  // Effects on receiver type: method applications and superclass calls
  class C1 {
    def m(): Unit = {
      m()
      n()
    }
    @pure def n(): Unit = {
      m()  // error
      n()
    }
  }
  class D1 extends C1 {
    @pure override def m() = {
      super.m()  // error
      super.n()
    }
    @pure override def n() = {
      super.m()  // error
      super.n()
    }
  }
}