import dotty._

object as_type {
  var a: AnyRef = ???

  @asType[Any](a) def m() = {
    val x: AnyRef = a   // error
    a = x  // ok
    val y: Any = a  // ok
    a = y  // error
  }

  class C

  @asType[C](a)   // error
  def n() = {
    m()
  }

  @asType[Any](this)  // error
  def n1() = {
  }

  @asType[Any](a) def o() = {
    @asType[AnyRef](a)  // error
    def o1() = {
      m()
    }
  }

  def o3() = {
    var x: C = ???
    @asType[C @mutable](x) def p1(): Unit = {
      p1()
      p2()
      p3()
      p4()
    }
    @asType[C @readonly](x) def p2(): Unit = {
      p1()  // error
      p2()
      p3()
      p4()  // error
    }
    @pure def p3(): Unit = {
      p1()  // error
      p2()  // error
      p3()
      p4()  // error
    }
    def p4(): Unit = {
      p1()
      p2()
      p3()
      p4()
    }
    p1()
    p2()
    p3()
    p4()
  }

}