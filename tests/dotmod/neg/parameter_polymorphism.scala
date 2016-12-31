import dotty._

object parameter_polymorphism {

  /// Two-parameter mutability inference

  def m[T >: mutable <: readonly](x: C @mutability[T], y: C @mutability[T])
  : C @mutability[T] =
  {
    if (cond()) x
    else y
  }

  // Helper: we need to return a boolean. Ideally random, but compiler shouldn't optimize out a method call during typing.
  def cond(): Boolean = ???

  class C { }
  val c: C = ???
  val cr: C @readonly = ???

  val x1 = m(c, c)
  val x2 = m(c, cr)
  val x3 = m(cr, c)

  var xm: Any @mutable = ???
  xm = x1   // ok
  xm = x2   // error
  xm = x3   // error



  /// Two-parameter-plus-receiver mutability inference

  class D {
    val f: C = ???

    type __MUTABILITY__ >: mutable <: readonly

    @polyread def m[T >: this.__MUTABILITY__ <: readonly](x: C @mutability[T], y: C @mutability[T])
    : C @mutability[T] = {
      if (cond()) f
      else if (cond()) x
      else y
    }
  }
  val d: D = ???
  val dr: D @readonly = ???

  val x4 = d.m(c, c)
  val x5 = d.m(c, cr)
  val x6 = d.m(cr, c)
  val x7 = dr.m(c, c)
  val x8 = dr.m(c, cr)
  val x9 = dr.m(cr, c)

  xm = x4    // ok
  xm = x5    // error
  xm = x6    // error
  xm = x7    // error
  xm = x8    // error
  xm = x9    // error
}