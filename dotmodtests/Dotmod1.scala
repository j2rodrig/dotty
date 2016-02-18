package dotty

import scala.annotation.meta._

trait D0[T0 >: Mutable <: Readonly] {
}

trait D05[T0 >: Mutable] {}

trait D1 {
  type T1
}

trait D2 {
  var v: AnyRef
  @rothis trait D2B {
    @mutable def m() = {
      v = this  //should be an error: v should be @readonly from this viewpoint
    }
  }
  trait D2C {
    var w: AnyRef @rothis
    @rothis def m(_this: D2C @rothis) = {
      var outer: D2C = this
      var x: AnyRef @rothis = this
      x // what's the type of x here? should be @rothis.
      w // what's the type of w here?  this.w: @rothis.@rothis
    }
  }
}

trait Dotmod1 {
  def x: Int = 2
  @getter def y()() = x
  def z[T]: Int = 2
  @getter def z1[T >: Int](): T = 2

  def qwe: Dotmod1 = this
  val qwe2: Dotmod1 @readonly = this
  def rty: Dotmod1 = this
  val rty2: Dotmod1 = this

  //val x6: Dotmod1 @mutable = qwe2
  val x7: Dotmod1 @mutable = qwe2.rty2
  val x8: Dotmod1 @mutable = x7

  val n = x   // tree type: Select(this,x)
  val n3 = y()()
  val n4 = z[AnyVal]
  val n5 = z1[AnyVal]
}
trait Dotmod2 extends Dotmod1 {
  override val x = 3
  def getX() = x
  def getX2() = x
  def getX3 = x
  //val n10 = x
  val n11 = getX     // tree type: Apply(Select(this,getX))
  val n12 = getX2()  // tree type: Apply(Select(this,getX2)
  val n13 = getX3    // tree type: Select(this,getX3)
}
trait Dotmod3 extends Dotmod1 {
  override def x: Int = 3
//  val n20 = x
}
trait Dotmod4 extends Dotmod1 {
  override def x() = 3
//  val n30 = x
//  val n31 = x()
}

object Test extends App {
  println("App!")
}