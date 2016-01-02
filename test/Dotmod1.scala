package dotty

import scala.annotation.meta._

trait D0[T0 >: AnyRef @mutable <: Any @readonly] {
}

trait D1 {
  type T1
}

trait Dotmod1 {
  def x: Int = 2
  @getter def y()() = x
  def z[T]: Int = 2
  @getter def z1[T >: Int](): T = 2

  def qwe: Dotmod1 = this
  val qwe2: Dotmod1 = this
  def rty: Dotmod1 = this

  val x5 = qwe.rty
  val x6 = qwe2.rty

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
