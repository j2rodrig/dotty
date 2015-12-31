import scala.annotation.polyread

trait Dotmod1 {
  def x: Int = 2
  @polyread def y()() = x

  val n = x   // tree type: Select(this,x)
  val n3 = y()()
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
