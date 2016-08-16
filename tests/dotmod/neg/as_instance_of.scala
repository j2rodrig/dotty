import dotty._

object as_instance_of {
  class C
  class D extends C
  val dm:C = ???
  val dr:C @readonly = ???
  val e:D = dm.asInstanceOf[D]  // ok
  val f:D = dr.asInstanceOf[D]  // error
  val g:D = dm.asInstanceOf[D @readonly]  // error

  val i:D = dr.asInstanceOf[D @mutable]  // error
  val j:D @readonly = dr.asInstanceOf[D @mutable]  // ok

  dr match {
    case d2: D =>
      val d3: D = d2
  }
}