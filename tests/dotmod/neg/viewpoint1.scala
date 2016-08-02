import dotty.readonly

object viewpoint1 {
  class C
  val c: C = null  // ok
  val d: C @readonly = new C  // ok
  val e: C = d  // error
}