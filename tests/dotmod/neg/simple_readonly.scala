import dotty.readonly

object simple_readonly {
  class C
  val c: C = null  // ok
  val d: C @readonly = new C  // ok
  val e: C = d  // error
}
