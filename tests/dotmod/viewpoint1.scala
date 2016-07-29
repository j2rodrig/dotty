import dotty.readonly

object viewpoint1 {
  class C
  val c: C = null
  val d: C @readonly = new C
  val e: C = d
}