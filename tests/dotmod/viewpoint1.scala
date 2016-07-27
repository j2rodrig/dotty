import dotty.readonly

object viewpoint1 {
  class C {
    def m(): C = this
  }
  val c: C @readonly = new C @readonly
  val d: C = c
  c.m()
}