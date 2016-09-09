import dotty._

object exprs {
  class D {
    class C

    def e: C = ???
    def f: C = e
    def g: C @mutabilityOfRef(this) = e
    @polyread def h: C = e  // error: incompatible receiver mutability
    @polyread def i: C @mutabilityOfRef(this) = e  // error: incompatible receiver mutability
    def j: C = g
    def k: C = h
    def l: C = i

    val ev: C = ???
    val fv: C = ev
    val gv: C @mutabilityOfRef(this) = ev
    @polyread val hv: C = ev  // @polyread has no effect on non-methods
    @polyread val iv: C @mutabilityOfRef(this) = ev  // @polyread has no effect on non-methods
    val jv: C = gv
    val kv: C = hv
    val lv: C = iv

    def em(): C = ???
    def fm(): C = em
    def gm(): C @mutabilityOfRef(this) = em
    def gm2(): C @mutabilityOfRef(this) = em()
    @polyread def hm(): C = em()  // error: incompatible receiver mutability
    @polyread def hm2(): C = em   // error: incompatible receiver mutability
    @polyread def im(): C @mutabilityOfRef(this) = em()  // error: incompatible receiver mutability
    @polyread def im2(): C @mutabilityOfRef(this) = em   // error: incompatible receiver mutability
    def jm(): C = gm
    def km(): C = hm
    def lm(): C = im

  }
}
