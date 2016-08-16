import dotty._

object exprs {
  class D {
    class C

    def e: C = ???
    def f: C = e
    def g: C @mutabilityOf(this) = e
    @polyread def h: C = e  // error: incompatible receiver mutability
    @polyread def i: C @mutabilityOf(this) = e  // error: incompatible receiver mutability
    def j: C = g
    def k: C = h
    def l: C = i

    val ev: C = ???
    val fv: C = ev
    val gv: C @mutabilityOf(this) = ev
    @polyread val hv: C = ev
    @polyread val iv: C @mutabilityOf(this) = ev
    val jv: C = gv
    val kv: C = hv
    val lv: C = iv

    def em(): C = ???
    def fm(): C = em
    def gm(): C @mutabilityOf(this) = em
    def gm2(): C @mutabilityOf(this) = em()
    @polyread def hm(): C = em()  // error: incompatible receiver mutability
    @polyread def hm2(): C = em   // error: incompatible receiver mutability
    @polyread def im(): C @mutabilityOf(this) = em()  // error: incompatible receiver mutability
    @polyread def im2(): C @mutabilityOf(this) = em   // error: incompatible receiver mutability
    def jm(): C = gm
    def km(): C = hm
    def lm(): C = im

  }
}
