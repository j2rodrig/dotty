import dotty._

// See <BACK-TO-RECEIVERS--TOWARD-PURITY> in notes.
object pure_rcv_mut {

  // Using F1 as an experimental stand-in for Function1.
  abstract class F1[-A,+B] {
    type __OUTER__
    @mutabilityOf[__OUTER__] def apply(x:A):B
  }

  class E {
    def n() = {
      var v = 0

      // Pure
      class E0 extends F1[Int,Int] {
        override type __OUTER__ = AnyRef@readonly
        @mutabilityOf[__OUTER__] def apply(diff: Int): Int = { v = v + diff; v }
      }
      val e0 = new E0

      // ImPure
      class E1 extends F1[Int,Int] {
        override type __OUTER__ = AnyRef@readonly
        var w = 0
        @mutabilityOf[__OUTER__] def apply(diff: Int): Int = { w = w + diff; w }  // error: w is not assignable here
      }
      val e1 = new E1

      e0
    }
  }
  val e: E = ???
  val fn: F1[Int, Int] = e.n()
  fn.apply(3)
}