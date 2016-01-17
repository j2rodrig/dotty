package dotty

trait arb0
trait arb1 {  }
trait arb2 { val b: Int }
trait arb3 { val b: Int = 10 }

trait T0 {
  //val x: Any
  //val x1: T0
  val x2: T0 @readonly
  val y2: T0 = x2             // ERROR
  val z2: T0 @mutable = y2    // OK
  val a20: T0 @readonly with MutableAny { val b: Int } = x2     // ERROR
  val a21: T0 with arb0 = x2     // ERROR
  val a21b: T0 with arb1 = x2     // ERROR
  val a21c: T0 with arb2 = x2     // ERROR
  val a21d: T0 with arb3 = x2     // ERROR
  val b2: MutableAny = y2     // OK
}