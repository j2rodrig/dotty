package dotty

trait T0 {
  //val x: Any
  //val x1: T0
  val x2: T0 @readonly
  val y2: T0 = x2             // ERROR
  val z2: T0 @mutable = y2    // OK
  val a20: (T0 @readonly) with MutableAny = x2     // ERROR
  val a21: T0 with MutableAny = x2     // ERROR
  val a22: T0 with MutableAny = y2     // OK
  val b2: MutableAny = x2     // ERROR
  val b2b: MutableAny = y2     // OK
}