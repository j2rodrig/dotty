package dotty

trait Mutability1 {

  type T = Mutability1
  type TR = Mutability1 @readonly

  val x: T
  val x1: T @readonly

  val y: T @readonly = x
  val y1: T = x1  // ERROR

  val z: T = x.x1   // ERROR
  val z1: T = x1.x  // ERROR
  val z2: T @readonly = x.x1
  val z3: T @readonly = x1.x

  type U0 >: Mutable
  type U1
  type U2 >: TR

  val a: T with U0 = x    // OK
  val a1: T with U0 = x1  // ERROR
  val a2: T with U1 = x   // ERROR
  val a3: T with U1 = x1  // ERROR
  val a4: T@readonly with T = x1   // ERROR: intersection with mutable T produces mutable type
  val a5: U2 = x1  // OK

  val b5a: Any = a5  // OK
  val b5b: Any @mutable = a5  // ERROR
  val b5c: Any with Mutable = a5  // ERROR
}
