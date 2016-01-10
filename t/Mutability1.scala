package dotty

trait Mutability1 {

  type T = Mutability1

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
  type U2 >: Any @readonly

  val a: T with U0 = x    // OK
  val a1: T with U0 = x1  // ERROR
  val a2: T with U1 = x   // ERROR
  val a3: T with U1 = x1  // ERROR
  val a4: T with U2 = x   // OK
  val a5: T with U2 = x1  // OK

  val b4: T = a4
  val b5: T = a5
}
