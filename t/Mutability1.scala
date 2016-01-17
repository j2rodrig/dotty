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

  type U0 >: MutableAny
  type U1
  type U2 >: TR

  val a: T with MutableAny = x    // OK
  val a0: T with U0 = x    // OK
  val a1: T with U0 = x1  // ERROR
  val a2: T with U1 = x   // ERROR
  val a3: T with U1 = x1  // ERROR
  val a4: (T@readonly) with T = x1   // ERROR: intersection with mutable T produces mutable type
  val a5: U2 = x1  // OK

  val b5a: Any = z2  // OK
  val b5b: T @mutable = x1  // ERROR
  val b5c: (T @readonly) with (T with MutableAny) = x1  // ERROR
  val b5d: (T @readonly) with (T @mutable) = x1  // ERROR
  val b5e: TR = x1  // OK
  val b5f: TR with U0 = x1  // ERROR
  val b5g: TR with U1 = x1  // ERROR
  val b5h: TR with U2 = x1  // OK
  val b5i: T with U0 = x1  // ERROR
  val b5j: T with U1 = x1  // ERROR
  val b5k: T with U2 = x1  // ERROR

  val c0: Any = x1   // OK
  val c1: TR@mutable = x1  // ERROR
  val c2: Any with T = x1  // ERROR
  val c3: Any with TR = x1  // OK
  val c4: MutableAny with TR = x1  // ERROR

}
