import dotty._


// See <DISCUSSION:POLYMORPH-PURITY> in notes.
object pure_rcv_mut_2 {

  // Using F1 as an experimental stand-in for Function1.
  abstract class F1[-A,+B] {
    type M >: mutable <: readonly
    @mutability[M] def apply(a: A): B
  }

  // ImPure and Pure extensions of F1
  class MutableF1[-A,+B] extends F1[A,B] {
    type M = mutable
    private var _local_ = 0
    @mutability[M] def apply(a: A): B = { _local_ = 1 ; ??? }  // ok
  }
  class ReadonlyF1[-A,+B] extends F1[A,B] {
    type M = readonly
    private var _local_ = 0
    @mutability[M] def apply(a: A): B = { /*_local_ = 1 ;*/ ??? }  // disabled-error
  }

  // Calling various versions of F1
  val mutableF1: F1[Int,Int] { type M = mutable ; type __MUTABILITY__ = mutable } = new MutableF1[Int,Int]
  mutableF1.apply(3)

  val readonlyF1: F1[Int,Int] { type M = readonly ; type __MUTABILITY__ = readonly } = new ReadonlyF1[Int,Int]
  readonlyF1.apply(3)

  app[mutable](mutableF1)
  app[readonly](readonlyF1)
  app(mutableF1)  // mutability parameter is inferred!!!
  app(readonlyF1) // mutability parameter is inferred!!!
  def app[N >: mutable <: readonly](fn: F1[Int,Int] { type M = N ; type __MUTABILITY__ = N }) = {
    fn.apply(3)
  }


  // Checking simple inference
  val x: AnyRef = ???
  val y: AnyRef @mutable = ???
  val z: AnyRef @readonly = ???
  app2(x)
  app2(y)
  app2(z)
  val x1: AnyRef = app2(x)  // is M defaulted to the lower bound (mutable) if no mutability specified?
  // app2[mutable](z)  // disabled-error: type mismatch
  // app2[Any](z)  // disabled-error: non-conformance to upper bound readonly
  def app2[M >: mutable <: readonly](p: AnyRef @mutability[M]) = p

  // Random stuff
  class Temp[-A,+B] extends MutableF1[A,B] {
    override type M = readonly  // error in RefChecks phase
    // Explanation: Can't re-assign an incompatible type to M here.
    // Otherwise, the call to temp.apply below would be able to perform mutations despite temp being readonly.
  }
  val temp: Temp[Int,Int] @readonly = new Temp[Int,Int]
  temp.apply(3)
}