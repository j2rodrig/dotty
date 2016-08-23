import dotty._

object collection_mut {

  class B[T, N](elem: T, v: N) {
    @mutabilityOf[T]  def g(): T = elem
    @mutabilityOf[N]  def h(): N = v
    @mutabilityOf[N]  def i(): T = elem  // error
    @polyread         def j(): T = elem  // error
    @mutable          def k(): T = elem
  }

  class C[T](elem: T) {
    val e: T = elem
    @mutabilityOf[T] def get: T = elem
    @mutabilityOf[T] def get2(): T = elem
  }

  class D
  val cd: C[D] = ???
  val cdr: C[D @readonly] = ???
  val crd: C[D] @readonly = ???

  val d0: D = cd.e       // ok
  val d1: D = cd.get     // ok
  val d2: D = cd.get2()  // ok

  val dr0: D = cdr.e       // error: type mismatch
  val dr1: D = cdr.get     // error: type mismatch
  val dr2: D = cdr.get2()  // error: type mismatch

  val rd0: D = crd.e       // error: type mismatch
  val rd1: D = crd.get     // error: incompatible receiver
  val rd2: D = crd.get2()  // error: incompatible receiver
}