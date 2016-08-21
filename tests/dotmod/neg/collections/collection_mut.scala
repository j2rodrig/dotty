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
  val crdr: C[D @readonly] @readonly = ???

  val d1: D = cd.get     // ok
  val d2: D = cd.get2()  // ok

  val dr0: D = cdr.e   // err
  val dr1: D = cdr.get  // err
  val dr2: D = cdr.get2()  // err

  /*
  val rd0: D = crd.e
  val rd1: D = crd.get  // err
  val rd2: D = crd.get2()  // err
  */
}