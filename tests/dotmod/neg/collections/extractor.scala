import dotty._

object extractor {

  class C[T](elem: T) {
    class Extractor {
      val e: T = elem
      @mutabilityOf[T] def get: T = elem
      @mutabilityOf[T] def get2(): T = elem
    }
  }

  class D
  val cd: C[D] = ???
  val cdr: C[D @readonly] = ???
  val crd: C[D] @readonly = ???
  val crdr: C[D @readonly] @readonly = ???

  val xrd: D = (new crd.Extractor).e  // need to set type of (new crd.Extractor) to not be able to call mutate

  val xrdr: D = (new crdr.Extractor).e  // error

}