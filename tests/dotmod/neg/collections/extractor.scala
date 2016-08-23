import dotty._

object extractor {

  class C {
    var e: AnyRef = ???

    class Extractor {
      @mutabilityOfRef(C.this) def get = e
      def set(_1: AnyRef): Unit = e = _1
    }

  }

  val cr: C @readonly = ???

  val x: AnyRef = (new cr.Extractor).get
  (new cr.Extractor).set(???)
}