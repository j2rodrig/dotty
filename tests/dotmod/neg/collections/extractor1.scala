import dotty._

object extractor1 {

  class C {
    var e: AnyRef = ???

    @outerpure class Extractor {  // constructor for Extractor is @outerpure
      var f: AnyRef = ???

      @outerpure @polyread def get = e       // result is @readonly because C.this is @outerpure
      @outerpure @polyread def getLocal = f  // result is @polyread because Extractor.this is @polyread

      def set(_1: AnyRef): Unit = e = _1
      @outerpure def setLocal(_1: AnyRef): Unit = f = _1
    }

  }

  // Viewpoint adaptation of Extractor with cr produces the type cr.Extractor@outerpure@mutable because cr is @readonly.
  // Doing "new" is OK because Extractor's constructor has receiver type @outerpure@mutable.
  // (If the constructor was not declared @outerpure, then "new" could not be used with the type cr.Extractor@outerpure@mutable.)
  val cr: C @readonly = ???
  val cm: C = ???
  val crex = new cr.Extractor()  // has type cr.Extractor@outerpure
  val cmex = new cm.Extractor()  // has type cm.Extractor

  val x1: AnyRef = crex.getLocal  // ok -- result is @mutable
  val x2: AnyRef = crex.get       // error: result is @readonly
  val y2: AnyRef = cmex.get       // error, unless get has polymorphic purity (i.e., receiver has mutability this.__OUTER__MUTABILITY__)

  crex.setLocal(???)   // ok -- declared receiver type is @outerpure@mutable
  crex.set(???)        // error: incompatible receiver -- cannot call outer-impure method with @outerpure receiver
  cmex.set(???)
}
