import dotty._

object extractor {

  class D { var x: AnyRef = ??? }

  class C {
    var e: D @mutabilityOfRef(C.this) = ???
    var e2: D = ???

    class Extractor {
      // Case 1: Method returning an outer-object field with polymorphic mutability
      @readonly def get = {
        e = ???
        e.x = ???   // error: should not be able to mutate anything in the locality of e
        e
      }

      // Case 2: Using a type view to locally give an outer-object field a polymorphic mutability
      @asType[D @mutabilityOfRef(C.this)](e2) @readonly def get2 = {
        e2 = ???
        e2.x = ???   // error: should not be able to mutate anything in the locality of e2
        e2
      }

      // Case 3: Using asFinal to restrict assignability
      @asFinal(e2) @asType[D @mutabilityOfRef(C.this)](e2) @readonly def get3 = {
        e2 = ???     // error
        e2.x = ???   // error: should not be able to mutate anything in the locality of e2
        e2
      }

      // Case 4: Caching a polymorphic-mutability outer-field.
      var cached_e = e
      // Allowed: reading of cached variable. (But result is mutable only if both C.this and Extractor.this are mutable.)
      @polyread def get_cached = {
        cached_e
      }
      // Allowed: external assignment to cached variable.
      def set_cached(_new_e: D @mutabilityOfRef(C.this)) = {
        e = _new_e  // and also assignment to original variable, if not effectively final
        cached_e = _new_e
      }

      // Control case: Extraction of a local field
      var f: D @mutabilityOfRef(Extractor.this) = ???
      @polyread def getLocal = {
        f.x = ???   // error
        f
      }
      @readonly def getLocal2 = {
        f.x = ???   // error
        f
      }
      def getLocal3 = {
        f.x = ???   // ok
        f
      }
    }
  }

  // Case 1A: mutable field extracted from readonly reference
  val c: C = ???
  val ex: c.Extractor @readonly = ???
  val x: AnyRef = ex.get

  // Case 1B: readonly field extracted from mutable reference
  val cr: C @readonly = ???
  val exr: cr.Extractor = ???
  val xr: AnyRef = exr.get  // error
  val xrr: AnyRef @readonly = exr.get
  val xrr2: AnyRef @mutabilityOfRef(cr) = exr.get

  // Case 4
  val dr: D @readonly = ???
  val xxx: D @mutabilityOfRef(cr) = cr.e  // prefix of @readonly adapted with @mutabilityOfRef(C.this) gives @mutabilityOfRef(cr)
  val xxx2: D @mutabilityOfRef(c) = c.e   //
  val xxx3d: D = exr.get_cached  // error
  val xxx3e: D = exr.cached_e    // error
  val xxx4d: D = ex.get_cached  // error
  val xxx4e: D = ex.cached_e    // error
  exr.cached_e = cr.e
  exr.set_cached(cr.e)
  exr.cached_e = dr   // ok: cr.e is @readonly, so exr can cache a @readonly reference
  exr.set_cached(dr)  // ok

  ex.cached_e = dr   // error: cache is readonly
  ex.set_cached(dr)  // error: cache is readonly

  val exm: c.Extractor @mutable = ???
  val xxx7d: D = exm.get_cached
  val xxx7e: D = exm.cached_e
  exm.cached_e = c.e
  exm.set_cached(c.e)
  exm.cached_e = dr    // error
  exm.set_cached(dr)   // error


  // Control-case examples
  val fxmLocal: AnyRef = exm.getLocal
  val fxrLocal: AnyRef = ex.getLocal   // error
  val fxmLocal2: AnyRef = exm.getLocal2  // error
  val fxrLocal2: AnyRef = ex.getLocal2   // error
  val fxmLocal3: AnyRef = exm.getLocal3
  val fxrLocal3: AnyRef = ex.getLocal3   // error: incompatible receiver

}