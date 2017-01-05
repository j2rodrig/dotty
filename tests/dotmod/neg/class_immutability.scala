import dotty._

object class_immutability {

  class O {
    class M {
      type __MUTABILITY__ = readonly
      def set(_1: Any): Unit = ???
      var f: Any = ???
      f = ???
    }
    class N extends M {
      type __MUTABILITY__ = mutable
      def set(_1: Any): Unit = ???
    }
  }

  val o1: O = new O
  val o2: O @readonly = new O   // also do a @readonly enclosing instance... should this make a difference? Answer: No.
  val m1 = new o1.M
  val m2 = new o2.M
  val n1 = new o1.N
  val n2 = new o2.N
  val n3: o1.M { type __MUTABILITY__ = mutable } = new o1.M    // error: mutability mismatch
  val n3r: o1.N { type __MUTABILITY__ = readonly } = new o1.N
  val n3m: o1.M { type __MUTABILITY__ = mutable } = new o1.M { // ok: mutability is overridden in synthetic class
    override type __MUTABILITY__ = mutable
  }

  //val check1: o1.M = n1  // should mutability cause an error to be generated here?
  //val check2: o2.M = n2
  m1.f = ???   // error: non-assignable
  m1.set(???)  // error: incompatible receiver
  m2.set(???)  // error: incompatible receiver
  n1.set(???)  // ok
  n2.set(???)  // ok
  n3.set(???)  // ok
  n3r.set(???) // error
  n3m.set(???) // ok
}