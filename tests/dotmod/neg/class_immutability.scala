import dotty._

object class_immutability {

  class O {
    class M extends AnyRef {
      type __MUTABILITY__ = readonly
      def set(_1: Any): Unit = ???
    }
    class N extends M {
      type __MUTABILITY__ = mutable  // reset mutability here
    }
  }

  val o1: O = new O
  val o2: O @readonly = new O   // also do a @readonly enclosing instance... should this make a difference?
  val m1 = new o1.M
  val m2 = new o2.M
  val n1 = new o1.N
  val n2 = new o2.N

  val check1: o1.M = n1  // should mutability cause an error to be generated here?
  val check2: o2.M = n2

  m1.set(???)  // error: incompatible receiver
  m2.set(???)  // error: incompatible receiver
  n1.set(???)  // ok
  n2.set(???)  // ok
}