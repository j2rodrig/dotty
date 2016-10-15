import dotty._

object pure_3 {

  // Using F1 as an experimental stand-in for Function1.
  abstract class F1[-A,+B] {
    // __NON_MUTABLE_AT__, if assigned, refers to a lexically enclosing class C (specifically, a TypeAlias of a TypeRef to the class C).
    // The mutability of C.this is C.this.__MUTABILITY__, and every E.this for class E enclosing C is E.this.__MUTABILITY__.
    // All local variables of methods enclosing C are seen as non-assignable and readonly.
    //
    // @nonMutable[C] on a method means that the method expects a receiver with a __NON_MUTABLE_AT__ that's no more restrictive than C.
    // (Because the method may mutate local variables and classes lexically enclosed by C.)
    // If C is the immediate owner of the method, then we say that the method is "pure."
    type __NON_MUTABLE_AT__
    type __MUTABILITY__ >: mutable <: readonly
    @nonMutableAt[__NON_MUTABLE_AT__] @mutablility[__MUTABILITY__] def apply(a: A): B
  }

  val ls: List[Int] = ???
  ls.foreach()

  class E {

    var x: Int = 0  // a field

    def n(): F1[Int,Int] = {

      var y: Int = 0  // a local variable

      // Examples of function classes
      class Fn extends F1[Int,Int] {
        type __NON_MUTABLE_AT__ = E
        @nonMutableAt[__NON_MUTABLE_AT__] @mutablility[__MUTABILITY__] def apply(a: Int): Int = {
          y += a  // ok
          x += a  // error
          y
        }
      }

      val fn = new Fn
      fn.apply(3)
      fn
    }
  }
}