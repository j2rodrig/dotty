import dotty.{pure, mutable, polyread, readonly}

object purityrefchecks {

  trait A {
    @pure def m: Any
  }

  trait B extends A {
    def m: Any
  }
}
