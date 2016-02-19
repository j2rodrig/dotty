package dotty

trait Subtyping1 {
  val x0: (Subtyping1 @readonly) with ReadonlyNothing
  var x1: MutableAny = x0   // ERROR
}