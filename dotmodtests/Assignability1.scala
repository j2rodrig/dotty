package dotty

import scala.annotation.meta._

trait Assignability1 {

  type T = Assignability1
  type TR = Assignability1@readonly

  var x: T
  var x1: TR

  x = x
  x = x1   // ERROR: mutability
  x1 = x
  x1 = x1

  x.x = x
  x.x1 = x
  x1.x = x  // ERROR: assignability
  x1.x1 = x  // ERROR: assignability

  x.x = x1  // ERROR: mutability
  x.x1 = x1
  x1.x = x1  // ERROR: mutability and assignability
  x1.x1 = x1  // ERROR: assignability

  def e1 = {  // z1:E1 =>

    var y: T = this
    var y1: TR = this

    y.x = y  // OK

    @polyread def e2 /* [V2 >: MutableAny <: Any](z1: (E1|ReadonlyNothing)&V2) */ = {  // z2:E2 =>

      val z: T = y   // ERROR
      val z1: T @polyread = y  // OK

      y = y   // ERROR: assignability
      y.x = y   // ERROR: assignability
      y.x = y1   // ERROR: assignability and mutability

      @polyread def e3 = {
        y = y   // ERROR: assignability
        y.x = y   // ERROR: assignability
        y.x = y1   // ERROR: assignability and mutability

        y1
      }

      y1
    }
    y1
  }

}