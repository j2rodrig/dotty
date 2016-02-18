package dotty

import scala.annotation.meta._

trait ViewpointAdaptation1 {
  type T = ViewpointAdaptation1
  type TR = ViewpointAdaptation1@readonly

  var x: T
  var x1: TR

  x.x.x = x
  x.x.x = x1    // ERROR:  mutability
  x.x.x1 = x
  x.x.x1 = x1
  x.x1.x = x    // ERROR: assignability
  x.x1.x = x1   // ERROR: assignability and mutability
  x.x1.x1 = x   // ERROR: assignability
  x.x1.x1 = x1  // ERROR: assignability
  x1.x.x = x    // ERROR: assignability
  x1.x.x = x1   // ERROR: assignability and mutability
  x1.x.x1 = x   // ERROR: assignability
  x1.x.x1 = x1  // ERROR: assignability
  x1.x1.x = x   // ERROR: assignability
  x1.x1.x = x1  // ERROR: assignability and mutability
  x1.x1.x1 = x  // ERROR: assignability
  x1.x1.x1 = x1 // ERROR: assignability

  @polyread def m: T = {
    var y: T = ???
    var y1: T @polyread = ???
    var y2: T @readonly = ???

    y = y
    y = y1   // ERROR
    y = y2   // ERROR
    y1 = y
    y1 = y1
    y1 = y2  // ERROR
    y2 = y
    y2 = y1
    y2 = y2

    x
  }
  m                // OK: environment mutability is inferred
  m[MutableAny]    // OK: environment mutability is explicit
  m[T]             // ERROR: T does not conform to mutability bounds

  @polyread def n[P]: T
  n
  n[T]             // OK: environment mutability is inferred
  n[MutableAny,T]  // OK: environment mutability is explicit
  n[T,T]           // ERROR: T does not conform to mutability bounds

  def o[P]: T
  o
  o[T]
  o[MutableAny,T]  // ERROR: not an environment-polymorphic method

  def p: T
  p
  p[MutableAny]    // ERROR: not an environment-polymorphic method
}