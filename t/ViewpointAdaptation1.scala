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

  @getter def m: T
  m                // OK: environment mutability is inferred
  m[MutableAny]    // OK: environment mutability is explicit
  m[T]             // ERROR: T does not conform to mutability bounds

  @getter def n[P]: T
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