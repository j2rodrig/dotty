package dotty

import scala.annotation.meta._

trait ViewpointAdaptation2 {
  type T = ViewpointAdaptation2
  type TR = ViewpointAdaptation2@readonly

  var x: T
  var x1: TR

  @polyread def m[X]: T @polyread

  val z0: T = x.m[MutableAny, Any]     // OK
  val z1: T = x1.m[Any,Any]   // ERROR

  val z2: T = x.m[Any]     // OK
  val z3: T = x1.m[Any]   // ERROR

  val z4: T = x.m[Any,Any]     // OK
  val z5: T = x1.m[MutableAny,Any]   // ERROR: receiver does not match environment mutability

  @polyread def n() = {
    val z10: T@polyread = x1.m[Any,Any] // ERROR
  }
}