
package object dotty {
  type Readonly = Any
  trait RoThis extends Readonly
  trait Mutable extends RoThis

  trait readonly extends scala.annotation.StaticAnnotation
  trait rothis extends scala.annotation.StaticAnnotation
  trait mutable extends scala.annotation.StaticAnnotation
}
