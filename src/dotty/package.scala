/**
 * There are two lattices: a lattice of mutable types, and a lattice of readonly types.
 * Each readonly type is a supertype of the mutable version of that type.
 *
 * A type can be made mutable by intersecting with MutableAny.
 * A type can be made readonly by unioning with ReadonlyNothing.
 *
 * ReadonlyAny is the top type, and MutableNothing is the bottom type.
 */


package object dotty {
  trait MutableAny       // to be understood as a supertype of all Mutable types
  trait ReadonlyNothing  // to be understood as a supertype of Nothing, and a subtype of all Readonly types

  final class readonly extends scala.annotation.StaticAnnotation
  final class polyread extends scala.annotation.StaticAnnotation
  final class rothis extends scala.annotation.StaticAnnotation
  final class mutable extends scala.annotation.StaticAnnotation
}
