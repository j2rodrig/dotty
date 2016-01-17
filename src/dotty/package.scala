/**
 * There are two lattices: a lattice of mutable types, and a lattice of readonly types.
 * Each readonly type is a supertype of the mutable version of that type.
 *
 * A type can be made mutable by intersecting with MutableAny.
 * A type can be made readonly by unioning with ReadonlyNothing.
 *
 * ReadonlyAny is the top type, and MutableNothing is hte bottom type.
 */


package object dotty {
  trait MutableAny { }       // to be understood as a supertype of all Mutable types
  trait ReadonlyNothing { }  // to be understood as a supertype of Nothing, and a subtype of all Readonly types

  trait Y { }

  trait readonly extends scala.annotation.StaticAnnotation
  trait rothis extends scala.annotation.StaticAnnotation
  trait mutable extends scala.annotation.StaticAnnotation


  // What about union with special trait RO, which is a supertype of Nothing?

  // e.g., add a ReadonlyNothing and a MutableAny.
}
