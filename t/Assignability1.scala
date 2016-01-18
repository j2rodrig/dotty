package dotty

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

}