package dotty

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

}