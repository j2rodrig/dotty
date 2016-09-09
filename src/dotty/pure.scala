package dotty

/**
  * pure annotation
  * Pure methods treat all non-field variables in the enclosing environment as final and readonly, and all this-mutabilities as polymorphic.
  */
class pure extends scala.annotation.Annotation
