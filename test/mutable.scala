package scala.annotation

/**
 * Indicates that a type is readonly (in a reference immutability system).
 *
 * This annotation is used by the DotMod compiler extension.
 */
class mutable extends StaticAnnotation with TypeConstraint
class polyread extends StaticAnnotation with TypeConstraint
class readonly extends StaticAnnotation with TypeConstraint
