package dotty

/**
 * Indicates that a type has the same mutability as the given reference.
 *
 * This annotation is used by the DotMod compiler extension.
 */
class mutabilityOf(ref: Any @Readonly) extends scala.annotation.StaticAnnotation
