/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation
package tmt

/** A type annotation that restricts mutations.
 *
 *  No writes may be performed to objects typed `readonly`.
 *  All members of a readonly object are also typed `readonly`.
 *
 *  @since 2.11
 */
//TODO: allow readonly to be applied via: `class C extends Readonly`?
class readonly extends StaticAnnotation with TypeConstraint
