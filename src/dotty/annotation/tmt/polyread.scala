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
 *  @polyread specifies polymorphic parameters and results.
 *  When applied, @polyread parameters/results may be given different TMT types,
 *  depending on argument TMTs.
 *
 *  See `readonly.scala` for more information.
 *
 *  @since 2.11
 */
class polyread(references: Any *) extends StaticAnnotation with TypeConstraint
