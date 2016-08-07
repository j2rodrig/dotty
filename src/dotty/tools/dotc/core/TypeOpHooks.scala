package dotty.tools.dotc.core

import Contexts._
import Denotations._
import Types._

/**
  * Customizable type operations.
  */
class TypeOpHooks(initCtx: Context) {
  implicit val ctx: Context = initCtx

  /** The info of the given denotation, as viewed from the given prefix. */
  def denotationAsSeenFrom(pre: Type, denot: Denotation): Type = denot.info

  /** A new object of the same type as this one, using the given context. */
  def copyIn(ctx: Context) = new TypeOpHooks(ctx)
}
