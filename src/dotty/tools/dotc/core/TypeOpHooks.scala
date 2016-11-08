package dotty.tools.dotc.core

import Contexts._
import Denotations._
import Flags._
import Names._
import SymDenotations._
import Types._

/**
  * Customizable type operations.
  */
class TypeOpHooks(initCtx: Context) {
  implicit val ctx: Context = initCtx

  /** The info of the given denotation, as viewed from the given prefix. */
  def denotInfoAsSeenFrom(pre: Type, denot: Denotation): Type = denot.info

  /** The default denotation of a named class member. Called whenever the named member cannot be found in the given class. */
  def defaultedMember(clsDenot: ClassDenotation, name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = NoDenotation

  /** A new object of the same type as this one, using the given context. */
  def copyIn(ctx: Context) = new TypeOpHooks(ctx)
}
