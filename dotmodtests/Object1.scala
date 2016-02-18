package dotty

import dotty.tools.dotc.util.DotClass

object Object1 {

  trait PhasesBase {
    object SomePhase extends Phase {
      val x2 = this
    }
  }

  trait Phase extends DotClass { self =>
    val x2: Phase @readonly
    self.clone()
    x2.clone()
  }

}