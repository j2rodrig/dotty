import dotty._

object explicit_mutability_member {
  class C {
    type __MUTABILITY__ = readonly
  }
  class D extends C {
    override type __MUTABILITY__ = mutable  // error: incompatible type-member override
  }

  trait L {
    type __MUTABILITY__ = readonly
  }
  trait M extends L {
    override type __MUTABILITY__ = mutable  // error: incompatible type-member override
  }

  val c1: C { type __MUTABILITY__ = mutable } = ???   // ok to refine
  val c2: C { type __MUTABILITY__ = readonly } = c1   // ok to refine

  new L { override type __MUTABILITY__ = mutable }  // error: incompatible type-member override
  new M { override type __MUTABILITY__ = readonly } // error: incompatible type-member override



  //NOTE: Really what we want to do here is make sure __MUTABILITY__ is in fact a covariant type member.
  // So we need some code in DotMod to detect this.
  //However, variance is different from bounds---bounds must be respected, otherwise anything that relies
  // on the type may be unsound. Variance matters wrt. type comparison, bounds matter wrt. object construction.
  //So perhaps I was wrong about the relationship between type assignments and object construction.
  //Nevertheless, refined types are allowed to have bad bounds... an experiment:
  class X {
    type FM = Int
    val f: FM = 2000000000
  }
  val x: X { type FM = Short } = ???
  val f: Short = x.f  // ok: seems to be a problem, but since we can't replace "???" above with "new X", it's OK.

}
