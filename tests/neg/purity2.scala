import dotty.{pure, mutable, polyread, readonly}

object purity2 {

  class C

  def A = {
    var ref1 = new C

    @pure def B = {
      var ref2 = new C

      ref1 = ref2   // ERROR 1 ReimPhase
      //ref2 = ref1   // ERROR 2 ReimTyper
    }

    @pure trait D {
      var ref2 = new C
      //@pure var this1: D @mutable = this   // ERROR 10 ReimTyper
      //this1 = this

      ref1 = ref2   // ERROR 3 ReimPhase
      //ref2 = ref1   // ERROR 4 ReimTyper
    }
  }

  trait R {
    var ref1 = new C

    @pure def B = {
      var ref2 = new C

      ref1 = ref2   // ERROR 5 ReimPhase
      //ref2 = ref1   // ERROR 6 ReimTyper
    }

    @pure trait D {
      @pure var ref2: C @mutable = { ref2 = new C; new C }   // ERROR 9 ReimPhase

      ref1 = ref2   // ERROR 7 ReimPhase
      //ref2 = ref1   // ERROR 8 ReimTyper
    }
  }
}
