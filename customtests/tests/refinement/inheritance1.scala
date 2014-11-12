import annotation.tmt._

// 

trait inheritance1 {

	class LRm[AnyRef @readonly] extends List[AnyRef @mutable] {}

	class LMr[AnyRef @mutable] extends List[AnyRef @reaonly] {}

}

