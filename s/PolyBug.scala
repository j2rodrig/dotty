	trait Annot extends annotation.StaticAnnotation with annotation.TypeConstraint

	trait PolyBug {

		// This works
		type T
		def m(param1: T @Annot): T @Annot

		// These do not: annotations are dropped
		def m1[P](param1: P @Annot): P @Annot
		def m2[P](param1: List[P @Annot] @Annot): List[P @Annot] @Annot

		// This doesn't work either, but the annotations on T remain intact
		def m3[P](param1: Map[P @Annot,T @Annot] @Annot): Map[P @Annot,T @Annot] @Annot
	}
