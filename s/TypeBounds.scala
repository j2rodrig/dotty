import annotation.tmt._

trait TypeBounds {
	type A
	type B

	type T1 >: A
	type T2 <: B
	type T3 >: A @readonly
	type T4 <: B @readonly
	type T5 >: A <: B
	type T6 >: A @readonly <: B
	type T7 >: A <: B @readonly
	type T8 >: A @readonly <: B @readonly
	
	type T9 <: (() => Unit @readonly)
}