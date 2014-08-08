package test

import scala.annotation.Annotation

// efftp
// ReIm / JPure

// An empty write-set means "pure."
// Writes to local variables are masked, so these do not contribute to impurity.

// "Readonly" is a property of a reference that means: Nobody is permitted to modify the object pointed to by this reference.
// At this juncture, there is a choice with respect to each field: do we constrain whether users can modify the object pointed to by the field?
// A: No, unless the programmer says the field has constraints.

// The opposite of "will not mutate" is "may mutate." ReIm enforces a property that says, "if a reference is readonly, then
//  all fields of the referenced object are also readonly (from the viewpoint of that reference)."
// It is OK to assign a mutable reference to a readonly variable, because although the reference is mutable from
//  one viewpoint, it may be readonly from another viewpoint.

// "Readonly" is a property of values and references.
// The analysis accumulates write-sets for each function.
// The presence of a value or reference in a write-set means that the function may-write to that value/reference,
// or anything transitively mutable through that reference (if it is a reference).
// Passing a mutable reference to a function of unknown effect causes that reference to be unconditionally placed
// in the caller's write-set.

// Select/assign chart:
// qualifier    field_name    result     may_assign            notes
// readonly     *             readonly   not allowed           -
// mutable      readonly      readonly   mutable or readonly   assigning mutable adds a readonly constraint
// mutable      mutable       mutable    mutable               assigning readonly would drop the readonly constraint

// Apply nodes: union of write-set

// Assign nodes:
// Need to find mutation type of left-hand side and right-hand side. Issue error if not compatible.
// Add left-hand side denotation to the write-set.

// ValDef nodes:
// Need to find the right-hand mutation type. Issue error if not compatible with left-hand modifiers.
// A ValDef does not need to add anything to the write-set, since it cannot be modified by definition.

// Return nodes:
// The return-expression's mutation-type is checked against the function's declared mutation type.
// For mutliple return nodes in a function, the union of constraints is taken.

// The "readonly" property is a guarantee that the write-set does not include anything transitively accessed through that reference.
// A function's write set may include any object transitively accessed through mutable references.
// (If any field so accessed has a @readonly modifier, then anything accessed through that field may be excluded from the write set.)
// An alternative way to state these properties is:
// Any mutable reference may be included in the write set, along with any reference transitively reachable through it.

class writeset(references: Any*) extends Annotation {}

class readonly extends Annotation {}
//class readonly(references: Any*) extends Annotation {}  // allows arguments

//class my () {
//	my.q()
//}

object my {

def enclosing = {

	def a (vp: Int) = {
		val r = 20
		var s = vp
		s = 10
		vp
	}
	
}

//	trait ATrait (val d: Any) { q() }
//	abstract class X extends ATrait (null) { q() }

//	@readonly def q () = {
//		val x = new my()
//	}
}

//case class B4R () {
//	my.q()
//}
