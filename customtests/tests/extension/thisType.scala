import annotation.tmt._

// 22 errors expected

object thisType {
	var u: AnyRef
	var m: AnyRef @mutable
	var r: AnyRef @readonly
	
	trait outerType {

		var u0: AnyRef

		// Make sure @readonly applies to this
		@readonly(this) def rt1(): Unit = {
			u = this    // error expected
		}
		@readonly(this) def rt2(): Unit = {
			u = outerType.this    // error expected
		}
		@readonly(outerType.this) def rt3(): Unit = {
			u = this    // error expected
		}
		@readonly(outerType.this) def rt4(): Unit = {
			u = outerType.this    // error expected
		}
		
		// Make sure @readonly this is transitive
		@readonly(this) def rt5(): Unit = {
			u = this.u0    // error expected
		}
		@readonly(this) def rt6(): Unit = {
			u = outerType.this.u0    // error expected
		}
		@readonly(outerType.this) def rt7(): Unit = {
			u = this.u0    // error expected
		}
		@readonly(outerType.this) def rt8(): Unit = {
			u = outerType.this.u0    // error expected
		}
	
		trait innerType {
			// Make sure @readonly this works with outer types
			@readonly(this) def rt1(): Unit = {
				u = this    // error expected
			}
			@readonly(this) def rt2(): Unit = {
				u = outerType.this    // no error: this and outerType.this are different objects
			}
			@readonly(outerType.this) def rt3(): Unit = {
				u = this    // no error: this and outerType.this are different objects
			}
			@readonly(outerType.this) def rt4(): Unit = {
				u = outerType.this    // error expected
			}	
		}
	}
	
	trait superType {
		var u0: AnyRef
	}
	
	trait subType extends superType {
		// Make sure @readonly applies to super types
		@readonly(super.u0) def rs1(): Unit = {
			u = super.u0    // error expected
		}
		@readonly(super.u0) def rs2(): Unit = {
			u = subType.super.u0    // error expected
		}
		@readonly(subType.super.u0) def rs3(): Unit = {
			u = super.u0    // error expected
		}
		@readonly(subType.super.u0) def rs4(): Unit = {
			u = subType.super.u0    // error expected
		}
		
		@readonly(super.u0) def rs5(): Unit = {
			u = this.u0    // error expected
		}
		@readonly(super.u0) def rs6(): Unit = {
			u = subType.this.u0    // error expected
		}
		@readonly(subType.super.u0) def rs7(): Unit = {
			u = this.u0    // error expected
		}
		@readonly(subType.super.u0) def rs8(): Unit = {
			u = subType.this.u0    // error expected
		}
		
		@readonly(this.u0) def rs9(): Unit = {
			u = super.u0    // error expected
		}
		@readonly(this.u0) def rs10(): Unit = {
			u = subType.super.u0    // error expected
		}
		@readonly(subType.this.u0) def rs11(): Unit = {
			u = super.u0    // error expected
		}
		@readonly(subType.this.u0) def rs12(): Unit = {
			u = subType.super.u0    // error expected
		}
	}

}