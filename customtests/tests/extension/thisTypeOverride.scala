import annotation.tmt._

// Tests overrides in two senses: type overrides and scope shadowing.

trait thisTypeOverride {

	// Can't enable this test b/c it causes typer error (masking override errors)
	/*class OuterType {
		@readonly(this) def o1(): Unit = {
			class InnerType {
				@mutable(this) def o11(): Unit = {   // error, but only if TMT system can't differentiate InnerType.this and OuterType.this
				}
			}
		}
	}*/
	
	class SuperType {
		@readonly(this) def o1(): Unit = {}
		@readonly(this) def o2(): Unit = {}
		@readonly(this) def o3(): Unit = {}
		@mutable(this) def o4(): Unit = {}
		@mutable(this) def o5(): Unit = {}
		@mutable(this) def o6(): Unit = {}
		def o7(): Unit = {}
		def o8(): Unit = {}
		def o9(): Unit = {}
		
		@readonly(this) def o10(): Unit = {}
		@readonly(this) def o11(): Unit = {}
		@readonly(this) def o12(): Unit = {}
	}
	class SubType extends SuperType {
		@readonly(this) override def o1(): Unit = {}
		@mutable(this) override def o2(): Unit = {}  // error expected
		override def o3(): Unit = {}  // error expected
		@readonly(this) override def o4(): Unit = {}
		@mutable(this) override def o5(): Unit = {}
		override def o6(): Unit = {}
		@readonly(this) override def o7(): Unit = {}
		@mutable(this) override def o8(): Unit = {}
		override def o9(): Unit = {}
	}
	class SubSubType extends SubType {
		def o1(): Unit = {}  // error expected
		def o2(): Unit = {}  // error expected
		def o3(): Unit = {}  // error expected
		@readonly(this) def o10(): Unit = {}
		@mutable(this) def o11(): Unit = {}  // error expected
		def o12(): Unit = {}  // error expected
	}
	
	trait E extends SubType {}
	trait F extends SubSubType {}
}