import annotation.tmt._

trait experimental {

	class OuterType {
		@readonly(this) def o1(): Unit = {
			o11()
			@mutable(this) def o11(): Unit = {  // error expected
				//o11()
			}
			//o1()
			//o2()
			this.o2()
		}
		@mutable(this) def o2(): Unit = {
		}
	}
}
