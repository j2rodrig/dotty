import dotty.readonly

object unannotatable {
  val x: Any = ???
  val y: x.type @readonly = x  // apparently no longer an error: x.type evaluates to Any, which is annotatable

  val z: this.type @readonly = this  // same here
}