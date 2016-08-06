import dotty.readonly

object unannotatable {
  val x: Any = ???
  val y: x.type @readonly = x  // error
}