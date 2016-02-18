package dotty

import scala.annotation.meta._

@readonly trait Iterator[T] {
  def next: T
}

@readonly class List[T](val hd: T, val tail: List[T]) {

  @rothis def iterator: Iterator[T @mutabilityOf(List.this)] @mutable = {
    // @mutabilityOf[List.this] here is the polymorphic type @rothis(with origin iterator).

    @mutable class MyIterator extends Iterator[T @mutabilityOf(List.this)] {  // takes an argument List.this
      private var nxt: List[T @mutabilityOf(List.this)] = List.this
      @mutable def next: T @mutabilityOf(List.this) = {
        val t: T @mutabilityOf(List.this) = nxt.hd
        nxt = nxt.tail
        t
      }
    }

    // The call to the constructor of MyIterator involves substitution of all @mutabilityOf(List.this)
    //  in the new object.
    // See: Type#subst and Substituters#subst1 and called/related methods for reference.
    new MyIterator
  }
}

object A extends App {
  val l: List[Any] @readonly = new List[Any](null, null)

  // The application of l.iterator involves substitution of all @mutabilityOf(List.this) in
  //  the iterator signature with the actual receiver mutability l, which here is @readonly.
  // See: Type#subst and Substituters@subst1 and called/related methods for reference.
  val it: Iterator[Any @readonly] = l.iterator

  val t: Any = it.next
}
