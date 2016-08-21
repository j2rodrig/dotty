import dotty.{readonly, mutabilityOfRef}

object iterator {

  abstract class Iterator[+E] {
    def next(): E
  }

  abstract class List[+E](val head: E, val tail: List[E]) {

    def iter() = new Iterator[E] {
      private var list: List[E] = List.this
      def next(): E = {
        val h: E = list.head
        list = list.tail
        h
      }
    }
  }

  val lst: List[Any] @readonly = new List[Any](???, ???) {}
  val it: Iterator[Any] = lst.iter()
  val nxt: Any = it.next()

  val lstm: List[Any] = lst   // error
}