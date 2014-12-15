import annotation.tmt._

object immutable {

	abstract class List[+A] {
		def isEmpty: Boolean
		def head: A
		def tail: List[A]

		// New methods in List

		/** Adds an element at the beginning of this list.
		 *  @param x the element to prepend.
		 *  @return  a list which contains `x` as first element and
		 *           which continues with this list.
		 *
		 *  @usecase def ::(x: A): List[A]
		 *    @inheritdoc
		 *
		 *    Example:
		 *    {{{1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)}}}
		 */
		def ::[B >: A] (x: B @readonly): List[B] =
			new immutable.::(x, this)

	}

	/** A non empty list characterized by a head and a tail.
	 *  @param hd   the first element of the list
	 *  @param tl   the list containing the remaining elements of this list after the first one.
	 *  @tparam B   the type of the list elements.
	 *  @author  Martin Odersky
	 *  @version 1.0, 15/07/2003
	 *  @since   2.8
	 */
	final case class ::[B](override val head: B, private[immutable] var tl: List[B]) extends List[B] {
	  override def tail : List[B] = tl
	  override def isEmpty: Boolean = false
	}
	/*	def ::[B](hd: B, _tl: List[B]) =
			new List[B] {
				override def head = hd
				var tl = _tl
				override def tail = tl
				override def isEmpty = false
			}*/

	object List {
	}
}
