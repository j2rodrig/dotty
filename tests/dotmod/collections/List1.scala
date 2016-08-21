import dotty._

object List1 {

  abstract class List[A] {
      def isEmpty: Boolean;
      def head: A;
      def tail: List[A];
      def prepend(x: A) = Cons[A](x, this);
  }

  def Nil[B] = new List[B] {
    def isEmpty: Boolean = true;
    def head = sys.error("head of Nil");
    def tail = sys.error("tail of Nil");
  }

  def Cons[C](x: C, xs: List[C]): List[C] = new List[C] {
    def isEmpty = false;
    def head = x;
    def tail = xs;
  }

  def foo = {
    val intnil = Nil[Int];
    val intlist = intnil.prepend(1).prepend(1 + 1);
    val x: Int = intlist.head;
    val strnil = Nil[String];
    val strlist = strnil.prepend("A").prepend("AA");
    val y: String = strlist.head;
    ()
  }

  class IntList() extends List[Int] {
    def isEmpty: Boolean = false;
    def head: Int = 1;
    def foo: List[Int] { def isEmpty: Boolean; def head: Int; def tail: List[Int] } = Nil[Int];
    def tail0: List[Int] = foo.prepend(1).prepend(1 + 1);
    def tail: List[Int] = Nil[Int].prepend(1).prepend(1 + 1);
  }

  def foo2 = {
    val il1 = new IntList();
    val il2 = il1.prepend(1).prepend(2);
    ()
  }
}
