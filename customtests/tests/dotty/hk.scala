import language.higherKinds

object hk {

	abstract class Functor[F[_]] {
	  def map[A, B](f: A => B): F[A] => F[B]
	}
	val ml: Functor[List] = ???
    val mx = ml
	
}
