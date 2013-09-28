package preamble

import scala.language.higherKinds
import scala.concurrent.{ExecutionContext, Future}

// TODO Check usefullness of @inline usage

trait Applicative[F[_]] {
  val functor: Functor[F] = ???
  def pure[A]: A => F[A]

  // <*>
  def <*>[A, B]: F[A => B] => F[A] => F[B]
  /*
(<*>) :: f (a -> b) -> f a -> f b
Sequential application.

(*>) :: f a -> f b -> f bSource
Sequence actions, discarding the value of the first argument.

(<*) :: f a -> f b -> f a
Sequence actions, discarding the value of the second argument. 
*/

}
trait Functor[F[_]] { def lift[A, B]: (A => B) => F[A] => F[B] }

object Functor {
  implicit val option = new Functor[Option] { @inline def lift[A, B] = f => opt => opt.map(f) }
  implicit val list = new Functor[List] { @inline def lift[A, B] = f => xs => xs.map(f) }
  implicit val seq = new Functor[Seq] { @inline def lift[A, B] = f => xs => xs.map(f) }
  implicit val set = new Functor[Set] { @inline def lift[A, B] = f => xs => xs.map(f) }
  implicit val traversable = new Functor[Traversable] { @inline def lift[A, B] = f => xs => xs.map(f) }
  implicit def future(implicit ec: ExecutionContext) = new Functor[Future] { @inline def lift[A, B] = f => xs => xs.map(f) }
}

trait Monad[M[_]] {
  implicit val functor: Functor[M]
  def inject[A]: A => M[A]
  def >>=[A, B]: M[A] => (A => M[B]) => M[B] 
  def >>[A, B]: M[A] => M[B] => M[B] = _ => identity
}

object Monad {
  implicit val option = new Monad[Option] {
    val functor = Functor.option
    @inline def inject[A] = Some(_)
    @inline def >>=[A, B] = xs => f => xs.flatMap(f)
  }
  implicit val list = new Monad[List] {
    val functor = Functor.list
    @inline def inject[A] = List(_)
    @inline def >>=[A, B] = xs => f => xs.flatMap(f)
  }
  implicit val seq = new Monad[Seq] {
    val functor = Functor.seq
    @inline def inject[A] = Seq(_)
    @inline def >>=[A, B] = xs => f => xs.flatMap(f)
  }
  implicit val set = new Monad[Set] {
    val functor = Functor.set
    @inline def inject[A] = Set(_)
    @inline def >>=[A, B] = xs => f => xs.flatMap(f)
  }
  implicit val traversable = new Monad[Traversable] {
    val functor = Functor.traversable
    @inline def inject[A] = Seq(_)
    @inline def >>=[A, B] = xs => f => xs.flatMap(f)
  }
  implicit def future(implicit ec: ExecutionContext) = new Monad[Future] {
    val functor = Functor.future
    @inline def inject[A] = Future(_)
    @inline def >>=[A, B] = xs => f => xs.flatMap(f)
  }
}


sealed trait Validation[F, S] {
  type Failure[F0] = Validation[F0, S]
  type Success[S0] = Validation[F, S0]
  def failure: Failure[F] = sys.error("Calling 'failure' on Success")
  def success: Success[S] = sys.error("Calling 'success' on Failure")
}

object Validation {
  case class Success[F, S](value: S) extends Validation[F, S] { override def success = this }
  case class Failure[F, S](value: F) extends Validation[F, S] { override def failure = this }

  implicit def _functor[F, S]= new Functor[Validation[F, S]#Success] {
    def lift[A, B]: (A => B) => Validation[F, S]#Success[A] => Validation[F, S]#Success[B] = f => _ match {
      case Success(value) => Success(f(value))
      case Failure(value) => Failure(value)
    }
  }

  implicit def _applicative[F, S] = new Applicative[Validation[F, S]#Success] {
    def pure[A]: A => Validation[F, A] = Success(_)
    def <*>[A, B]: Validation[F,A => B] => (Validation[F,A] => Validation[F,B]) = _ match {
      case Success(f) => <*>[A, B](pure[A => B](f))
      case Failure(value) => _ => Failure(value)
    }
  }
}

object Preamble {
  def functor[F[_]](implicit functor: Functor[F]) = functor
  def monad[F[_]](implicit monad: Monad[F]) = monad

  @inline def map[F[_] : Functor, A, B](x: F[A])(f: A => B) = functor[F].lift(f)(x)
  @inline def flatMap[M[_] : Monad, A, B](x: M[A])(f: A => M[B]) = monad[M].>>=(x)(f)
}

object Demo {
  import Preamble._

  val xs = map(Seq(1, 2, 3)) {
    case i if i > 2 => Option(i + i)
    case _ => None
  }

  map(xs)(_.headOption)

}

/*
object Syntax {
  object Do {
    def apply[T](f: => T): T = f
  }

  import Prelude._
  import Validation._

  @Do(Future, Option)
  def loadData(venueId: Int): Venue = {
    val venue = find(venueId)
    val translations = findAll(venue.translationId)
    val imageUrl = find(venueId)
    Venue(name, translations, imageUrl)
  }

  // If Venue(Venue, Seq[Translation], Url)
  def loadData(venueId: Int): Future[Option[Venue]] = {
    val venue = find(venueId)
    val imageUrl = find(venueId)
    flatMap(translations)(venue) { venue =>
      val translations = findAll(venue.translationId)
      map(translations)(translations => {
        val imageUrl = find(venueId)
        map(imageUrl)(imageUrl => {
          Venue(venue, translations, imageUrl)
        })
      })
    }
  }
}
*/
