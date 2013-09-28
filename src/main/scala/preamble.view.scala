package preamble
package view

import scala.language.existentials // TODO Remove once cleaned operation
import scala.language.higherKinds

class View[TC[_], A, ITC[_], I](operations: Seq[View.Operation]) {
  import View._

  def apply(in: ITC[I])(implicit viewable: Viewable[ITC]): TC[A] = viewable[I, A, TC](operations)(in)
  def head(implicit ev: TC[_] <:< Traversable[_]) = new View[Option, I, ITC, I](Operation.Head +: operations)
  def map[B](f: A => B)(implicit functor: Functor[TC]) = new View[TC, B, ITC, I](Operation.Map(f)(functor.lift(f)(_: TC[A])) +: operations)
  def flatMap[B](f: A => TC[B])(implicit monad: Monad[TC]) = new View[TC, B, ITC, I](Operation.FlatMap(f)(monad.>>=(_: TC[A])(f)) +: operations)
}

object View {
  sealed trait Operation

  object Operation {
    case object Head extends Operation
    // TODO Clean to Any with polyconstructor doing the cast
    case class Map(apply: _ => _)(functor: _ => _) extends Operation
    case class FlatMap(apply: _ => _)(monad: _ => _) extends Operation
  }

  def apply[TC[_] : Viewable, A] = new View[TC, A, TC, A](Nil)
}

trait Viewable[ITC[_]] {
  def apply[I, O, OTC[_]](operations: Seq[View.Operation])(in: ITC[I]): OTC[O]
}

object Viewable {
  import View._

  implicit def cbf[ITC[_] <: TraversableOnce[_], OTC[_]](implicit cbf: collection.generic.CanBuildFrom[_, Any, OTC[Any]]) = new Viewable[ITC] {
    def apply[I, O, OTC[_]](operations: Seq[View.Operation])(_xs: ITC[I]): OTC[O] = {
      val xs = _xs.asInstanceOf[Traversable[I]]
      val ys = cbf()

      var one: Option[O] = None
      
      val ops = operations.foldRight[Any => Any](identity _) { (operation, f) => operation match {
          case Operation.Head => 
            x => { one = Some(f.asInstanceOf[Any => O].apply(x)); x }
          case Operation.Map(apply) => 
            x => apply.asInstanceOf[Any => Any].apply(f.asInstanceOf[Any => Any].apply(x)) 
          case Operation.FlatMap(apply) => 
            x => ???
        }
      }

      val iterator = xs.toIterator

      while (one == None && iterator.hasNext) {
        val x = iterator.next
        ys += ops(x)
      }

      (if (one.isDefined) one else ys.result).asInstanceOf[OTC[O]]
    }
  }
  /*
  implicit val traversable = new Viewable[Traversable] {
    def apply[I, O](operations: Seq[View.Operation[_, _]])(xs: Traversable[I]): Traversable[O] = {
      val ys = Traversable.newBuilder[O]
      
      val ops = operations.foldRight[Any => Any](identity _) { (operation, f) => 
        x => operation.asInstanceOf[Any => Any].apply(f.asInstanceOf[Any => Any].apply(x)) 
      }

      xs.foreach { x =>
        ys += ops(x).asInstanceOf[O]
      }
      ys.result
    }
  }
  */
}


/*
scala> View[Traversable, Int].map(x => {println(x); x.toString}).map(_ * 5).map(_.toUpperCase).head.apply(1 to 10)
1
res1: Option[Int] = Some(11111)

scala> (1 to 10).map(x => {println(x); x.toString}).map(_ * 5).map(_.toUpperCase).head
*/
