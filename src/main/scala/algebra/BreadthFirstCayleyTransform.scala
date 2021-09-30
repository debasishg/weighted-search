package algebra

import cats.{ Applicative, Functor }
import cats.syntax.all._
import mset._
import MSet.Multiset
import Multiset._
import Realm._
import Ap._
import Levels._

object BreadthFirstCayleyTransform extends App with Ops {
  
  def bfe[T](t: Tree[T]): Levels[T] = {

    def f[T](t: Tree[T], ms: List[Multiset[T]]): List[Multiset[T]] = 
  	  (t, ms) match {
  		  case (Tip, qs) => qs
  
  		  case (Node(x, xs), q :: qs) => 
  			  Multiset.empty[T].insert(x).sum(q) :: 
            xs.foldRight(qs)(f)
  
        case (Node(x, xs), Nil) => 
          Multiset.empty[T].insert(x) :: 
            xs.foldRight(List.empty[Multiset[T]])(f) 
  
        case _ => ???
  		}

	  Levels(f(t, List.empty[Multiset[T]]))
	}

  val source =
    Node(
      1,
      List(
        Node(2, List(Node(5, List(Node(9, Nil), Node(10, Nil))), Node(6, Nil))),
        Node(3, Nil),
        Node(4, List(Node(7, List(Node(11, Nil), Node(12, Nil))), Node(8, Nil)))
      )
    )

  println(s"bfe: ${bfe(source)}")

	type Queue[F[_], A] = Cayley[Ap[F, *], A]

	// wrap xs = Cayley (f xs) 
	//   where f :: Applicative f ⇒ Queue f a → Ap f b → Ap f (a, b)
  //         f xs (Pure y) = Lift (const id) (pure ()) (runC xs (Pure y)) 
	//         f xs (Lift g y ys) = Lift (fmap ◦ g) y (runC xs ys)
  def wrap[F[_]: Applicative, A](qfa: Queue[F, A]): Queue[F, A] = {

    def f[F[_]: Applicative, A, B](qfa: Queue[F, A])(apfb: Ap[F, B]): Ap[F, (A, B)] = {
      (qfa, apfb) match {
        case (xs, Pure(y)) => toLift(().pure[F], xs.apply(Pure(y)))(_ => identity) 
        case (xs, Lift(g, y, ys)) => toLift(y, xs.apply(ys))(a => b => b.map(g(a)))
      }
    }

    new Cayley[Ap[F, *], A] {
      def apply[B](apfb: Ap[F, B]): Ap[F, (A, B)] = f(qfa)(apfb)
    }
  } 

  // bft :: Applicative f ⇒ (a → f b) → Tree a → f (Tree b)
  def bft[F[_]: Applicative, A, B](f: A => F[B], ta: Tree[A]): F[Tree[B]] = {
    def h: Tree[A] => Cayley[Ap[F, *], Tree[B]] = {
      case Tip => Cayley.applicative[Ap[F, *]].pure(Tip)
      case Node(x, xs) => 
        liftA2(
          Cayley.apply(lift(f(x))),
          wrap(xs traverse h)
        )((b) => (ns) => Node(b, ns)) 
    }
    lower(h(ta).rep)
  }
}
