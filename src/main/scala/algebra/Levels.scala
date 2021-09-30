package algebra

import cats.{ Alternative, Applicative, Monad }
import cats.syntax.all._
import mset._
import MSet.Multiset
import spire.algebra.MultiplicativeMonoid
import spire.algebra.AdditiveMonoid
import scala.annotation.tailrec

// that Levels represents a collection of nondeterministic computations, grouped into 
// bags that are ordered in a list by the number of steps it took to reach them.
// as an example have a look at the output of the `bfe` function that does a 
// breadth first traversal of a tree
case class Levels[T](value: List[Multiset[T]])

/**
 * One way to think of the Levels type is as nondeterministic computations where the outcomes 
 * are grouped into buckets by the number of łstepsž needed to reach an outcome. For example, 
 * the value Levels [{a}, {}, {b, c}, {d}] represents a computation which has 4 outcomes 
 * (a, b, c and d), where a takes 0 steps to reach, b and c both take 2, and d takes 3. 
 */

object Levels extends Ops {

  implicit val levelsAlternative = new Alternative[Levels] {
    def pure[A](a: A) = Levels(List(Multiset(a)))

    def empty[A] = Levels(List.empty[Multiset[A]])

    def combineK[A](l: Levels[A], r: Levels[A]): Levels[A] = Levels(zipL(l.value)(r.value))

    /*
    def ap[A, B](ff: Levels[A => B])(fa: Levels[A]): Levels[B] = {
      val v = for {
        la  <- fa.value
        lab <- ff.value
      } yield lab <*> la
      Levels(v)
    }
    */

    // Levels (x : xs) <*> Levels ys = Levels (map (x <*>) ys) <|> wrap (Levels xs <*> Levels ys)
    def ap[A, B](ff: Levels[A => B])(fa: Levels[A]): Levels[B] = {
      (ff, fa) match {
        case (Levels(Nil), _) => Levels(Nil)
        case (Levels(x :: xs), Levels(ys)) => combineK(Levels((ys.map(x <*> _))), Levels.wrap(ap(Levels(xs))(Levels(ys))))
      }
    }
  }

  implicit def levelsMonad(implicit app: Applicative[Levels]) = new Monad[Levels] {
    def pure[A](a: A) = app.pure(a)

    override def flatMap[A, B](fa: Levels[A])(f: A => Levels[B]): Levels[B] = fa.value match {
      case Nil => Levels(List.empty[Multiset[B]])
      case ms :: Nil => choices(ms)(f)
      case ms :: mss => choices(ms)(f) <+> Levels.wrap(flatMap(Levels(mss))(f))
    }

    def tailRecM[A, B](value: A)(f: A => Levels[Either[A, B]]): Levels[B] = {
      val buf = collection.mutable.ListBuffer.empty[Multiset[B]]

      @tailrec 
      def go(msets: List[Multiset[Either[A, B]]]): Unit = {
        msets match {
          case Nil => ()
          case m :: ms =>
            goOne(m)
            go(ms)
        }
      }

      @tailrec
      def goOne(mset: Multiset[Either[A, B]]): Unit = {
        mset.occurList match {
          case Nil => ()
          case (ab, n) :: tail =>
            ab match {
              case Left(a) => 
                go(f(a).value) 
                goOne(MSet.fromOccurList(tail))
              case Right(b) => 
                buf += Multiset.empty[B].insertN(b, n)
                goOne(MSet.fromOccurList(tail))
            }
        }
      }

      go(f(value).value) 
      Levels(buf.toList)
    }
  }

  def pure[T]: T => Levels[T]         = t => Levels(List(Multiset(t)))
  def wrap[T]: Levels[T] => Levels[T] = xs => Levels(Multiset.empty[T] :: xs.value)
  def cat[T](lhs: Levels[T], rhs: Levels[T]) = lhs <+> Levels.wrap(rhs)
}