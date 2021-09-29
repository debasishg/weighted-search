package algebra

import cats.{ Alternative, Applicative, Monad }
import cats.syntax.all._
import cats.effect.IO
import spire.syntax.all._
import mset._
import MSet.Multiset
import Multiset._
import Realm._
import spire.algebra.MultiplicativeMonoid
import spire.algebra.AdditiveMonoid
import spire.algebra.Eq
import spire.math.Natural
import scala.annotation.tailrec

object WeightedSearch extends App {

  /** dfe :: Tree a → [a]
    * dfe Tip = []
    * dfe Node(x, xs) = [x] ++ choices dfe xs
    */
  def dfe[T]: Tree[T] => List[T] = {
    case Tip         => List.empty[T]
    case Node(x, xs) => List(x) ++ choices(xs)(dfe)
  }

  def poe[T]: Tree[T] => List[T] = {
    case Tip         => List.empty[T]
    case Node(x, xs) => choices(xs)(poe) ++ List(x)
  }

  /** choices :: Alternative f ⇒ (a → f b) → [a] → f b
    * choices f [ ] = empty
    * choices f (x : xs) = f x <|> choices f xs
    */
  def choices[F[_]: Alternative, A, B](as: List[A])(f: A => F[B]): F[B] =
    (f, as) match {
      case (_, Nil)      => Alternative[F].empty
      case (fn, x :: xs) => fn(x) <+> choices(xs)(fn)
    }

  def choices[F[_]: Alternative, A, B](as: Multiset[A])(f: A => F[B]): F[B] =
    (f, as.toList) match {
      case (_, Nil)      => Alternative[F].empty
      case (fn, x :: xs) => fn(x) <+> choices(xs)(fn)
    }

  // that Levels represents a collection of nondeterministic computations, grouped into 
  // bags that are ordered in a list by the number of steps it took to reach them.
  // as an example have a look at the output of the `bfe` function that does a 
  // breadth first traversal of a tree
  case class Levels[T](value: List[Multiset[T]])

  def zipL[T]: List[Multiset[T]] => List[Multiset[T]] => List[Multiset[T]] = first =>
    second =>
      (first, second) match {
        case (f, Nil)           => f
        case (Nil, s)           => s
        case (f :: fs, s :: ss) => f.sum(s) :: zipL(fs)(ss)
      }

  implicit def msetApplicative[F: MultiplicativeMonoid: AdditiveMonoid: Eq]: Applicative[MSet[F, *]] = 
    new Applicative[MSet[F, *]] {
      override def product[A, B](fa: MSet[F, A], fb: MSet[F, B]): MSet[F, (A, B)] = fa.product(fb)

      def pure[A](a: A) = MSet.empty[F, A].insert(a)

      override def map[A, B](fa: MSet[F, A])(f: A => B): MSet[F, B] = fa map f

      def ap[A, B](fab: MSet[F, A => B])(fa: MSet[F, A]): MSet[F, B] = map2(fab, fa)(_(_))
  }

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

  object Levels {
    def pure[T]: T => Levels[T]         = t => Levels(List(Multiset(t)))
    def wrap[T]: Levels[T] => Levels[T] = xs => Levels(Multiset.empty[T] :: xs.value)
    def cat[T](lhs: Levels[T], rhs: Levels[T]) = lhs <+> Levels.wrap(rhs)
  }

  // bfe (x & xs) = pure x <cat> choices bfe xs 
  //   where lhs <cat> rhs = lhs <|> wrap rhs
  def bfe[T]: Tree[T] => Levels[T] = {
    case Tip         => Levels(Nil)
    case Node(x, xs) => Levels.cat(Levels.pure(x), choices(xs)(bfe))
  }


  /**                           1
    *        +------------------+------------------+
    *        |                  |                  |
    *        2                  3                  4
    *        |                                     |
    *    +---+--+                              +---+---+
    *    |      |                              |       |
    *    5      6                              7       8
    *    |                                     |
    *  +-+-+                               +---+----+
    *  |   |                               |        |
    *  9  10                              11        12
    */
  val source =
    Node(
      1,
      List(
        Node(2, List(Node(5, List(Node(9, Nil), Node(10, Nil))), Node(6, Nil))),
        Node(3, Nil),
        Node(4, List(Node(7, List(Node(11, Nil), Node(12, Nil))), Node(8, Nil)))
      )
    )

  // List(1, 2, 5, 9, 10, 6, 3, 4, 7, 11, 12, 8)
  println(s"dfe: ${dfe(source)}")
  // List(9, 10, 5, 6, 2, 3, 11, 12, 7, 8, 4, 1)
  println(s"poe: ${poe(source)}")
  // Levels(List(List(1), List(2, 3, 4), List(5, 6, 7, 8), List(9, 10, 11, 12)))
  // Levels(List(MSet(Map(1 -> 1)), MSet(Map(3 -> 1, 4 -> 1, 2 -> 1)), MSet(Map(5 -> 1, 6 -> 1, 7 -> 1, 8 -> 1)), MSet(Map(9 -> 1, 10 -> 1, 11 -> 1, 12 -> 1))))
  println(s"bfe: ${bfe(source)}")

  /**
    * Representing polynomials with Levels
    * It is possible to represent a polynomials of a single variable (in normalised form) with a list of 
    * coeﬃcients, where the 𝑖th entry in the list is the coeﬃcient of the term x^i.
    * 
    * (2x^2 + 1)(x^2 + 2) = 2x^4 + 4x^2 + x^2 + 2 = 2x^0 + 0x^1 + 5x^2 + 0x^3 + 2x^4 = [2, 0, 5, 0, 2]
    * 
    * Addition:
    * (2x^2 + 1) + (x^3 + 2) = Levels [1, 0, 2] <+> Levels [2, 0, 0, 1] = Levels [3, 0, 2, 1] = 3 + 2x^2 + x^3
    * 
    * Multiplication:
    * (2x^2 + 1) + (x^2 + 2) = Levels [1, 0, 2] >> Levels [2, 0, 1] = Levels [2, 0, 5, 0, 2] = 2x^4 + 5x^2 + 2
    */

  def makeBag(n: Int): Multiset[Unit] = Multiset.empty[Unit].insertN((), Natural(n))
  def makeBags(ns: List[Int]): List[Multiset[Unit]] = ns map makeBag

  // Levels [1, 0, 2] => 2x^2 + 1
  val p1 = Levels(makeBags(List(1, 0, 2)))
  // Levels [2, 0, 0, 1] => x^3 + 2
  val p2 = Levels(makeBags(List(2, 0, 0, 1)))
  // Levels [2, 0, 1] => x^2 + 2
  val p3 = Levels(makeBags(List(2, 0, 1)))

  // Addition
  // Levels(List(MSet(Map(() -> 3)), MSet(Map()), MSet(Map(() -> 2)), MSet(Map(() -> 1))))
  // 3 + 2x^2 + x^3
  println(p1 <+> p2)

  // Multiplication
  // Levels(List(MSet(Map(() -> 2)), MSet(Map()), MSet(Map(() -> 5)), MSet(Map()), MSet(Map(() -> 2))))
  // 2x^4 + 5x^2 + 2
  println(p1 >> p3)
}