package algebra

import cats.{ Alternative, Applicative, Functor, Monad }
import cats.syntax.all._
import mset._
import MSet.Multiset
import annotation.tailrec

/** Monads such as the Levels type are a convenient abstraction that enable specific effects to be expressed in a
  * controlled manner within a program. The problem with monads, however, is that they do not generally compose with one
  * another. The solution to this is to provide a monad transformer, which takes a monad m, and augments it to a monad t
  * m with additional effects.
  *
  * The following introduces the `LevelsT` monad transformer, which adds the effect of breadth-first backtracking with
  * `Levels` to an arbitrary monad
  */

// newtype LevelsT m a = LevelsT {runLevelsT :: m (Maybe (Bag a, LevelsT m a)) }
final case class LevelsT[F[_], A](value: F[Option[(Multiset[A], LevelsT[F, A])]])

/** This transforms a monad `m` by adding a list (of bags) whose constructors are nested by m. The list [{1},{2},{3}]
  * could be encoded in `LevelsT m (Just ({1}, m (Just ({2}, m (Just ({3}, m Nothing))))))`, although this
  * representation intersperses every bag in the list with some effect m. The cons constructor `(:)` has been replaced
  * by `Just`, and the `nil` constructor `([])` has been replaced by `Nothing`.
  */

object LevelsT extends Ops {
  implicit def levelsTFunctor[F[_]: Functor] = new Functor[LevelsT[F, *]] {
    override def map[A, B](fa: LevelsT[F, A])(f: A => B): LevelsT[F, B] = {
      def go(fa: LevelsT[F, A]): LevelsT[F, B] =
        LevelsT(fa.value.map { _.map { case (b, l) => (b.map(f), go(l)) } })
      go(fa)
    }
  }

  implicit def levelsTAlternative[F[_]](implicit ev: Monad[F]) = new Alternative[LevelsT[F, *]] {
    def pure[A](a: A) = LevelsT(ev.pure(Some((Multiset(a), LevelsT(ev.pure(None))))))
    def empty[A]      = LevelsT(ev.pure(None))
    def combineK[A](l: LevelsT[F, A], r: LevelsT[F, A]): LevelsT[F, A] = {
      def go: Option[(Multiset[A], LevelsT[F, A])] => Option[(Multiset[A], LevelsT[F, A])] => Option[
        (Multiset[A], LevelsT[F, A])
      ] = left =>
        right =>
          (left, right) match {
            case (None, r)                      => r
            case (l, None)                      => l
            case (Some((x, xs)), Some((y, ys))) => Some((x.sum(y), combineK(xs, ys)))
          }

      LevelsT(liftA2(l.value, r.value)(go))
    }
    def ap[A, B](ff: LevelsT[F, A => B])(fa: LevelsT[F, A]): LevelsT[F, B] = {
      def go: Option[(Multiset[A => B], LevelsT[F, A => B])] => Option[(Multiset[A], LevelsT[F, A])] => Option[
        (Multiset[B], LevelsT[F, B])
      ] = ff =>
        fa =>
          (ff, fa) match {
            case (None, _)                      => None
            case (_, None)                      => None
            case (Some((x, xs)), Some((y, ys))) => Some((x.ap(y), ap(xs)(ys)))
          }

      LevelsT(liftA2(ff.value, fa.value)(go))
    }
  }

  def choices[F[_]: Monad, A, B](as: Multiset[A])(f: A => LevelsT[F, B]): LevelsT[F, B] =
    (f, as.toList) match {
      case (_, Nil)      => LevelsT(Applicative[F].pure(None))
      case (fn, x :: xs) => fn(x) <+> choices(xs)(fn)
    }

  // wrap xs = LevelsT (pure (Just ( {} , xs)))
  def wrap[F[_]: Monad, A](l: LevelsT[F, A]) = LevelsT((Option((Multiset.empty[A], l))).pure[F])

  implicit def levelsTMonad[F[_]](implicit app: Applicative[LevelsT[F, *]], ev: Monad[F]) =
    new Monad[LevelsT[F, *]] {
      def pure[A](a: A) = app.pure(a)

      override def flatMap[A, B](fa: LevelsT[F, A])(f: A => LevelsT[F, B]): LevelsT[F, B] = {

        def go(fa: Option[(Multiset[A], LevelsT[F, A])]): LevelsT[F, B] =
          fa match {
            case None => pure(None.asInstanceOf[B])
            case Some((x, xs)) =>
              choices(x)(f) <+> wrap(flatMap(xs)(f))
          }

        LevelsT(fa.value.flatMap(a => go(a).value))
      }

      def tailRecM[A, B](value: A)(f: A => LevelsT[F, Either[A, B]]): LevelsT[F, B] = {

        def go(fa: Option[(Multiset[Either[A, B]], LevelsT[F, Either[A, B]])]): LevelsT[F, B] =
          fa match {
            case None => pure(None.asInstanceOf[B])
            case Some((x, xs)) =>
              x.occurList match {
                case Nil => LevelsT(ev.pure(None))
                case (ab, n) :: tail =>
                  ab match {
                    case Left(a)  => LevelsT(go0(f(a) <+> xs))
                    case Right(b) => pure(b)
                  }
              }
          }

        def go0(fab: LevelsT[F, Either[A, B]]) = fab.value.flatMap(a => go(a).value)

        LevelsT(f(value).value.flatMap(a => go(a).value))
      }
    }
}

import cats.effect.IO
object LevelsTApp extends App {
  import LevelsT._

  def printLevelsT(l: LevelsT[IO, Int], acc: Multiset[Int]): IO[Unit] = {
    l.value.flatMap { ops =>
      ops match {
        case None          => IO.println(acc.occurList)
        case Some((x, xs)) => printLevelsT(xs, acc.sum(x))
      }
    }
  }

  val x = LevelsT[IO, Int](
    IO(
      Some(
        (
          Multiset(1),
          LevelsT[IO, Int](
            IO(Some((Multiset(2), LevelsT[IO, Int](IO(Some((Multiset(3), LevelsT[IO, Int](IO(None)))))))))
          )
        )
      )
    )
  )
  val y = LevelsT[IO, Int](
    IO(
      Some(
        (
          Multiset(10),
          LevelsT[IO, Int](
            IO(Some((Multiset(2), LevelsT[IO, Int](IO(Some((Multiset(30), LevelsT[IO, Int](IO(None)))))))))
          )
        )
      )
    )
  )
  import cats.effect.unsafe.implicits.global
  import cats.Id

  printLevelsT(x <+> y, Multiset.empty[Int]).unsafeRunSync()
}
