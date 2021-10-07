package algebra

import cats.{ Alternative, Applicative, Functor, Monad }
import cats.syntax.all._
import mset._
import MSet.Multiset

// newtype LevelsT m a = LevelsT {runLevelsT :: m (Maybe (Bag a, LevelsT m a)) }
final case class LevelsT[F[_], A](value: F[Option[(Multiset[A], LevelsT[F, A])]])

object LevelsT extends Ops {
  implicit def levelsTFunctor[F[_]: Functor] = new Functor[LevelsT[F, *]] {
    override def map[A, B](fa: LevelsT[F, A])(f: A => B): LevelsT[F, B] = {
      def go(fa: LevelsT[F, A]): LevelsT[F, B] =
        LevelsT(fa.value.map { _.map { case (b, l) => (b.map(f), go(l)) } })
      go(fa)
    }
  }

  implicit def levelsTAlternative[F[_]: Monad] = new Alternative[LevelsT[F, *]] {
    def pure[A](a: A) = LevelsT(Monad[F].pure(Some((Multiset(a), LevelsT(Monad[F].pure(None))))))
    def empty[A]      = LevelsT(Monad[F].pure(None))
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
      case (fn, x :: xs) => Alternative[LevelsT[F, *]].combineK(fn(x), choices(xs)(fn))
    }

  // wrap xs = LevelsT (pure (Just ( {} , xs)))
  def wrap[F[_]: Monad, A](l: LevelsT[F, A]) = LevelsT((Option((Multiset.empty[A], l))).pure[F])

  implicit def levelsTMonad[F[_]: Monad](implicit app: Applicative[LevelsT[F, *]]) = new Monad[LevelsT[F, *]] {
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
    def tailRecM[A, B](value: A)(f: A => LevelsT[F, Either[A, B]]): LevelsT[F, B] = ???
  }
}
