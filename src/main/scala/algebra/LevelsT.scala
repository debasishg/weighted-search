package algebra

import cats.{ Alternative, Applicative, Functor, Monad }
import cats.syntax.all._
import mset._
import MSet.Multiset

// newtype LevelsT m a = LevelsT {runLevelsT :: m (Maybe (Bag a, LevelsT m a)) }
final case class LevelsT[F[_], A](value: F[Option[(Multiset[A], LevelsT[F, A])]])

object LevelsT {
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
    def combineK[A](l: LevelsT[F, A], r: LevelsT[F, A]): LevelsT[F, A]     = ???
    def ap[A, B](ff: LevelsT[F, A => B])(fa: LevelsT[F, A]): LevelsT[F, B] = ???
  }

  implicit def levelsTMonad[F[_]: Monad](implicit app: Applicative[LevelsT[F, *]]) = new Monad[LevelsT[F, *]] {
    def pure[A](a: A)                                                                   = app.pure(a)
    override def flatMap[A, B](fa: LevelsT[F, A])(f: A => LevelsT[F, B]): LevelsT[F, B] = ???
    def tailRecM[A, B](value: A)(f: A => LevelsT[F, Either[A, B]]): LevelsT[F, B]       = ???
  }
}
