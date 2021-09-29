package algebra

import cats.{ Applicative, Alternative }
import cats.syntax.all._
import mset._
import MSet.Multiset
import Multiset._
import spire.algebra.MultiplicativeMonoid
import spire.algebra.AdditiveMonoid

trait Ops {
  def zipL[T]: List[Multiset[T]] => List[Multiset[T]] => List[Multiset[T]] = first =>
    second =>
      (first, second) match {
        case (f, Nil)           => f
        case (Nil, s)           => s
        case (f :: fs, s :: ss) => f.sum(s) :: zipL(fs)(ss)
      }

  // liftA2 :: Applicative f ⇒ (a → b → c) → f a → f b → f c 
  // liftA2 f xs ys = pure f <*> xs <*> ys
	def liftA2[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B])(f: A => B => C) = 
		(Applicative[F].pure(f) <*> fa ) <*> fb

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

  implicit def msetApplicative[F: MultiplicativeMonoid: AdditiveMonoid: Eq]: Applicative[MSet[F, *]] = 
    new Applicative[MSet[F, *]] {
      override def product[A, B](fa: MSet[F, A], fb: MSet[F, B]): MSet[F, (A, B)] = fa.product(fb)

      def pure[A](a: A) = MSet.empty[F, A].insert(a)

      override def map[A, B](fa: MSet[F, A])(f: A => B): MSet[F, B] = fa map f

      def ap[A, B](fab: MSet[F, A => B])(fa: MSet[F, A]): MSet[F, B] = map2(fab, fa)(_(_))
  }
}
