package algebra

import cats.Applicative
import cats.syntax.all._

trait Ops {
  // liftA2 :: Applicative f ⇒ (a → b → c) → f a → f b → f c 
  // liftA2 f xs ys = pure f <*> xs <*> ys
	def liftA2[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B])(f: A => B => C) = 
		(Applicative[F].pure(f) <*> fa ) <*> fb
}
