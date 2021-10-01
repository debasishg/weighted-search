package algebra

import cats.Applicative

/**
  * In a sense, Levels took the Alternative-Applicative semiring and facilitated the reordering of `Alternative` 
  * expressions with the `wrap` combinator. See Levels.scala and TreeEnumerationsAsAlgebra#bfe function.
  * 
  * However we may also want to stage and rearrange the outcomes of programs with other effects than `Alternative`.
  * A type to stage and reorder effects, then, can be constructed from a polynomial with Alternative swapped out 
  * for some monoid in a monoidal category representing effects. Applicatives themselves represent effects, as it 
  * happens, so the polynomial for rearranging and staging effects will be the polynomial over the 
  * Applicative-Applicative semiring.
  */

// The `Ap` type is the free applicative: it is a list of effectful computations, 
// terminated by a pure value, and interspersed with functions to combine the results of each computation.
abstract class Ap[F[_], +A]

case class Pure[F[_], A](a: A) extends Ap[F, A]

case class Lift[F[_], A, B, C](
  f: A => B => C,
  fa: F[A],
  fab: Ap[F, B]
) extends Ap[F, C]

object Ap extends Ops {
  // for better type inference
  def toLift[F[_], A, B, C](
    fa: F[A],
    apfb: Ap[F, B]
  )(f: A => B => C): Ap[F, C] = Lift(f, fa, apfb)
  
  // lift :: f a → Ap f a 
  // lift x = Lift const x (Pure ())
  // Any `f a` can be lifted into the free applicative
  def lift[F[_], A, B](fa: F[A]): Ap[F, A] = Lift(Function.const[A, Unit], fa, Pure(()))
  
  // lower :: Applicative f ⇒ Ap f a → f a 
  // lower (Pure x) = pure x 
  // lower (Lift f x xs) = liftA2 f x (lower xs)
  // if `f` is itself `Applicative`, `lower` defines conversion in the other direction. 
  // It uses the `liftA2` function, which combines effectful computations with a binary function
  def lower[F[_]: Applicative, A](apfa: Ap[F, A]): F[A] =
    apfa match {
      case Pure(x) => Applicative[F].pure(x)
      case Lift(f, x, xs) => liftA2(x, lower(xs))(f)
    }
  
  // `Applicative` instance for `Ap[F, A]`
  implicit def apApplicative[F[_]: Applicative]: Applicative[Ap[F, *]] = new Applicative[Ap[F, *]] {
    def pure[A](a: A) = Pure(a)
  
    def ap[A, B](ff: Ap[F, A => B])(fa: Ap[F, A]): Ap[F, B] = 
      (ff, fa) match {
        case (Pure(f), Pure(a)) => Pure(f(a))
        case (Pure(f), Lift(g, y, ys)) => toLift(y, ys)((a) => (b) => f(g(a)(b)))
        case (Lift(f, x, xs), Pure(y)) => toLift(x, xs)((x) => (xs) => f(x)(xs)(y))
        case (Lift(f, x, xs), Lift(g, y, ys)) => 
          toLift(
            liftA2 (x, y) ((a) => (b) => (a, b)),
            liftA2 (xs, ys) ((a) => (b) => (a, b)) 
          )((xs) => (ys) => f(xs._1)(ys._1)(g(xs._2)(ys._2))) 
      }
  }
}