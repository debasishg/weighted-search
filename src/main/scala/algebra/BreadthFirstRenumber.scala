package algebra

import cats.{ Applicative, Eval, Traverse }
import cats.data.{ State, StateT }
import cats.syntax.all._
import cats.effect.IO
import Ap._

object BreadthFirstRenumber extends App with Ops {

  /** 10
    * +------------------+------------------+
    * | | | 20 30 40
    * | |
    * +---+--+ +---+---+
    * | | | | 50 60 70 80
    * | |
    * +-+-+ +---+----+
    * | | | | 90 100 110 120
    */
  val source =
    Node(
      10,
      List(
        Node(20, List(Node(50, List(Node(90, Nil), Node(100, Nil))), Node(60, Nil))),
        Node(30, Nil),
        Node(40, List(Node(70, List(Node(110, Nil), Node(120, Nil))), Node(80, Nil)))
      )
    )

  // pushes its contents one level lower
  // Rather than altering the order of nondeterministic results, this alters the order of effect execution
  // as demonstrated by the `stages` function
  def wrap[F[_]: Applicative, A](fa: Ap[F, A]): Ap[F, A] =
    Lift((_: Unit) => (x: A) => x, ().pure[F], fa)

  def out(s: String) = lift {
    (IO.println(s"out: $s")).flatMap(_ => IO.pure(s))
  }

  def stages: Ap[IO, List[String]] =
    List(
      wrap(wrap(wrap(out("a")))),
      wrap(out("b")),
      wrap(wrap(out("c"))),
      out("d"),
      wrap(out("e"))
    ).sequence

  import cats.effect.unsafe.implicits.global

  println(lower(stages).unsafeRunSync())

  // bft :: Applicative f ⇒ (a → f b) → Tree a → f (Tree b)
  def bft[F[_]: Applicative, A, B](f: A => F[B], ta: Tree[A]): F[Tree[B]] = {
    def go(ta: Tree[A]): Ap[F, Tree[B]] = {
      ta match {
        case Tip => Applicative[Ap[F, *]].pure(Tip)
        case Node(x, xs) =>
          liftA2(
            lift(f(x)),
            wrap(xs traverse go)
          )((b) => (ns) => Node(b, ns))
      }
    }
    lower(go(ta))
  }

  // evalState :: State s a → s → a
  def evalState[S, A](st: State[S, A], s: S): A = st.run(s).value._2

  /** The `renumber` function numbers the nodes of a tree in breadth-first order. First, the `bft` function applies the
    * effectful action num to every value in the tree. This action produces an effect in the state monad: the state in
    * question here is an integer counter, which is first incremented using `modify` and then the updated value is
    * retrieved and returned using `get`. Finally, `evalState` runs the whole computation with an initial state of 0.
    */

  // renumber = flip evalState 0 ◦ bft num
  //   where num = do modify (+1); get
  //
  // renumber = (State s a -> a) o bft num
  def renumber[A](t: Tree[A]): Tree[Int] = {
    def num(a: A): State[Int, Int] = State.modify[Int](_ + 1).get
    evalState(bft(num, t), 0)
  }

  println(renumber(source))
}
