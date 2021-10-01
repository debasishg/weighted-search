package algebra

import cats.{ Alternative, Applicative, Monad }
import cats.syntax.all._
import cats.effect.IO
import spire.syntax.all._
import mset._
import MSet.Multiset
import Levels._

object TreeEnumerationsAlgebra extends App with Ops {

  // depth first enumeration
  //
  // dfe :: Tree a → [a]
  //   dfe Tip = []
  //   dfe Node(x, xs) = [x] ++ choices dfe xs
  def dfe[T]: Tree[T] => List[T] = {
    case Tip         => List.empty[T]
    case Node(x, xs) => List(x) ++ choices(xs)(dfe) // parent before children
  }

  /**
    * In the `Tip` case, there are no solutions, so the empty list is returned, 
    * otherwise, the node x & xs immediately returns the solution [x] followed 
    * by the choices offered in xs, which are extracted using `choices`
    * 
    * Notice how the algebra of non-determinism is modeled using `choices`,
    * which uses the `Alternative` class to model nondeterminism: the <+> operation 
    * represents nondeterministic choice, and empty represents no solutions. This class 
    * assumes that <+> forms a monoid with `empty`, which means that <+> is associative 
    * with unit `empty`. When called by `dfe`, this specialises to the instance on lists, 
    * where empty = [], and (<+>) = (++).
    * 
    * Also note that we choose 2 operators to model non-determinism, `choices` (which use <+>)
    * and (++). Both of them have different roles to play - (++) chooses between parents and 
    * children while <+> in `choices` chooses between siblings. The definition of these operators 
    * is what dictates the order of the search. For depth-first search, parents come before their 
    * children, and children are ordered left-to-right. If this order is changed to children before 
    * parents, for instance, the tree would be enumerated in post order (see `poe` below).
    */

  // post order enumeration
  //
  // poe :: Tree a → [a] 
  // poe Tip = []
  // poe (x & xs) = [x] <++> choices poe xs 
  //   where lhs <++> rhs = rhs ++ lhs
  def poe[T]: Tree[T] => List[T] = {
    case Tip         => List.empty[T]
    case Node(x, xs) => choices(xs)(poe) ++ List(x) // children before parent
  }

  // breadth first enumeration
  //
  // bfe (x & xs) = pure x <cat> choices bfe xs 
  //   where lhs <cat> rhs = lhs <|> wrap rhs
  def bfe[T]: Tree[T] => Levels[T] = {
    case Tip          => Levels(Nil)
    case Node(x, xs)  => Levels.cat(Levels.pure(x), choices(xs)(bfe))
  }

  /**
    * Breadth first traversal results in elements in Levels, each level encoding the number of steps
    * needed to reach the level starting from root. e.g. for the following tree, `bfe` would yield
    * [{1}, {2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 12}].
    * 
    * The only difference between this function and dfe is the replacement of ++ with another ordering. 
    * Have  alook at `cats` defined in `Ops`. This ordering is described by the operators 
    * `pure`, `wrap`, and `<+>`, which assign the cost of steps.
    * 
    * The result of `pure x` assigns a cost of 0 to the value x by placing it in the first bag in the list 
    * of outcomes, and `wrap` xs increments the cost of xs by prepending an empty bag.
    * 
    * The core of `bfe` is the Levels type that allows us to stage and rearrange the outcomes of programs 
    * with nondeterminism using `Alternative`, `Applicative`, and `wrap`; we may also want to stage and rearrange 
    * the outcomes of programs with other effects. Take a look at `Ap` typeclass and `BreadthFirstRenumber`.
    * 
    */

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
}