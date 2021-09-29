package algebra

import cats.{ Alternative, Applicative, Monad }
import cats.syntax.all._
import cats.effect.IO
import spire.syntax.all._
import mset._
import MSet.Multiset
import Levels._

object TreeEnumerationsAlgebra extends App with Ops {

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

  // bfe (x & xs) = pure x <cat> choices bfe xs 
  //   where lhs <cat> rhs = lhs <|> wrap rhs
  def bfe[T]: Tree[T] => Levels[T] = {
    case Tip          => Levels(Nil)
    case Node(x, xs)  => Levels.cat(Levels.pure(x), choices(xs)(bfe))
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
}