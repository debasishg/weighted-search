package algebra

import cats.syntax.all._
import mset._
import MSet.Multiset
import spire.math.Natural

object LevelsAsPolynomials extends App {
  /**
    * Representing polynomials with Levels
    * It is possible to represent a polynomials of a single variable (in normalised form) with a list of 
    * coeﬃcients, where the 𝑖th entry in the list is the coeﬃcient of the term x^i.
    * 
    * (2x^2 + 1)(x^2 + 2) = 2x^4 + 4x^2 + x^2 + 2 = 2x^0 + 0x^1 + 5x^2 + 0x^3 + 2x^4 = [2, 0, 5, 0, 2]
    * 
    * Addition: (corresponds to <+>)
    * (2x^2 + 1) + (x^3 + 2) = Levels [1, 0, 2] <+> Levels [2, 0, 0, 1] = Levels [3, 0, 2, 1] = 3 + 2x^2 + x^3
    * 
    * Multiplication: (Multiplication corresponds to the monadic sequencing operator >>)
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
