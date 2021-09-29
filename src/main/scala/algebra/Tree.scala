package algebra

sealed trait Tree[+T]
case object Tip                                     extends Tree[Nothing]
case class Node[+T](value: T, nodes: List[Tree[T]]) extends Tree[T]
