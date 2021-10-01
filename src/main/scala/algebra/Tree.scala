package algebra

// One way to represent a nondeterministic search space of solutions is with
// a tree given by the Tree datatype, where Tip indicates that there are no solutions,
// and each node of the tree x & xs contains a solution x, together with branches xs that
// lead to further possible solutions.
// The two most common ways of enumerating the solutions in such a search space are using
// depthfirst and breadth-first traversals.
// For an initial attempt at such traversals, see @TreeEnumerationsAlgebra
sealed trait Tree[+T]
case object Tip                                     extends Tree[Nothing]
case class Node[+T](value: T, nodes: List[Tree[T]]) extends Tree[T]
