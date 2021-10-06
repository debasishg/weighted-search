package algebra

import mset._
import MSet.Multiset

// newtype LevelsT m a = LevelsT {runLevelsT :: m (Maybe (Bag a, LevelsT m a)) }
final case class LevelsT[F[_], A](value: F[Option[(Multiset[A], LevelsT[F, A])]])
