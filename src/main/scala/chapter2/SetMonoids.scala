package chapter2

object SetSemigroups {
  def setIntersectionMonoid[A]: Semigroup[Set[A]] = { (a: Set[A], b: Set[A]) =>
    a intersect b
  }
}

object SetMonoids {
  def unionMonoid[A]: MyMonoid[Set[A]] = {
    new MyMonoid[Set[A]] {
      override def combine(a: Set[A], b: Set[A]) = a union b

      override def empty = Set.empty[A]
    }
  }

  def symDiffMonoid[A]: MyMonoid[Set[A]] =
    new MyMonoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] =
        (a diff b) union (b diff a)
      def empty: Set[A] = Set.empty
    }
}
