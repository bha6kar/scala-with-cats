package chapter2

trait Semigroup[A] {
  def combine(a: A, b: A): A
}

trait MyMonoid[A] extends Semigroup[A] {
  def empty: A
}

object BooleanMonoids {
  def apply[A](implicit monoid: MyMonoid[A]) = monoid

  implicit val and: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a: Boolean, b: Boolean) = a && b

      override def empty = true
    }

  implicit val or: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a: Boolean, b: Boolean) = a || b

      override def empty = false
    }

  implicit val xor: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)

      override def empty = false
    }

  implicit val xnor: MyMonoid[Boolean] =
    new MyMonoid[Boolean] {
      override def combine(a: Boolean, b: Boolean) = (!a || b) && (a || !b)

      override def empty = true
    }

}
