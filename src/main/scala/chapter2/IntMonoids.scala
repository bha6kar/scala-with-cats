package chapter2

object IntMonoids {
  implicit val intAddMonoid: MyMonoid[Int] = {
    new MyMonoid[Int] {
      override def empty = 0

      override def combine(a: Int, b: Int) = a + b
    }
  }

}
