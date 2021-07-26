package chapter2

import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.all._

object Chapter2Exercise extends App {

  // test superadder using simple fold
  def add(items: List[Int]): Int =
    items.foldLeft(0)(_ + _)
  println(s"Add: ${add(List(1, 2, 3, 4))}")

  // test superadder using own monoid
  def addMonoid(items: List[Int]): Int =
    items.foldLeft(Monoid[Int].empty)(_ |+| _)
  println(s"addMonoid: ${addMonoid(List(2, 3, 4))}")

  def addAll[A](values: List[A])(implicit monoid: Monoid[A]): A =
    values.foldRight(monoid.empty)(_ |+| _)

  def addAllContextBound[A: Monoid](values: List[A]): A =
    values.foldLeft(Monoid[A].empty)(_ |+| _)
  println(s"addAllContextBound: ${addAllContextBound(List(Some(2), Some(3), None))}")

  // test superadder using cats' monoid
  def addCats(items: List[Int]): Int = {
    items.fold(Monoid[Int].empty)((n1, n2) => n1 |+| n2)
  }
  println(s"addCats: ${addCats(List(1, 2, 3, 4))}")

  // test superadder for List[Option[Int]]
  def addOptionCats(items: List[Option[Int]]): Option[Int] = {
    val moi = Monoid[Option[Int]]
    items.fold(moi.empty)((o1, o2) => moi.combine(o1, o2))
  }
  println(s"addOptionCats: ${addOptionCats(List(Some(1), None, Some(2), None))}")

  def addOptionCatSyntax(items: List[Option[Int]]): Option[Int] = {
    import cats.instances.all._
    import cats.syntax.all._
    val moi = Monoid[Option[Int]]
    items.fold(moi.empty)((o1, o2) => o1 |+| o2)
  }
  println(s"addOptionCatSyntax: ${addOptionCatSyntax(List(Some(9), None, Some(2), None))}")

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {

    override def empty = Order(0.0, 0.0)

    override def combine(x: Order, y: Order) = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }

  // test superadder unified as per book's solution
  def addUnified[A](items: List[A])(implicit monoid: cats.Monoid[A]) = {
    items.fold[A](monoid.empty)(_ |+| _)
  }
  println(s"addUnified: ${addUnified(List(Some(1), None, Some(2), None))}")

  def addUnifiedContextBound[A: cats.Monoid](items: List[A]) = {
    items.fold(cats.Monoid[A].empty)(_ |+| _)
  }
  println(s"addUnifiedContextBound: ${addUnifiedContextBound(List(Some(1), None, Some(2), None))}")

  // add order
  case class Order(totalCost: Double, quantity: Double)
  val order1 = Order(2.0, 2.0)
  val order2 = Order(1.0, 1.0)
  val orderList = List(order1, order2)

  println(s"addUnified order: ${addUnified(orderList)}")
  println(s"addUnifiedContextBound order: ${addUnifiedContextBound(orderList)}")

  // testing boolean monoids
  println(s"and: ${BooleanMonoids(BooleanMonoids.and).combine(true, true)}") // true
  println(s"or: ${BooleanMonoids(BooleanMonoids.or).combine(false, true)}") // true
  println(s"xor: ${BooleanMonoids(BooleanMonoids.xor).combine(true, true)}") // false
  println(s"xnor(f,t): ${BooleanMonoids(BooleanMonoids.xnor).combine(false, true)}") // false
  println(s"xnor(t,t): ${BooleanMonoids(BooleanMonoids.xnor).combine(true, true)}") // true
}
