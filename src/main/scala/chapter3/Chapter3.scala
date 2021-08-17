package chapter3

import cats.instances.all._
import cats.syntax.all._

object Chapter3 extends App {

  // testing the code in the book
  val func1: Int => Double = (x: Int) => x.toDouble
  val func2: Double => String = (x: Double) => x.show
  val func3: Double => String = (x: Double) => x.toString
  val func4: Double => Double = (x: Double) => x * 2.0

  val mapped1 = func1.map[String](func2)
  val mapped2 = func1.map(func3)
  val mapped3 = func1.map(func4)
  val mapped4 = func1.map[Double](func4)

  println(s"I D: ${mapped1(5)}")
  println(s"D S: ${mapped2(9)}")
  println(s"D S: ${mapped3(10)}")

  // testing my tree functor
  val mapF = (s: String) => s.length

  import BranchingFunctor._

  val l1 = Tree.leaf[String]("einde")
  val l2 = Tree.leaf[String]("fine")
  val b1 = Tree.branch(l1, l2)
  println(s"Branch leaf: ${b1}") // initial tree

  val b2 = b1.map(mapF)
  println(s"Branch map : ${b2}") // mapped tree

  // test with a wider tree
  val b3 = Tree.branch(Tree.branch(l1, l2), l1)
  println(s"Branch tree : ${b3.map(mapF)}")

}
