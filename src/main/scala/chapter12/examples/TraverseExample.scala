package chapter12.examples

import chapter12.Applicative._
import chapter12.Traverse._

object TraverseExample extends App {
  val optTraverse = optionTraverse.traverse(Option.empty[Int])(a => List(a))
  val listTraverse = listTravers.traverse(List.empty[Int])(a => Option(a))

  println(optTraverse)
  println(listTraverse)
}
