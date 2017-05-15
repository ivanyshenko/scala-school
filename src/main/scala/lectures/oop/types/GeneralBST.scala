package lectures.oop.types

import lectures.matching.SortingStuff.Watches
import scala.util.Random


/**
  * Модифицируйте реализацию BSTImpl из предыдущего задания.
  * Используя тайп параметры и паттерн Type Class, реализуйте GeneralBSTImpl таким образом,
  * чтобы дерево могло работать с произвольным типом данных.
  *
  * Наследников GeneralBSTImpl определять нельзя.
  *
  * Создайте генератор для деревьев 3-х типов данных:
  * * * * float
  * * * * String
  * * * * Watches из задачи SortStuff. Большими считаются часы с большей стоимостью
  */

trait GeneralBST[T] {
  val value: T
  val left: Option[GeneralBST[T]]
  val right: Option[GeneralBST[T]]

  def add(newValue: T): GeneralBST[T]

  def find(value: T): Option[GeneralBST[T]]
}

case class GeneralBSTImpl[T: Ordering](value: T,
                                       left: Option[GeneralBSTImpl[T]] = None,
                                       right: Option[GeneralBSTImpl[T]] = None) extends GeneralBST[T] {

  val ord: Ordering[T] = implicitly[Ordering[T]]
  import ord._

  override def find(value: T): Option[GeneralBSTImpl[T]] = Option(value).flatMap(v => findImpl(v, Option(this)))

  override def add(newValue: T): GeneralBSTImpl[T] = Option(newValue).map(v => addImpl(v, this)).get

  def addImpl(newValue: T, node: GeneralBSTImpl[T]): GeneralBSTImpl[T] = {
    if (newValue < node.value)
      if (node.left.isEmpty) node.copy(left = Option(GeneralBSTImpl(newValue)))
      else node.copy(left = Option(addImpl(newValue, node.left.get)))
    else if (newValue > node.value)
      if (node.right.isEmpty) node.copy(right = Option(GeneralBSTImpl(newValue)))
      else node.copy(right = Option(addImpl(newValue, node.right.get)))
    else this
  }

  def findImpl(value: T, node: Option[GeneralBSTImpl[T]]): Option[GeneralBSTImpl[T]] = {
    node match {
      case None => None
      case Some(n) if n.value == value => node
      case Some(n) if n.value > value => findImpl(value, node.get.left)
      case Some(n) if n.value < value => findImpl(value, node.get.right)
    }
  }
}

object GeneralTreeTest extends App {

  implicit val watchOrd: Ordering[Watches] = Ordering.by(_.cost)

  val sc = new java.util.Scanner(System.in)
  val maxValue = 110000
  val nodesCount = sc.nextInt()

  val floatTree: GeneralBSTImpl[Float] = (1 until nodesCount).foldLeft(GeneralBSTImpl[Float](maxValue / 2))((node, _) => node.add((Math.random() * maxValue).toFloat))
  val stringTree: GeneralBST[String] = (1 until nodesCount).foldLeft(GeneralBSTImpl[String]("aaaa"))((node, _) => node.add(Random.alphanumeric.take(10).mkString))
  val watchTree: GeneralBST[Watches] = (1 until nodesCount).foldLeft(GeneralBSTImpl[Watches](Watches("bbbb", maxValue / 2)))((node, _) => node.add(Watches(Random.alphanumeric.take(10).mkString, (Math.random() * maxValue).toFloat)))

}