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

  override def find(value: T): Option[GeneralBSTImpl[T]] = {
    if (value == null) return null
    findImpl(value, Option(this))
  }

  override def add(newValue: T): GeneralBSTImpl[T] = {
    if (newValue == null) return null
    addImpl(newValue, Option(this))
  }

  def addImpl(newValue: T, node: Option[GeneralBSTImpl[T]]): GeneralBSTImpl[T] = {
    if (newValue < node.get.value)
      if (node.get.left == None) node.get.copy(left = Option(GeneralBSTImpl(newValue)))
      else node.get.copy(left = Option(addImpl(newValue, node.get.left)))
    else if (newValue > node.get.value)
      if (node.get.right == None) node.get.copy(right = Option(GeneralBSTImpl(newValue)))
      else node.get.copy(right = Option(addImpl(newValue, node.get.right)))
    else this
  }

  def findImpl(value: T, node: Option[GeneralBSTImpl[T]]): Option[GeneralBSTImpl[T]] = {
    if (node.isEmpty) None
    else if(node.get.value == value) node
    else if(node.get.value > value) findImpl(value, node.get.left)
    else findImpl(value, node.get.right)
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