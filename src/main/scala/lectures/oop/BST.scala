package lectures.oop

import scala.annotation.tailrec
import scala.math.pow

/**
  * BSTImpl - это бинарное дерево поиска, содержащее только значения типа Int
  *
  * * Оно обладает следующими свойствами:
  * * * * * левое поддерево содержит значения, меньшие значения родителя
  * * * * * правое поддерево содержит значения, большие значения родителя
  * * * * * значения, уже присутствующие в дереве, в него не добавляются
  * * * * * пустые значения (null) не допускаются
  *
  * * Завершите реализацию методов кейс класс BSTImpl:
  * * * * * Трейт BST и BSTImpl разрешается расширять любым образом
  * * * * * Изменять сигнатуры классов и методов, данные в условии, нельзя
  * * * * * Постарайтесь не использовать var и мутабильные коллекции
  * * * * * В задаче про распечатку дерева, нужно раскомментировать и реализовать метод toString()
  *
  * * Для этой структуры нужно реализовать генератор узлов.
  * * Генератор:
  * * * * * должен создавать дерево, содержащее nodesCount узлов.
  * * * * * не должен использовать переменные или мутабильные структуры.
  *
  */
trait BST {
  val value: Int
  val left: Option[BST]
  val right: Option[BST]

  def add(newValue: Int): BST

  def find(value: Int): Option[BST]
}

case class BSTImpl(value: Int,
                   left: Option[BSTImpl] = None,
                   right: Option[BSTImpl] = None) extends BST {

  def add(newValue: Int): BST = {
    if (newValue == null) return null

    def addImpl(value: Int, node: Option[BSTImpl]): BSTImpl = {
      if (newValue < node.get.value)
        if (node.get.left == None) node.get.copy(left = Option(BSTImpl(newValue)))
        else node.get.copy(left = Option(addImpl(newValue, node.get.left)))
      else if (newValue > node.get.value)
            if (node.get.right == None) node.get.copy(right = Option(BSTImpl(newValue)))
            else node.get.copy(right = Option(addImpl(newValue, node.get.right)))
      else this
    }
    addImpl(newValue, Option(this))
  }

  def find(value: Int): Option[BST] = {
    if (value == null) return null
    @tailrec
    def findImpl(value: Int, node: Option[BSTImpl]): Option[BSTImpl] = {
      if (node.isEmpty) None
      else if(node.get.value == value) node
      else if(node.get.value > value) findImpl(value, node.get.left)
      else findImpl(value, node.get.right)
    }
    findImpl(value, Option(this))
  }


  override def toString() = {
    def getChildren(node:Option[BSTImpl]): List[Option[BSTImpl]] = {
      if (node.isEmpty) List(None, None)
      else List(node.get.left, node.get.right)
    }
    def getNextLevel(nodes:List[Option[BSTImpl]]): List[Option[BSTImpl]] ={
      if (!nodes.exists(_.nonEmpty)) List()
      else nodes.flatMap(getChildren)
    }
    def foldLeftOp(n:Int): (String, Option[BSTImpl]) => String = {
      val drawback:Int = (math.pow(2,n) - 1).toInt
      def foldLeft(acc: String, node: Option[BSTImpl]): String = {
        node match {
          case None => acc + " " * drawback + "-1"
          case _ => acc + " " * drawback + node.get.value
        }
      }
      foldLeft
    }

    def toStringImpl(nodes: List[Option[BSTImpl]]): (String, Int) = {
      if (nodes.isEmpty) ("", 0)
      else {
        val (str, level) = toStringImpl(getNextLevel(nodes))
        (nodes.foldLeft("")(foldLeftOp(level + 1)) + "\n" + str, level + 1)
      }
    }
    toStringImpl(List(Option(this)))_1
  }
}
/* result looks like:
                               5500
               1               7644
       -1       2988       -1       -1
   -1   -1   114   -1   -1   -1   -1   -1
 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
 */




object TreeTest extends App {

  val sc = new java.util.Scanner(System.in)
  val maxValue = 11000
  val nodesCount = sc.nextInt()

  val markerItem = (Math.random() * maxValue).toInt
  val markerItem2 = (Math.random() * maxValue).toInt
  val markerItem3 = (Math.random() * maxValue).toInt

  // Generate huge tree
  val root: BST = BSTImpl(maxValue / 2)
  val tree: BST = (1 to nodesCount).foldLeft(root)((acc, i) => acc.add(i))

  // add marker items
  val testTree = tree.add(markerItem).add(markerItem2).add(markerItem3)
  println(testTree)
  // check that search is correct
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)
  require(testTree.find(markerItem).isDefined)

  println(testTree)
}