package lectures.collections

import scala.annotation.tailrec

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {

    val m = data.length/2
    if (m == 0) data
    else {
      @tailrec
      def merge(left: Seq[Int], right: Seq[Int], acc: Seq[Int] = List()):  Seq[Int] = (left, right) match {
        case (_, Nil) => acc ++ left
        case (Nil, _) => acc ++ right
        case (l::l_tail, r::r_tail) => {
          if (l > r) {
            merge(l_tail, right, acc :+ l)
          } else {
            merge(left, r_tail, acc :+ r)
          }
        }
      }
      val (left, right) = data splitAt m
      merge(mergeSort(left), mergeSort(right))
    }
  }
  println(mergeSort(List(4, 2, 1, 3, 8, 10)))

}
