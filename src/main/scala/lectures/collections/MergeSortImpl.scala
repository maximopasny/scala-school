package lectures.collections

import scala.util.control.TailCalls.TailRec

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {


  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val middle = data.length / 2
    if (middle == 0) data
    else {
      def tailRecMergeSort(leftpart: Seq[Int], rightpart: Seq[Int], acc: Seq[Int]): Seq[Int]
      = (leftpart, rightpart) match {
        case (leftpart, Nil) => acc ++ leftpart
        case (Nil, rightpart) => acc ++ rightpart
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) tailRecMergeSort(leftTail, rightpart, acc :+ leftHead)
          else tailRecMergeSort(leftpart, rightTail, acc :+ rightHead)
      }
      val (l, r) = data splitAt(middle)
      tailRecMergeSort(mergeSort(l), mergeSort(r), Seq())
    }
  }

  println(mergeSort(List(4, 2, 3, 1, 5)))

}
