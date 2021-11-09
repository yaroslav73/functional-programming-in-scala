package chapter14

object QuickSortExampleApp extends App {
  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else {
      val array = xs.toArray

      def swap(x: Int, y: Int): Unit = {
        val tmp = array(x)
        array(x) = array(y)
        array(y) = tmp
      }

      def partition(l: Int, r: Int, pivot: Int): Int = {
        val pivotValue = array(pivot)
        swap(pivot, r)
        var j = l
        for (i <- l until r) if (array(i) < pivotValue) {
          swap(i, j)
          j += 1
        }
        swap(j, r)
        j
      }

      def qs(l: Int, r: Int): Unit =
        if (l < r) {
          val pi = partition(l, r, l + (r - l) / 2)
          qs(l, pi - 1)
          qs(pi + 1, r)
        }
      qs(0, array.length - 1)
      array.toList
    }

  println(quicksort(List(7, 2, 5, 3, 1, 4, 8, 6, 10, 9)))
}
