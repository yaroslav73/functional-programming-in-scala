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

  def noop[S]: ST[S, Unit] = ST[S, Unit](())

  def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = {
    for {
      pivotValue <- a.read(pivot)
      _ <- a.swap(pivot, r)
      j <- STRef(l)
      _ <- (l until r).foldLeft(noop[S])((s, i) =>
        for {
          _ <- s
          iValue <- a.read(i)
          _ <-
            if (iValue < pivotValue) for {
              jValue <- j.read
              _ <- a.swap(i, jValue)
              _ <- j.write(jValue + 1)
            } yield ()
            else noop[S]
        } yield ()
      )
      x <- j.read
      _ <- a.swap(x, r)
    } yield x
  }

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] =
    if (l < r) for {
      pi <- partition(a, l, r, l + (r - l) / 2)
      _ <- qs(a, l, pi - 1)
      _ <- qs(a, pi + 1, r)
    } yield ()
    else noop

  def pureQuicksort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) xs
    else {
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] =
          for {
            arr <- STArray.fromList(xs)
            size <- arr.size
            _ <- qs(arr, 0, size - 1)
            sorted <- arr.freeze
          } yield sorted
      })
    }
  }

  println(pureQuicksort(List(7, 2, 5, 3, 1, 4, 8, 6, 10, 9)))
}
