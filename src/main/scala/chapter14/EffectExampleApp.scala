package chapter14

object EffectExampleApp extends App {
  val p = new RunnableST[(Int, Int)] {
    override def apply[S]: ST[S, (Int, Int)] =
      for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(x + 1)
        _ <- r2.write(y + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
  }

  println(ST.runST(p))

//  println(ST.runST())
}
