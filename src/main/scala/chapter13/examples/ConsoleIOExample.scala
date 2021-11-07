package chapter13.examples

object ConsoleIOExample extends App {

//  This is not working, because Console does not have flatMap and map
//  val f1: Free[Console, Option[String]] = for {
//    _ <- Translate.consoleToFunction0(printLn("I can only interact with the console."))
//    line <- Translate.consoleToFunction0(readLn)
//  } yield line
}
