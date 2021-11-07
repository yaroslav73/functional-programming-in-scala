package chapter13.examples

import chapter13._04_free_monad.Free
import chapter13._05_console_io.Console.printLn

object ConsoleIOExample extends App {

//  This is not working, because Console does not have flatMap and map
//  val f1: Free[Console, Option[String]] = for {
//    _ <- Translate.consoleToFunction0(printLn("I can only interact with the console."))
//    line <- Translate.consoleToFunction0(readLn)
//  } yield line

  Free.runConsole(printLn("I can only interact with the console."))
}
