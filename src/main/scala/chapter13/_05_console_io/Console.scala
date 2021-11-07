package chapter13._05_console_io

import chapter07.Nonblocking.Par
import chapter13._04_free_monad.{Free, Return, Suspend}

import scala.io.StdIn

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def toPar: Par[Option[String]] = Par.lazyUnit(run)

  def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try {
      Some(StdIn.readLine())
    } catch {
      case _: Exception => None
    }
}

final case class PrintLine(line: String) extends Console[Unit] {
  def toPar: Par[Unit] = Par.lazyUnit(println(line))

  def toThunk: () => Unit = () => println(line)
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
}
